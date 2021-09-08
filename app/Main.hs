{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- TODO: use https://www.imperialviolet.org/binary/jpeg/
-- syn thing to pattern match the instruction

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState (get, put), State)
import Data.Array (Array, Ix (inRange, index, range), (!), (//))
import Data.BitSyntax (ReadType (Unsigned), bitSyn)
import Data.Bits (Bits, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word16, Word8)
import Data.Word.Odd (Word4)
import Data.Word12 (Word12)
import Font (hexFont)
import System.Random (StdGen, genWord8)

-- from https://hackage.haskell.org/package/base-4.13.0.0/docs/src/GHC.Word.html
instance Ix Word4 where
  range (m, n) = [m .. n]
  index (m, _) i = fromIntegral (i - m)
  inRange (m, n) i = m <= i && i <= n

newtype Addr = Addr Word12

newtype Reg = Reg Word16

type RawInst = (Word4, Word4, Word4, Word4)

joinNibbles :: (Integral i, Bits i) => [Word4] -> i
joinNibbles ns = sum $ zipWith (*) (map fromIntegral $ reverse ns) nibbleMasks
  where
    nibbleMasks = iterate (`shiftL` 4) 1

chop16 :: Word16 -> (Word8, Word8)
chop16 w = (fromIntegral l, fromIntegral r)
  where
    l = (w `shiftR` 8) .&. 0xff
    r = w .&. 0xff

chop8s :: (Word8, Word8) -> (Word4, Word4, Word4, Word4)
chop8s (l, r) = (ll, lr, rl, rr)
  where
    (ll, lr) = chop8 l
    (rl, rr) = chop8 r

chop8 :: Word8 -> (Word4, Word4)
chop8 b = (fromIntegral l, fromIntegral r)
  where
    l = (b `shiftR` 4) .&. 0x0f
    r = b .&. 0x0f

interp :: Computer -> Either String Computer
interp comp@Computer {memory, stack, pc, registers, randGen, delayTimer, iReg} = case inst of
  -- CLS: Clear the display (TODO: implement)
  (0x0, 0x0, 0xe, 0x0) -> undefined
  -- RET: Return from a subroutine
  (0x0, 0x0, 0xe, 0xe) -> do
    (newStack, retAddr) <- maybeToEither "stack underflow" $ unsnoc stack
    return comp {stack = newStack, pc = retAddr}
  -- SYS addr: Jump to a machine code routine at addr
  (0x0, nl, nm, nr) -> Right $ comp {pc = joinNibbles [nl, nm, nr]}
  -- JP addr: Jump to addr
  (0x1, nl, nm, nr) -> Right $ comp {pc = joinNibbles [nl, nm, nr]}
  -- CALL addr: Call subroutine at addr
  (0x2, nl, nm, nr) ->
    Right $
      comp {stack = stack ++ [nextPc], pc = joinNibbles [nl, nm, nr]}
  -- SE Vx, byte: Skip next instruction if Vx == byte
  (0x3, vx, bl, bh) ->
    let byte = joinNibbles [bl, bh]
     in Right $ if registers ! vx == byte then skipComp else nextComp
  -- SNE Vx, byte: Skip next instruction if Vx != byte
  (0x4, vx, bl, bh) ->
    let byte = joinNibbles [bl, bh]
     in Right $ if registers ! vx /= byte then skipComp else nextComp
  -- SE Vx, Vy: Skip next instruction if Vx == Vy
  (0x5, vx, vy, 0x0) ->
    Right $ if registers ! vx /= registers ! vy then skipComp else nextComp
  -- LD Vx, byte: Set Vx = byte
  (0x6, vx, bl, bh) ->
    let byte = joinNibbles [bl, bh]
     in Right $ nextComp {registers = registers // [(vx, byte)]}
  -- ADD Vx, byte: Set Vx = Vx + byte
  (0x7, vx, bl, bh) ->
    let byte = joinNibbles [bl, bh]
        vx' = (registers ! vx) + byte
     in Right $ nextComp {registers = registers // [(vx, vx')]}
  -- LD Vx, Vy: Set Vx = Vy
  (0x8, vx, vy, 0x0) ->
    Right $ nextComp {registers = registers // [(vx, registers ! vy)]}
  -- OR Vx, Vy: Set Vx = Vx OR Vy (bitwise OR)
  (0x8, vx, vy, 0x1) ->
    let vx' = (registers ! vx) .|. (registers ! vy)
     in Right $ nextComp {registers = registers // [(vx, vx')]}
  -- AND Vx, Vy: Set Vx = Vx OR Vy (bitwise AND)
  (0x8, vx, vy, 0x2) ->
    let vx' = (registers ! vx) .&. (registers ! vy)
     in Right $ nextComp {registers = registers // [(vx, vx')]}
  -- XOR Vx, Vy: Set Vx = Vx XOR Vy (bitwise XOR)
  (0x8, vx, vy, 0x3) ->
    let vx' = (registers ! vx) `xor` (registers ! vy)
     in Right $ nextComp {registers = registers // [(vx, vx')]}
  -- ADD Vx, Vy: Set Vx = Vx + Vy, set VF = carry
  (0x8, vx, vy, 0x4) -> Right $ nextComp {registers = registers'}
    where
      registers' = registers // [(vx, sum), (vf, if carry then 0x1 else 0x0)]
      (sum, carry) = add (registers ! vx) (registers ! vy)
  -- SUB Vx, Vy: Set Vx = Vx - Vy, set VF = NOT borrow
  (0x8, vx, vy, 0x5) -> Right $ nextComp {registers = registers'}
    where
      registers' = registers // [(vx, diff), (vf, if valX > valY then 0x1 else 0x0)]
      diff = valX - valY
      (valX, valY) = (registers ! vx, registers ! vy)
  -- SHR Vx: Set Vy SHR 1, set VF = 1 if LSB Vy
  (0x8, vx, vy, 0x6) -> Right $ nextComp {registers = registers'}
    where
      (valX, valY) = (registers ! vx, registers ! vy)
      registers' = registers // [(vx, valY `shiftR` 1), (vf, valY .&. 0b1)]
  -- SUBN Vx, Vy: Set Vx = Vy - Vx, set VF = NOT borrow
  (0x8, vx, vy, 0x7) -> Right $ nextComp {registers = registers'}
    where
      registers' = registers // [(vx, diff), (vf, if valY > valX then 0x1 else 0x0)]
      diff = valY - valX
      (valX, valY) = (registers ! vx, registers ! vy)
  -- SHL Vx: Set Vy SHR 1, set VF = 1 if MSB Vy
  (0x8, vx, vy, 0xe) -> Right $ nextComp {registers = registers'}
    where
      (valX, valY) = (registers ! vx, registers ! vy)
      msbIsSet = valY .&. 0b10000000 /= 0
      registers' = registers // [(vx, valY `shiftL` 1), (vf, if msbIsSet then 1 else 0)]
  -- SNE Vx, Vy: Skip next instruction if Vx != Vy
  (0x9, vx, vy, 0) ->
    Right $ if registers ! vx /= registers ! vy then skipComp else nextComp
  -- LD nnn: Set I = nnn
  (0xa, nl, nm, nr) -> Right $ nextComp {iReg = joinNibbles [nl, nm, nr]}
  -- JP V0, addr: Jump to location nnn + V0
  (0xb, nl, nm, nr) -> Right $ comp {pc = jumpDest}
    where
      jumpDest = joinNibbles [nl, nm, nr] + fromIntegral (registers ! v0)
  -- RND Vx, byte: Set Vx = random byte AND kk
  (0xc, vx, nl, nh) -> Right $ nextComp {randGen = randGen', registers = registers'}
    where
      (randByte, randGen') = genWord8 randGen
      registers' = registers // [(vx, randByte .&. joinNibbles [nl, nh])]
  -- DRW Vx, Vy, nibble: Display n-byte sprite starting at memory location I at
  -- (Vx, Vy), set VF = collision
  -- TODO: unimplemented until we have concept of display
  (0xd, vx, vy, n) -> undefined
  -- SKP Vx: Skip next instruction if key with the value of Vx is pressed
  -- TODO: unimplemented until we have concept of keyboard
  (0xe, vx, 0x9, 0xe) -> undefined
  -- SKNP Vx: Skip next instruction if key with the value of Vx is not
  -- pressed
  -- TODO: unimplemented until we have concept of keyboard
  (0xe, vx, 0xa, 0x1) -> undefined
  -- Fx07 - LD Vx, DT: Set Vx = delay timer value.
  (0xf, vx, 0x0, 0x7) -> Right $ nextComp {registers = registers // [(vx, delayTimer)]}
  -- LD Vx, K: Wait for a key press, store the value of the key in Vx
  -- TODO: unimplemented until we have a concept of keyboard and concept of waiting
  (0xf, vx, 0x0, 0xa) -> undefined
  -- LD DT, Vx: Set delay timer = Vx
  (0xf, vx, 0x1, 0x5) -> Right $ nextComp {delayTimer = registers ! vx}
  -- LD ST, Vx: Set sound timer = Vx
  (0xf, vx, 0x1, 0x8) -> Right $ nextComp {soundTimer = registers ! vx}
  -- ADD I, Vx: Set I = I + Vx
  (0xf, vx, 0x1, 0xe) -> Right $ nextComp {iReg = iReg + fromIntegral (registers ! vx)}
  -- LD F, Vx: Set I = location of sprite for digit Vx
  -- TODO: unimplemented until we load the font into memory
  (0xf, vx, 0x2, 0x9) -> undefined
  -- Fx33 - LD B, Vx: Store BCD representation of Vx in memory locations
  -- I, I+1, and I+2:
  -- Hundreds digit in memory at location in I, the tens digit at location I+1,
  -- and the ones digit at location I+2
  (0xf, vx, 0x3, 0x3) -> Right $ nextComp {memory = memory // updates}
    where
      memoryLocations = take 3 $ iterate (+ 1) iReg
      updates = zip memoryLocations (bcd3 $ registers ! vx)
  --
  -- END OF INSTRUCTIONS
  -- Error: Invalid instruction
  notfound -> Left $ "invalid instruction " <> show notfound
  where
    inst = chop8s (memory ! pc, memory ! pc + 1)
    nextPc = pc + 1
    nextComp = comp {pc = nextPc}
    skipPc = nextPc + 1
    skipComp = comp {pc = skipPc}
    getReg = (registers !)
    vf = 0xf
    v0 = 0x0

add :: Word8 -> Word8 -> (Word8, Bool)
add x y = (sumL, sumH /= 0x0)
  where
    (sumH, sumL) = chop16 $ fromIntegral x + fromIntegral y

bcd3 :: Word8 -> [Word8]
bcd3 n = [hundreds, tens, ones]
  where
    hundreds = n `div` 100
    tens = (n - (hundreds * 100)) `div` 10
    ones = n - (hundreds * 100) - (tens * 10)

data Computer = Computer
  { -- usually 4096 addresses from 0x000 to 0xFFF
    memory :: Array Word16 Word8,
    -- 16 registers
    registers :: Array Word4 Word8,
    -- special register called I
    iReg :: Word16,
    delayTimer :: Word8,
    soundTimer :: Word8,
    pc :: Word16,
    stack :: [Word16],
    -- random number generator
    randGen :: StdGen
  }

main :: IO ()
main = putStrLn "sup"

-- from https://hackage.haskell.org/package/extra-1.7.10/docs/src/Data.List.Extra.html#unsnoc
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = Just (x : a, b)
  where
    Just (a, b) = unsnoc xs

-- from https://hackage.haskell.org/package/MissingH-1.4.3.0/docs/src/Data.Either.Utils.html#maybeToEither
maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither errorval Nothing = throwError errorval
maybeToEither _ (Just normalval) = return normalval