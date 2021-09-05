{-# LANGUAGE BinaryLiterals, TemplateHaskell #-}

module Main where

-- TODO: use https://www.imperialviolet.org/binary/jpeg/
-- syn thing to pattern match the instruction
import Data.BitSyntax (bitSyn, ReadType (Unsigned))
import Control.Monad.State (State)
import qualified Data.ByteString as BS
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Word.Odd (Word4)
import Data.Word (Word16, Word8)
import Data.Word12 (Word12)
import Font (hexFont)

newtype Addr = Addr Word12
newtype Reg = Reg Word16
type RawInst = (Word4, Word4, Word4, Word4)

nibbleMasks :: [Int]
nibbleMasks = iterate (`shiftL` 4) 0xf

join4to16LE :: [Word4] -> Word16
join4to16LE ns =
  foldl (\acc (n, mask) -> 123) 0 $ zip (reverse ns) nibbleMasks

chop16 :: (Word8, Word8) -> (Word4, Word4, Word4, Word4)
chop16 (l, r) = (ll, lr, rl, rr)
  where (ll, lr) = chop8 l
        (rl, rr) = chop8 r

chop8 :: Word8 -> (Word4, Word4)
chop8 b = (fromIntegral l, fromIntegral r)
  where l = (b `shiftR` 4) .&. 0x0f
        r = b .&. 0x0f

interp :: RawInst -> State Computer ()
interp (0, nl, nm, nr) = undefined 
  where x = 123
-- TODO: need to map this to an error somehow
interp _ = undefined

-- TODO: not sure if i need this, going to try interping
-- the instructions manually first
-- data Instruction
--   = SYS Addr
--   | CLS
--   | RET
--   | JP Addr
--   | CALL Addr
--   | SEb Reg Word8
--   | SNE Reg Word8
--   | SEr Reg Reg
--   | LDb Reg Word8
--   | ADDb Reg Word8
--   | LDr Reg Reg
--   | OR Reg Reg
--   | AND Reg Reg
--   | XOR Reg Reg
--   | ADDr Reg Reg
--   | SUB Reg Reg

data Computer = Computer
  { -- usually 4096 addresses from 0x000 to 0xFFF
    memory :: BS.ByteString,
    -- 16 registers
    registers :: BS.ByteString,
    iReg :: Word8,
    delayReg :: Word8,
    soundTimingReg :: Word8,
    programCounter :: Word16,
    stackPointer :: Word8,
    stack :: [Word16]
  }

main :: IO ()
main = putStrLn "sup"
