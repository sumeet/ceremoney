{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- TODO: use https://www.imperialviolet.org/binary/jpeg/
-- syn thing to pattern match the instruction

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState (get, put), State)
import Data.Array (Array)
import Data.BitSyntax (ReadType (Unsigned), bitSyn)
import Data.Bits (Bits, shiftL, shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.Word (Word16, Word8)
import Data.Word.Odd (Word4)
import Data.Word12 (Word12)
import Font (hexFont)

newtype Addr = Addr Word12

newtype Reg = Reg Word16

type RawInst = (Word4, Word4, Word4, Word4)

joinNibbles :: (Integral i, Bits i) => [Word4] -> i
joinNibbles ns = sum $ zipWith (*) (map fromIntegral $ reverse ns) nibbleMasks
  where
    nibbleMasks = iterate (`shiftL` 4) 1

chop16 :: (Word8, Word8) -> (Word4, Word4, Word4, Word4)
chop16 (l, r) = (ll, lr, rl, rr)
  where
    (ll, lr) = chop8 l
    (rl, rr) = chop8 r

chop8 :: Word8 -> (Word4, Word4)
chop8 b = (fromIntegral l, fromIntegral r)
  where
    l = (b `shiftR` 4) .&. 0x0f
    r = b .&. 0x0f

interp :: RawInst -> Computer -> Either String Computer
-- CLS: Clear the display (TODO: implement)
interp (0x0, 0x0, 0xe, 0x0) comp = undefined
-- RET: Return from a subroutine
interp (0x0, 0x0, 0xe, 0xe) comp@Computer {stack} = do
  (newStack, retAddr) <- maybeToEither "stack underflow" $ unsnoc stack
  return comp {stack = newStack, pc = retAddr}
-- SYS addr: Jump to a machine code routine at addr
interp (0x0, nl, nm, nr) comp = Right $ comp {pc = joinNibbles [nl, nm, nr]}
-- JP addr: Jump to addr
interp (0x1, nl, nm, nr) comp = Right $ comp {pc = joinNibbles [nl, nm, nr]}
-- CALL addr: Call subroutine at addr
interp (0x2, nl, nm, nr) comp@Computer {stack, pc} =
  Right $ comp {stack = stack ++ [pc], pc = joinNibbles [nl, nm, nr]}
-- SE Vx, byte
-- not found
interp notfound comp = Left $ "invalid instruction " <> show notfound

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
    memory :: Array Word12 Word8,
    -- 16 registers
    registers :: Array Word4 Word8,
    -- special register called I
    iReg :: Word8,
    delayReg :: Word8,
    soundTimingReg :: Word8,
    pc :: Word16,
    stack :: [Word16]
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