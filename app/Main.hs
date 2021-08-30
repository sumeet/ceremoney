{-# LANGUAGE BinaryLiterals #-}

module Main where

-- TODO: use https://www.imperialviolet.org/binary/jpeg/
-- syn thing to pattern match the instruction
import Control.Monad.State (State)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word8)
import Data.Word12 (Word12)
import Font (hexFont)

newtype Addr = Addr Word12

newtype Reg = Reg Word16

eval :: Word16 -> State Computer ()
eval [0o0, x, y, z] = undefined

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
