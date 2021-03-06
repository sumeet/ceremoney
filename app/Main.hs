{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- TODO: use https://www.imperialviolet.org/binary/jpeg/
-- syn thing to pattern match the instruction

import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState (get, put), State)
import Data.Array (Ix (inRange, index, range), array, assocs, (!), (//))
import Data.BitSyntax (ReadType (Unsigned), bitSyn)
import Data.Bits (Bits, FiniteBits (finiteBitSize), shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Foldable (find)
import Data.Ratio (Ratio)
import qualified Data.Vector.Storable as SV
import Data.Word (Word16, Word32, Word8)
import Data.Word.Odd (Word4)
import Data.Word12 (Word12)
import Debug.Trace (traceShow, traceShowId)
import Font (charBytes, hexFont)
import GHC.Arr (Array (Array))
import Numeric (showHex)
import SDL (EventPayload (KeyboardEvent), InputMotion (Pressed, Released), KeyboardEventData (KeyboardEventData), Point (P), Renderer, V2 (V2), V4 (V4), WindowConfig, clear, createRenderer, createWindow, defaultRenderer, defaultWindow, destroyWindow, drawPoint, drawPoints, eventPayload, initializeAll, keyboardEventKeyMotion, keyboardEventKeysym, keysymKeycode, pollEvents, present, rendererDrawColor, rendererScale, ticks, windowHighDPI, windowInitialSize, ($=))
import qualified SDL
import SDL.Input.Keyboard.Codes
import System.Environment (getArgs)
import System.Random (StdGen, genWord8, mkStdGen)

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
    l = w `shiftR` 8 .&. 0xff
    r = w .&. 0xff

chop8s :: (Word8, Word8) -> (Word4, Word4, Word4, Word4)
chop8s (l, r) = (ll, lr, rl, rr)
  where
    (ll, lr) = chop8 l
    (rl, rr) = chop8 r

chop8 :: Word8 -> (Word4, Word4)
chop8 b = (fromIntegral l, fromIntegral r)
  where
    l = b `shiftR` 4 .&. 0x0f
    r = b .&. 0x0f

timing :: Word32 -> Computer -> Computer
timing numMsSinceOn comp@Computer {delayTimer, soundTimer, numTicked, prevNumMsSeen} =
  comp
    { delayTimer = fromIntegral $ decr $ fromIntegral delayTimer,
      soundTimer = fromIntegral $ decr $ fromIntegral soundTimer,
      prevNumMsSeen = numMsSinceOn
    }
  where
    decr timer = max (timer - numTicksDiff) 0
    numTicksDiff = numTicksNow - numTicked
    numTicksNow = floor $ (fromIntegral numMsSinceOn :: Ratio Word32) / (1 / 60 * 1000)
    numMsUsed = (fromIntegral numTicked :: Ratio Word32) * ((1 / 60) * 1000)

numBytesPerInstruction :: Num n => n
numBytesPerInstruction = 2

interp :: Computer -> Either String Computer
interp
  comp@Computer
    { memory,
      stack,
      pc,
      registers,
      randGen,
      delayTimer,
      iReg,
      display,
      kb
    } = case inst of
    -- CLS: Clear the display
    (0x0, 0x0, 0xe, 0x0) -> Right $ nextComp {display = clearDisplay}
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
          vx' = registers ! vx + byte
       in Right $ nextComp {registers = registers // [(vx, vx')]}
    -- LD Vx, Vy: Set Vx = Vy
    (0x8, vx, vy, 0x0) ->
      Right $ nextComp {registers = registers // [(vx, registers ! vy)]}
    -- OR Vx, Vy: Set Vx = Vx OR Vy (bitwise OR)
    (0x8, vx, vy, 0x1) ->
      let vx' = registers ! vx .|. registers ! vy
       in Right $ nextComp {registers = registers // [(vx, vx')]}
    -- AND Vx, Vy: Set Vx = Vx OR Vy (bitwise AND)
    (0x8, vx, vy, 0x2) ->
      let vx' = registers ! vx .&. registers ! vy
       in Right $ nextComp {registers = registers // [(vx, vx')]}
    -- XOR Vx, Vy: Set Vx = Vx XOR Vy (bitwise XOR)
    (0x8, vx, vy, 0x3) ->
      let vx' = registers ! vx `xor` registers ! vy
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
    -- screen position (Vx, Vy), set VF = collision
    -- sprites are always 8 across
    (0xd, vx, vy, n) ->
      Right $
        nextComp
          { display = display // displayUpdates,
            registers = registers // [(vf, if collision then 0x1 else 0x0)]
          }
      where
        displayUpdates = zip coords newPx
        (newPx, collision) =
          foldl
            ( \(upd, hasColli) (ex, new) ->
                (upd <> [ex `xor` new], hasColli || ex && new)
            )
            ([], False)
            $ zip existingPx memPx
        xor = (/=)
        memPx =
          concatMap bits $
            take (fromIntegral n) $ map (memory !) [iReg ..]
        existingPx = map (display !) coords
        coords = drawRange display startX startY n
        startX = registers ! vx
        startY = registers ! vy
    -- SKP Vx: Skip next instruction if key with the value of Vx is pressed
    (0xe, vx, 0x9, 0xe) ->
      Right $
        -- TODO: no bounds checking on KB
        -- (the register is 8 bit, but kb is addressed by 4-bits)
        if kb ! fromIntegral (registers ! vx)
          then skipComp
          else nextComp
    -- SKNP Vx: Skip next instruction if key with the value of Vx is not
    -- pressed
    (0xe, vx, 0xa, 0x1) ->
      Right $
        -- TODO: no bounds checking on KB
        -- (the register is 8 bit, but kb is addressed by 4-bits)
        if kb ! fromIntegral (registers ! vx)
          then nextComp
          else skipComp
    -- Fx07 - LD Vx, DT: Set Vx = delay timer value.
    (0xf, vx, 0x0, 0x7) -> Right $ nextComp {registers = registers // [(vx, delayTimer)]}
    -- LD Vx, K: Wait for a key press, store the value of the key in Vx
    (0xf, vx, 0x0, 0xa) ->
      let pressedKey = fst <$> find snd (assocs kb)
       in case pressedKey of
            Just key -> Right $ nextComp {registers = registers'}
              where
                registers' = registers // [(vx, fromIntegral key)]
            -- will repeat this instruction
            Nothing -> Right comp
    -- LD DT, Vx: Set delay timer = Vx
    (0xf, vx, 0x1, 0x5) -> Right $ nextComp {delayTimer = registers ! vx}
    -- LD ST, Vx: Set sound timer = Vx
    (0xf, vx, 0x1, 0x8) -> Right $ nextComp {soundTimer = registers ! vx}
    -- ADD I, Vx: Set I = I + Vx
    (0xf, vx, 0x1, 0xe) ->
      Right $ nextComp {iReg = iReg + fromIntegral (registers ! vx)}
    -- LD F, Vx: Set I = location of sprite for digit Vx
    (0xf, vx, 0x2, 0x9) -> Right $ nextComp {iReg = fontLoc $ registers ! vx}
    -- LD B, Vx: Store BCD representation of Vx in memory locations
    -- I, I+1, and I+2:
    -- Hundreds digit in memory at location in I, the tens digit at location I+1,
    -- and the ones digit at location I+2
    (0xf, vx, 0x3, 0x3) -> Right $ nextComp {memory = memory // updates}
      where
        memoryLocations = take 3 [iReg ..]
        updates = zip memoryLocations (bcd3 $ registers ! vx)
    -- LD [I], Vx: Store registers V0 through Vx in memory starting at location I
    (0xf, vx, 0x5, 0x5) -> Right $ nextComp {memory = memory // updates}
      where
        updates = zip [iReg ..] $ map (registers !) [fromIntegral v0 .. vx]
    -- LD Vx, [I]: Read registers V0 through Vx from memory starting at location I.
    (0xf, vx, 0x6, 0x5) -> Right $ nextComp {registers = registers // updates}
      where
        updates =
          zip [fromIntegral v0 .. vx] $
            map (memory !) [iReg ..]
    --
    -- END OF VALID INSTRUCTIONS
    -- Error: Invalid instruction
    (a, b, c, d) -> Left $ "invalid instruction " <> show (ph a, ph b, ph c, ph d)
    where
      --inst = traceShow (ph a, ph b, ph c, ph d) inst'
      --inst'@(a, b, c, d) = chop8s (memory ! pc, memory ! (pc + 1))
      inst = chop8s (memory ! pc, memory ! (pc + 1))
      nextPc = pc + numBytesPerInstruction
      nextComp = comp {pc = nextPc}
      skipPc = nextPc + numBytesPerInstruction
      skipComp = comp {pc = skipPc}
      vf = 0xf
      v0 = 0x0

ph n = "0x" ++ showHex (fromIntegral n) ""

bits :: Word8 -> [Bool]
bits bs = reverse $ map (testBit bs) [0 .. finiteBitSize bs - 1]

drawRange :: Integral i => Display -> Word8 -> Word8 -> i -> [(Word8, Word8)]
drawRange (Array _ (maxX, maxY) _ _) startX startY n =
  [(x, y) | y <- ys, x <- xs]
  where
    ys = map (`mod` (maxY + 1)) [startY .. startY + fromIntegral n - 1]
    xs = map (`mod` (maxX + 1)) [startX .. startX + 7]

add :: Word8 -> Word8 -> (Word8, Bool)
add x y = (sumL, sumH /= 0x0)
  where
    (sumH, sumL) = chop16 $ fromIntegral x + fromIntegral y

bcd3 :: Word8 -> [Word8]
bcd3 n = [hundreds, tens, ones]
  where
    hundreds = n `div` 100
    tens = (n - hundreds * 100) `div` 10
    ones = n - hundreds * 100 - tens * 10

-- display: 64x32 pixels
displayWidth :: Word8
displayWidth = 64

displayHeight :: Word8
displayHeight = 32

-- (0x00, 0x00), and the bottom-right is assigned (0x3F, 0x1F)
type Display = Array (Word8, Word8) Bool

clearDisplay :: Display
clearDisplay =
  array
    ((0x00, 0x00), (maxX, maxY))
    [((x, y), False) | y <- [0 .. maxY], x <- [0 .. maxX]]
  where
    maxX = displayWidth - 1
    maxY = displayHeight - 1

data Computer = Computer
  { -- usually 4096 addresses from 0x000 to 0xFFF
    memory :: Memory,
    -- 16 registers
    registers :: Registers,
    -- special register called I
    iReg :: Word16,
    pc :: Word16,
    stack :: [Word16],
    delayTimer :: Word8,
    soundTimer :: Word8,
    display :: Display,
    kb :: Keyboard,
    -- numTicked and prevNumMsSeen used for timing
    -- basically prevNumMsSeen at 60Hz... see `timing` for more information
    numTicked :: Word32,
    prevNumMsSeen :: Word32,
    -- random number generator
    randGen :: StdGen
  }

programStart :: Word16
programStart = 0x200

loadProgram :: String -> IO Computer
loadProgram filename = do
  let comp@Computer {memory} = newComputer
  content <- BS.readFile filename
  let memUpdates = zip [programStart ..] $ BS.unpack content
  pure comp {pc = programStart, memory = memory // memUpdates}

-- TODO: take in IO here for the rand generator?
newComputer :: Computer
newComputer =
  Computer
    { memory = newMemory,
      registers = newRegisters,
      iReg = 0x0,
      pc = 0x0,
      stack = [],
      delayTimer = 0x0,
      soundTimer = 0x0,
      display = clearDisplay,
      kb = newKeyboard,
      numTicked = 0,
      prevNumMsSeen = 0,
      randGen = mkStdGen 42069
    }

type Registers = Array Word4 Word8

newRegisters :: Registers
newRegisters = array (0x0, 0xf) $ map (,0x0) [0x0 .. 0xf]

type Memory = Array Word16 Word8

newMemory :: Memory
newMemory = array (0x000, 0xFFF) $ fontRegion <> restRegion
  where
    restRegion = map (,0x0) [fromIntegral $ length fontRegion .. 0xFFF]
    fontRegion = zip [0 ..] $ concat hexFont

fontLoc :: Num b => Word8 -> b
fontLoc hexDigit = fromIntegral $ hexDigit * charBytes hexDigit

type Keyboard = Array Word4 Bool

newKeyboard :: Keyboard
newKeyboard = array (0x0, 0xf) $ map (,False) [0x0 .. 0xf]

black :: V4 Word8
black = V4 0 0 0 0

white :: V4 Word8
white = V4 0xff 0xff 0xff 0xff

nextComp :: Word32 -> Computer -> Computer
nextComp tix computer = case interp $ timing tix computer of
  Left s -> error s
  Right comp -> comp

scale :: Num n => n
scale = 20

windowConfig :: WindowConfig
windowConfig =
  defaultWindow
    { windowHighDPI = True,
      windowInitialSize =
        V2
          (fromIntegral displayWidth * scale)
          (fromIntegral displayHeight * scale)
    }

-- main and apploop from
-- https://hackage.haskell.org/package/sdl2-2.5.3.0/docs/SDL.html
main :: IO ()
main = do
  initializeAll
  args <- getArgs
  comp <- loadProgram $ head args
  window <- createWindow "Ceremoney" windowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  rendererScale renderer $= fromIntegral scale
  appLoop comp renderer
  destroyWindow window

coordToV2 :: (Integral a1, Integral a2, Num a3) => (a1, a2) -> Point V2 a3
coordToV2 (x, y) = P $ V2 (fromIntegral x) (fromIntegral y)

data Event = KeyDown Word4 | KeyUp Word4 | Quit | None

processSDLEvent :: SDL.Event -> Event
processSDLEvent
  SDL.Event
    { eventPayload =
        KeyboardEvent
          KeyboardEventData
            { keyboardEventKeyMotion,
              keyboardEventKeysym
            }
    } =
    case (keyboardEventKeyMotion, keysymKeycode keyboardEventKeysym) of
      (Pressed, KeycodeQ) -> Quit
      (Pressed, Keycode0) -> KeyDown 0x0
      (Pressed, Keycode1) -> KeyDown 0x1
      (Pressed, Keycode2) -> KeyDown 0x2
      (Pressed, Keycode3) -> KeyDown 0x3
      (Pressed, Keycode4) -> KeyDown 0x4
      (Pressed, Keycode5) -> KeyDown 0x5
      (Pressed, Keycode6) -> KeyDown 0x6
      (Pressed, Keycode7) -> KeyDown 0x7
      (Pressed, Keycode8) -> KeyDown 0x8
      (Pressed, Keycode9) -> KeyDown 0x9
      (Pressed, KeycodeA) -> KeyDown 0xa
      (Pressed, KeycodeB) -> KeyDown 0xb
      (Pressed, KeycodeC) -> KeyDown 0xc
      (Pressed, KeycodeD) -> KeyDown 0xd
      (Pressed, KeycodeE) -> KeyDown 0xe
      (Pressed, KeycodeF) -> KeyDown 0xf
      (Released, Keycode0) -> KeyUp 0x0
      (Released, Keycode1) -> KeyUp 0x1
      (Released, Keycode2) -> KeyUp 0x2
      (Released, Keycode3) -> KeyUp 0x3
      (Released, Keycode4) -> KeyUp 0x4
      (Released, Keycode5) -> KeyUp 0x5
      (Released, Keycode6) -> KeyUp 0x6
      (Released, Keycode7) -> KeyUp 0x7
      (Released, Keycode8) -> KeyUp 0x8
      (Released, Keycode9) -> KeyUp 0x9
      (Released, KeycodeA) -> KeyUp 0xa
      (Released, KeycodeB) -> KeyUp 0xb
      (Released, KeycodeC) -> KeyUp 0xc
      (Released, KeycodeD) -> KeyUp 0xd
      (Released, KeycodeE) -> KeyUp 0xe
      (Released, KeycodeF) -> KeyUp 0xf
      _ -> None
processSDLEvent _ = None

appLoop :: Computer -> Renderer -> IO ()
appLoop comp@Computer {display} renderer = do
  events <- pollEvents

  let (comp', quit) =
        foldl
          ( \(comp''@Computer {kb}, quit) event ->
              -- the /= (kb ! key) part is the debounce
              let setKb key onOrOff = comp'' {kb = kb // [(key, onOrOff /= (kb ! key))]}
               in case processSDLEvent event of
                    KeyDown k -> (setKb k True, quit)
                    KeyUp k -> (setKb k False, quit)
                    Quit -> (comp'', True)
                    None -> (comp'', quit)
          )
          (comp, False)
          events

  rendererDrawColor renderer $= black
  clear renderer
  rendererDrawColor renderer $= white
  drawPoints renderer $ SV.fromList $ map (coordToV2 . fst) $ filter snd $ assocs display
  present renderer

  tix <- ticks
  unless quit $ appLoop (nextComp tix comp') renderer

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