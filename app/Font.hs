{-# LANGUAGE BinaryLiterals #-}

module Font where

import Data.Word (Word8)
import Data.Word.Odd (Word4)

charBytes :: Integral i => Word8 -> i
charBytes n = fromIntegral $ length $ hexFont !! fromIntegral n

hexFont :: [[Word8]]
hexFont = [zero, one, two, three, four, five, six, seven, eight, nine, a, b, c, d, e, f]
  where
    zero =
      [ 0b11110000,
        0b10010000,
        0b10010000,
        0b10010000,
        0b11110000
      ]
    one =
      [ 0b00100000,
        0b01100000,
        0b00100000,
        0b10100000,
        0b01110000
      ]
    two =
      [ 0b11110000,
        0b00010000,
        0b11110000,
        0b10000000,
        0b11110000
      ]
    three =
      [ 0b11110000,
        0b00010000,
        0b11110000,
        0b00010000,
        0b11110000
      ]
    four =
      [ 0b10010000,
        0b10010000,
        0b11110000,
        0b00010000,
        0b00010000
      ]
    five =
      [ 0b11110000,
        0b10000000,
        0b11110000,
        0b00010000,
        0b11110000
      ]
    six =
      [ 0b11110000,
        0b10000000,
        0b11110000,
        0b10010000,
        0b11110000
      ]
    seven =
      [ 0b11110000,
        0b00010000,
        0b00100000,
        0b01000000,
        0b01000000
      ]
    eight =
      [ 0b11110000,
        0b10010000,
        0b11110000,
        0b10010000,
        0b11110000
      ]
    nine =
      [ 0b11110000,
        0b10010000,
        0b11110000,
        0b00010000,
        0b11110000
      ]
    a =
      [ 0b11110000,
        0b10010000,
        0b11110000,
        0b10010000,
        0b10010000
      ]
    b =
      [ 0b11100000,
        0b10010000,
        0b11100000,
        0b10010000,
        0b11100000
      ]
    c =
      [ 0b11110000,
        0b10000000,
        0b10000000,
        0b10000000,
        0b11110000
      ]
    d =
      [ 0b11100000,
        0b10010000,
        0b10010000,
        0b10010000,
        0b11100000
      ]
    e =
      [ 0b11110000,
        0b10000000,
        0b11110000,
        0b10000000,
        0b11110000
      ]
    f =
      [ 0b11110000,
        0b10000000,
        0b11110000,
        0b10000000,
        0b10000000
      ]
