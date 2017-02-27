-- |
-- Module      : Data.Memory.ExtendedWords
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Extra Word size
--
module Data.Memory.ExtendedWords
    ( Word128(..)
    ) where

import Data.Word (Word64)
import Data.Bits
import Data.Ratio ((%))
import GHC.Enum

-- | A simple Extended Word128 composed of 2 Word64
data Word128 = Word128 !Word64 !Word64 deriving (Show, Eq)

applyToParts :: (Word64 -> Word64 -> Word64) -> Word128 -> Word128 -> Word128
applyToParts opp (Word128 w64w1p1 w64w1p2) (Word128 w64w2p1 w64w2p2) =
  Word128 (w64w1p1 `opp` w64w2p1) (w64w1p2 `opp` w64w2p2)

instance Bits Word128 where
  (.&.) = applyToParts (.&.)
  (.|.) = applyToParts (.|.)
  xor = applyToParts xor
  complement (Word128 w64p1 w64p2) = Word128 (complement w64p1) (complement w64p2)
  shiftL (Word128 w64p1 w64p2) shiftCount
    | shiftCount < 0 = minBound
    | shiftCount > 64 = shiftL (Word128 w64p2 0) (shiftCount - 64)
    | otherwise = Word128 mostSignificant leastSignificant
      where leastSignificant = shiftL w64p2 shiftCount
            w64overflow = shiftR w64p2 (64 - shiftCount)
            mostSignificant = shiftL w64p1 shiftCount .|. w64overflow
  shiftR (Word128 w64p1 w64p2) shiftCount
    | shiftCount < 0 = minBound
    | shiftCount > 64 = shiftR (Word128 0 w64p1) (shiftCount - 64)
    | otherwise = Word128 mostSignificant leastSignificant
      where mostSignificant = shiftR w64p1 shiftCount
            w64overflow = shiftL w64p1 (64 - shiftCount)
            leastSignificant = w64overflow .|. shiftR w64p2 shiftCount
  rotateL w128 rotateCount
    | rotateCount < 0 = minBound
    | rotateCount > 128 = rotateL w128 (rotateCount `mod` 128)
    | otherwise = shifted .|. overflow
      where overflow = shiftR w128 (128 - rotateCount)
            shifted = shiftL w128 rotateCount
  rotateR w128 rotateCount
    | rotateCount < 0 = Word128 0 0
    | rotateCount > 128 = rotateR w128 (rotateCount `mod` 128)
    | otherwise = overflow .|. shifted
      where overflow = shiftL w128 (128 - rotateCount)
            shifted = shiftR w128 rotateCount
  bitSize = finiteBitSize
  bitSizeMaybe = Just . finiteBitSize
  isSigned _ = False
  testBit = testBitDefault
  bit = bitDefault
  popCount (Word128 w64p1 w64p2) = popCount w64p1 + popCount w64p2

instance FiniteBits Word128 where
  finiteBitSize _ = 128

instance Bounded Word128 where
  minBound = Word128 minBound minBound
  maxBound = Word128 maxBound maxBound

instance Ord Word128 where
  (<=) (Word128 w64w1p1 w64w1p2) (Word128 w64w2p1 w64w2p2)
    | w64w1p1 < w64w2p1 = True
    | w64w1p1 == w64w2p1 && w64w1p2 <= w64w2p2 = True
    | otherwise = False

instance Num Word128 where
  fromInteger integer = let w64p1 = fromIntegral $ shiftR integer 64 :: Word64
                            w64p2 = fromIntegral integer :: Word64
                        in Word128 w64p1 w64p2
  (+) w1 w2 = fromInteger $ toInteger w1 + toInteger w2
  (*) w1 w2 = fromInteger $ toInteger w1 * toInteger w2
  abs x = x
  signum 0 = 0
  signum _ = 1
  negate x = maxBound - x + 1

instance Real Word128 where
  toRational x = toInteger x % 1

instance Enum Word128 where
  succ x
    | x /= maxBound = x + 1
    | otherwise     = succError "Word128"
  pred x
    | x /= minBound = x - 1
    | otherwise     = predError "Word128"
  toEnum x
    | x >= 0        = fromInteger $ toInteger x
    | otherwise     = toEnumError "Word128" x (minBound::Word128, maxBound::Word128)
  fromEnum x
    | x <= fromIntegral (maxBound::Int)
                    = fromInteger $ toInteger x
    | otherwise     = fromEnumError "Word128" x
  enumFrom x        = enumFromTo x maxBound
  enumFromThen x y  = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise                = minBound

instance Integral Word128 where
  toInteger (Word128 w64p1 w61p2) = shiftL (fromIntegral w64p1) 64 + fromIntegral w61p2
  quot w1 w2 = fromInteger $ toInteger w1 `quot` toInteger w2
  rem w1 w2 = fromInteger $ toInteger w1 `rem` toInteger w2
  quotRem w1 w2 = (w1 `quot` w2, w1 `rem` w2)
  div = quot
  mod = rem