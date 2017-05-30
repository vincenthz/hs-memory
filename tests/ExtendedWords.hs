{-# LANGUAGE ScopedTypeVariables #-}

module ExtendedWords where

import           Imports
import           Utils
import           Data.Bits
import           Data.Memory.ExtendedWords


instance Arbitrary Word128 where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Word128 a1 a2


w128properties = testGroup "Word 128 Properties"
  [ testProperty "complement . complement = id" $ \(w128 :: Word128) -> w128 == (complement . complement $ w128)
  , testProperty "rotateR (rotateL A x) x = id" $
      \(w128 :: Word128, Positive rotateCount) -> w128 == rotateR (rotateL w128 rotateCount) rotateCount
  , testProperty "abs x * signum x == x" $ \(w128 :: Word128) -> w128 == (abs w128 * signum w128)
  , testProperty "(x `quot` y)*y + (x `rem` y) == x" $
      \(Positive (w1 :: Word128), Positive (w2 :: Word128)) -> w1 == ((w1 `quot` w2) * w2 + (w1 `rem` w2))
  , testProperty "(x `div` y)*y + (x `mod` y) == x" $
      \(Positive (w1 :: Word128), Positive (w2 :: Word128)) -> w1 == ((w1 `div` w2) * w2 + (w1 `mod` w2))
  , testProperty "fromInteger . toInteger = id" $ \(w128 :: Word128) -> w128 == (fromInteger . toInteger $ w128)]