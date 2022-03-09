-- |
-- Module      : Data.Memory.Hash.FNV
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- Fowler Noll Vo Hash (1 and 1a / 32 / 64 bits versions)
-- <http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function>
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE BangPatterns               #-}
module Data.Memory.Hash.FNV
    (
    -- * types
      FnvHash32(..)
    , FnvHash64(..)
    -- * methods
    , fnv1
    , fnv1a
    , fnv1_64
    , fnv1a_64
    ) where

import           Basement.Bits
import           Basement.IntegralConv
import           Data.Memory.Internal.Compat ()
import           Data.Memory.Internal.Imports
import           GHC.Word
import           GHC.Prim hiding (Word64#, Int64#)
import           GHC.Types
import           GHC.Ptr

-- | FNV1(a) hash (32 bit variants)
newtype FnvHash32 = FnvHash32 Word32
    deriving (Show,Eq,Ord,NFData)

-- | FNV1(a) hash (64 bit variants)
newtype FnvHash64 = FnvHash64 Word64
    deriving (Show,Eq,Ord,NFData)

fnv1_32_Mix8 :: Word8 -> FnvHash32 -> FnvHash32
fnv1_32_Mix8 !w (FnvHash32 acc) = FnvHash32 ((0x01000193 * acc) .^. integralUpsize w)
{-# INLINE fnv1_32_Mix8 #-}

fnv1a_32_Mix8 :: Word8 -> FnvHash32 -> FnvHash32
fnv1a_32_Mix8 !w (FnvHash32 acc) = FnvHash32 (0x01000193 * (acc .^. integralUpsize w))
{-# INLINE fnv1a_32_Mix8 #-}

fnv1_64_Mix8 :: Word8 -> FnvHash64 -> FnvHash64
fnv1_64_Mix8 !w (FnvHash64 acc) = FnvHash64 ((0x100000001b3 * acc) .^. integralUpsize w)
{-# INLINE fnv1_64_Mix8 #-}

fnv1a_64_Mix8 :: Word8 -> FnvHash64 -> FnvHash64
fnv1a_64_Mix8 !w (FnvHash64 acc) = FnvHash64 (0x100000001b3 * (acc .^. integralUpsize w))
{-# INLINE fnv1a_64_Mix8 #-}

-- | compute FNV1 (32 bit variant) of a raw piece of memory
fnv1 :: Ptr Word8 -> Int -> IO FnvHash32
fnv1 (Ptr addr) n = loop (FnvHash32 0x811c9dc5) 0
  where 
        loop :: FnvHash32 -> Int -> IO FnvHash32
        loop !acc !i
            | i == n    = pure $ acc
            | otherwise = do
                v <- read8 addr i
                loop (fnv1_32_Mix8 v acc) (i + 1)

-- | compute FNV1a (32 bit variant) of a raw piece of memory
fnv1a :: Ptr Word8 -> Int -> IO FnvHash32
fnv1a (Ptr addr) n = loop (FnvHash32 0x811c9dc5) 0
  where 
        loop :: FnvHash32 -> Int -> IO FnvHash32
        loop !acc !i
            | i == n    = pure $ acc
            | otherwise = do
                v <- read8 addr i
                loop (fnv1a_32_Mix8 v acc) (i + 1)

-- | compute FNV1 (64 bit variant) of a raw piece of memory
fnv1_64 :: Ptr Word8 -> Int -> IO FnvHash64
fnv1_64 (Ptr addr) n = loop (FnvHash64 0xcbf29ce484222325) 0
  where 
        loop :: FnvHash64 -> Int -> IO FnvHash64
        loop !acc !i
            | i == n    = pure $ acc
            | otherwise = do
                v <- read8 addr i
                loop (fnv1_64_Mix8 v acc) (i + 1)

-- | compute FNV1a (64 bit variant) of a raw piece of memory
fnv1a_64 :: Ptr Word8 -> Int -> IO FnvHash64
fnv1a_64 (Ptr addr) n = loop (FnvHash64 0xcbf29ce484222325) 0
  where 
        loop :: FnvHash64 -> Int -> IO FnvHash64
        loop !acc !i
            | i == n    = pure $ acc
            | otherwise = do
                v <- read8 addr i
                loop (fnv1a_64_Mix8 v acc) (i + 1)

read8 :: Addr# -> Int -> IO Word8
read8 addr (I# i) = IO $ \s -> case readWord8OffAddr# addr i s of
    (# s2, e #) -> (# s2, W8# e #)
