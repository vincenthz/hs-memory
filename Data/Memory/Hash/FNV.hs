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

import           Data.Memory.Internal.Compat ()
import           Data.Memory.Internal.CompatPrim
import           Data.Memory.Internal.Imports
import           GHC.Word
import           GHC.Prim
import           GHC.Types
import           GHC.Ptr

newtype FnvHash32 = FnvHash32 Word32
    deriving (Show,Eq,NFData)

newtype FnvHash64 = FnvHash64 Word64
    deriving (Show,Eq,NFData)

fnv1 :: Ptr Word8 -> Int -> IO FnvHash32
fnv1 (Ptr addr) (I# n) = IO $ \s -> loop 0x811c9dc5## 0# s
  where 
        loop :: Word# -> Int# -> State# s -> (# State# s, FnvHash32 #)
        loop !acc i s
            | booleanPrim (i ==# n) = (# s, FnvHash32 $ W32# (narrow32Word# acc) #)
            | otherwise             =
                case readWord8OffAddr# addr i s of
                    (# s2, v #) ->
                        let !nacc = (0x01000193## `timesWord#` acc) `xor#` v
                         in loop nacc (i +# 1#) s2

fnv1a :: Ptr Word8 -> Int -> IO FnvHash32
fnv1a (Ptr addr) (I# n) = IO $ \s -> loop 0x811c9dc5## 0# s
  where 
        loop :: Word# -> Int# -> State# s -> (# State# s, FnvHash32 #)
        loop !acc i s
            | booleanPrim (i ==# n) = (# s, FnvHash32 $ W32# (narrow32Word# acc) #)
            | otherwise             =
                case readWord8OffAddr# addr i s of
                    (# s2, v #) ->
                        let !nacc = 0x01000193## `timesWord#` (acc `xor#` v)
                         in loop nacc (i +# 1#) s2

fnv1_64 :: Ptr Word8 -> Int -> IO FnvHash64
fnv1_64 (Ptr addr) (I# n) = IO $ \s -> loop 0xcbf29ce484222325## 0# s
  where 
        loop :: Word# -> Int# -> State# s -> (# State# s, FnvHash64 #)
        loop !acc i s
            | booleanPrim (i ==# n) = (# s, FnvHash64 $ W64# acc #)
            | otherwise             =
                case readWord8OffAddr# addr i s of
                    (# s2, v #) ->
                        let !nacc = (0x100000001b3## `timesWord#` acc) `xor#` v
                         in loop nacc (i +# 1#) s2

fnv1a_64 :: Ptr Word8 -> Int -> IO FnvHash64
fnv1a_64 (Ptr addr) (I# n) = IO $ \s -> loop 0xcbf29ce484222325## 0# s
  where 
        loop :: Word# -> Int# -> State# s -> (# State# s, FnvHash64 #)
        loop !acc i s
            | booleanPrim (i ==# n) = (# s, FnvHash64 $ W64# acc #)
            | otherwise             =
                case readWord8OffAddr# addr i s of
                    (# s2, v #) ->
                        let !nacc = 0x100000001b3## `timesWord#` (acc `xor#` v)
                         in loop nacc (i +# 1#) s2
