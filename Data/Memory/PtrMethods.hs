-- |
-- Module      : Data.Memory.PtrMethods
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- methods to manipulate raw memory representation
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Memory.PtrMethods
    ( bufCreateTemporary
    , bufXor
    , bufXorWith
    , bufCopy
    , bufSet
    ) where

import           Data.Memory.Internal.Imports
import           Foreign.Ptr              (Ptr, plusPtr)
import           Foreign.Storable         (peek, poke, pokeByteOff, peekByteOff)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc    (allocaBytesAligned)
import           Data.Bits                (xor)

-- | Create a new temporary buffer
bufCreateTemporary :: Int -> (Ptr Word8 -> IO a) -> IO a
bufCreateTemporary size f = allocaBytesAligned size 8 f

-- | xor bytes from source1 and source2 to destination
-- 
-- d = s1 xor s2
--
-- s1, nor s2 are modified unless d point to s1 or s2
bufXor :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int -> IO ()
bufXor _ _  _  0 = return ()
bufXor d s1 s2 n = do
    (xor <$> peek s1 <*> peek s2) >>= poke d
    bufXor (d `plusPtr` 1) (s1 `plusPtr` 1) (s2 `plusPtr` 1) (n-1)

-- | xor bytes from source with a specific value to destination
--
-- d = replicate (sizeof s) v `xor` s
bufXorWith :: Ptr Word8 -> Word8 -> Ptr Word8 -> Int -> IO ()
bufXorWith d v s n = loop 0
  where
    loop i
        | i == n    = return ()
        | otherwise = do
            (xor v <$> peekByteOff s i) >>= pokeByteOff d i
            loop (i+1)

bufCopy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
bufCopy dst src n = c_memcpy dst src (fromIntegral n)

-- | Set @n number of bytes to the same value @v
bufSet :: Ptr Word8 -> Word8 -> Int -> IO ()
bufSet start v n = c_memset start (fromIntegral v) (fromIntegral n) >>= \_ -> return ()
    {-loop 0
  where loop i
            | i == n    = return ()
            | otherwise = pokeByteOff start i v >> loop (i+1)
-}

foreign import ccall unsafe "memset"
    c_memset :: Ptr Word8 -> Word8 -> CSize -> IO ()

foreign import ccall unsafe "memcpy"
    c_memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
