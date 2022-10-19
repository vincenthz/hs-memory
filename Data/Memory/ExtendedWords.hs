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
import Foreign (Storable(..), Ptr, plusPtr, castPtr)

import Data.Memory.Endian (Endianness(..), getSystemEndianness)

-- | A simple Extended Word128 composed of 2 Word64
--
-- Stored most-significant 'Word64' first.
data Word128 = Word128 !Word64 !Word64 deriving (Show, Eq)

instance Storable Word128 where
  sizeOf    _ = 16
  alignment _ = 16

  peek ptr = do
      r1 <- peek ptrR1
      r2 <- peek ptrR2
      return $ Word128 r1 r2
    where
      (ptrR1, ptrR2) = splitPtr ptr

  poke ptr (Word128 r1 r2) = do
      poke ptrR1 r1
      poke ptrR2 r2
    where
      (ptrR1, ptrR2) = splitPtr ptr

-- | Internal auxiliary: pointers to the two halves
--
-- First pointer will point to the most significant 'Word64'.
splitPtr :: Ptr Word128 -> (Ptr Word64, Ptr Word64)
splitPtr ptr =
    case getSystemEndianness of
      BigEndian    -> (castPtr ptr, castPtr ptr `plusPtr` 8)
      LittleEndian -> (castPtr ptr `plusPtr` 8, castPtr ptr)
