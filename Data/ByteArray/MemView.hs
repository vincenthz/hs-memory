-- |
-- Module      : Data.ByteArray.MemView
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
module Data.ByteArray.MemView
    ( MemView(..)
    ) where

import           Foreign.Ptr
import           Data.ByteArray.Types
import           Data.Memory.Internal.Imports

-- | A simple abstraction to a piece of memory.
--
-- Do beware that garbage collection related to
-- piece of memory could be triggered before this
-- is used.
--
-- Only use with the appropriate handler has been
-- used (e.g. withForeignPtr on ForeignPtr)
--
data MemView = MemView {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !Int

instance ByteArrayAccess MemView where
    length (MemView _ l) = l
    withByteArray (MemView p _) f = f (castPtr p)

