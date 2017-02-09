-- |
-- Module      : Data.ByteArray.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Data.ByteArray.Types
    ( ByteArrayAccess(..)
    , ByteArray(..)
    ) where

import           Foreign.Ptr
import           Data.Monoid

#ifdef WITH_BYTESTRING_SUPPORT
import qualified Data.ByteString as B (length)
import qualified Data.ByteString.Internal as B
import           Foreign.ForeignPtr (withForeignPtr)
#endif

-- | Class to Access size properties and data of a ByteArray
class ByteArrayAccess ba where
    -- | Return the length in bytes of a bytearray
    length        :: ba -> Int
    -- | Allow to use using a pointer
    withByteArray :: ba -> (Ptr p -> IO a) -> IO a

-- | Class to allocate new ByteArray of specific size
class (Eq ba, Ord ba, Monoid ba, ByteArrayAccess ba) => ByteArray ba where
    allocRet  :: Int -> (Ptr p -> IO a) -> IO (a, ba)

#ifdef WITH_BYTESTRING_SUPPORT
instance ByteArrayAccess B.ByteString where
    length = B.length
    withByteArray (B.PS fptr off _) f = withForeignPtr fptr $ \ptr -> f $! (ptr `plusPtr` off)

instance ByteArray B.ByteString where
    allocRet sz f = do
        fptr <- B.mallocByteString sz
        r    <- withForeignPtr fptr (f . castPtr)
        return (r, B.PS fptr 0 sz)
#endif

