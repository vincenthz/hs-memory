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
#ifdef WITH_FOUNDATION_SUPPORT
import qualified Foundation as F
import qualified Foundation.Collection as F
import qualified Foundation.String as F (toBytes, Encoding(UTF8))
import qualified Foundation.Array.Internal as F
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

#ifdef WITH_FOUNDATION_SUPPORT
uarrayRecastW8 :: F.PrimType ty => F.UArray ty -> F.UArray F.Word8
uarrayRecastW8 = F.recast

instance F.PrimType ty => ByteArrayAccess (F.UArray ty) where
    length a = let F.CountOf i = F.length (uarrayRecastW8 a) in i
    withByteArray a f = F.withPtr (uarrayRecastW8 a) (f . castPtr)

instance ByteArrayAccess F.String where
    length str = let F.CountOf i = F.length str in i
    withByteArray s f = withByteArray (F.toBytes F.UTF8 s) f

instance (Ord ty, F.PrimType ty) => ByteArray (F.UArray ty) where
    allocRet sz f = do
        mba <- F.new (F.CountOf sz)
        a   <- F.withMutablePtr mba (f . castPtr)
        ba  <- F.unsafeFreeze mba
        return (a, ba)
#endif
