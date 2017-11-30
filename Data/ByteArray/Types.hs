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
import           Data.Proxy
import           Data.Word

#ifdef WITH_BYTESTRING_SUPPORT
import qualified Data.ByteString as B (length)
import qualified Data.ByteString.Internal as B
import           Foreign.ForeignPtr (withForeignPtr)
#endif

import qualified Basement.Types.OffsetSize as F
import qualified Basement.UArray as F
import qualified Basement.String as F (String, toBytes, Encoding(UTF8))
import qualified Basement.PrimType as F (primSizeInBytes)

-- | Class to Access size properties and data of a ByteArray
class ByteArrayAccess ba where
    -- | Return the length in bytes of a bytearray
    length        :: ba -> Int
    -- | Allow to use using a pointer
    withByteArray :: ba -> (Ptr p -> IO a) -> IO a

-- | Class to allocate new ByteArray of specific size
class (Eq ba, Ord ba, Monoid ba, ByteArrayAccess ba) => ByteArray ba where
    -- | allocate `n` bytes and perform the given operation
    allocRet  :: Int
                -- ^ number of bytes to allocate. i.e. might not match the
                -- size of the given type `ba`.
              -> (Ptr p -> IO a)
              -> IO (a, ba)

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

uarrayRecastW8 :: F.PrimType ty => F.UArray ty -> F.UArray Word8
uarrayRecastW8 = F.recast

instance F.PrimType ty => ByteArrayAccess (F.UArray ty) where
    length a = let F.CountOf i = F.length (uarrayRecastW8 a) in i
    withByteArray a f = F.withPtr (uarrayRecastW8 a) (f . castPtr)

instance ByteArrayAccess F.String where
    length str = let F.CountOf i = F.length bytes in i
      where
        -- the Foundation's length return a number of elements not a number of
        -- bytes. For @ByteArrayAccess@, because we are using an @Int@, we
        -- didn't see that we were returning the wrong @CountOf@.
        bytes = F.toBytes F.UTF8 str
    withByteArray s f = withByteArray (F.toBytes F.UTF8 s) f

instance (Ord ty, F.PrimType ty) => ByteArray (F.UArray ty) where
    allocRet sz f = do
        mba <- F.new $ sizeRecastBytes sz Proxy
        a   <- F.withMutablePtr mba (f . castPtr)
        ba  <- F.unsafeFreeze mba
        return (a, ba)
      where
        sizeRecastBytes :: F.PrimType ty => Int -> Proxy ty -> F.CountOf ty
        sizeRecastBytes w p = F.CountOf $
            let (q,r) = w `Prelude.quotRem` szTy
             in q + (if r == 0 then 0 else 1)
          where !(F.CountOf szTy) = F.primSizeInBytes p
        {-# INLINE [1] sizeRecastBytes #-}
