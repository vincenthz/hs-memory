-- |
-- Module      : Data.ByteArray.Bytes
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- Simple and efficient byte array types
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.ByteArray.Bytes
    ( Bytes
    ) where

import           GHC.Types
import           GHC.Prim
import           GHC.Ptr
import           Data.Monoid
import           Data.Memory.PtrMethods
import           Data.Memory.Internal.CompatPrim
import           Data.Memory.Internal.Compat      (unsafeDoIO)
import           Data.Memory.Encoding.Base16      (showHexadecimal)
import           Data.ByteArray.Types

-- | Simplest Byte Array
data Bytes = Bytes (MutableByteArray# RealWorld)

instance Show Bytes where
    show = bytesShowHex
instance Eq Bytes where
    (==) = bytesEq
instance Ord Bytes where
    compare = bytesCompare
instance Monoid Bytes where
    mempty        = unsafeDoIO (newBytes 0)
    mappend b1 b2 = unsafeDoIO $ bytesAppend b1 b2
    mconcat       = unsafeDoIO . bytesConcat

instance ByteArrayAccess Bytes where
    length        = bytesLength
    withByteArray = withBytes
instance ByteArray Bytes where
    allocRet = bytesAllocRet

------------------------------------------------------------------------
newBytes :: Int -> IO Bytes
newBytes (I# sz)
    | booleanPrim (sz <# 0#) = error "Bytes: size must be >= 0"
    | otherwise              = IO $ \s ->
        case newAlignedPinnedByteArray# sz 8# s of
            (# s', mbarr #) -> (# s', Bytes mbarr #)

touchBytes :: Bytes -> IO ()
touchBytes (Bytes mba) = IO $ \s -> case touch# mba s of s' -> (# s', () #)

sizeofBytes :: Bytes -> Int
sizeofBytes (Bytes mba) = I# (sizeofMutableByteArray# mba)

withPtr :: Bytes -> (Ptr p -> IO a) -> IO a
withPtr b@(Bytes mba) f = do
    a <- f (Ptr (byteArrayContents# (unsafeCoerce# mba)))
    touchBytes b
    return a
------------------------------------------------------------------------

bytesAlloc :: Int -> (Ptr p -> IO ()) -> IO Bytes
bytesAlloc sz f = do
    ba <- newBytes sz
    withPtr ba f
    return ba

bytesConcat :: [Bytes] -> IO Bytes
bytesConcat l = bytesAlloc retLen (copy l)
  where
    retLen = sum $ map bytesLength l

    copy []     _   = return ()
    copy (x:xs) dst = do
        withPtr x $ \src -> memCopy dst src chunkLen
        copy xs (dst `plusPtr` chunkLen)
      where
        chunkLen = bytesLength x

bytesAppend :: Bytes -> Bytes -> IO Bytes
bytesAppend b1 b2 = bytesAlloc retLen $ \dst -> do
    withPtr b1 $ \s1 -> memCopy dst                  s1 len1
    withPtr b2 $ \s2 -> memCopy (dst `plusPtr` len1) s2 len2
  where
    len1   = bytesLength b1
    len2   = bytesLength b2
    retLen = len1 + len2

bytesAllocRet :: Int -> (Ptr p -> IO a) -> IO (a, Bytes)
bytesAllocRet sz f = do
    ba <- newBytes sz
    r <- withPtr ba f
    return (r, ba)

bytesLength :: Bytes -> Int
bytesLength = sizeofBytes

withBytes :: Bytes -> (Ptr p -> IO a) -> IO a
withBytes = withPtr

bytesEq :: Bytes -> Bytes -> Bool
bytesEq b1@(Bytes m1) b2@(Bytes m2)
    | l1 /= l2  = False
    | otherwise = unsafeDoIO $ IO $ \s -> loop 0# s
  where
    !l1@(I# len) = bytesLength b1
    !l2          = bytesLength b2

    loop i s
        | booleanPrim (i ==# len) = (# s, True #)
        | otherwise               =
            case readWord8Array# m1 i s of
                (# s', e1 #) -> case readWord8Array# m2 i s' of
                    (# s'', e2 #) ->
                        if booleanPrim (eqWord# e1 e2)
                            then loop (i +# 1#) s''
                            else (# s', False #)

bytesCompare :: Bytes -> Bytes -> Ordering
bytesCompare b1@(Bytes m1) b2@(Bytes m2) = unsafeDoIO $ IO $ \s -> loop 0# s
  where
    !l1       = bytesLength b1
    !l2       = bytesLength b2
    !(I# len) = min l1 l2

    loop i s1
        | booleanPrim (i ==# len) =
            if l1 == l2
                then (# s1, EQ #)
                else if l1 > l2 then (# s1, GT #)
                                else (# s1, LT #)
        | otherwise               =
            case readWord8Array# m1 i s1 of
                (# s2, e1 #) -> case readWord8Array# m2 i s2 of
                    (# s3, e2 #) ->
                        if booleanPrim (eqWord# e1 e2)
                            then loop (i +# 1#) s3
                            else if booleanPrim (ltWord# e1 e2) then (# s3, LT #)
                                                                else (# s3, GT #)

bytesShowHex :: Bytes -> String
bytesShowHex b = showHexadecimal (withPtr b) (bytesLength b)
{-# NOINLINE bytesShowHex #-}
