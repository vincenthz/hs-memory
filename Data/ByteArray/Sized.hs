-- |
-- Module      : Data.ByteArray.Sized
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : stable
-- Portability : Good
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ByteArray.Sized
    ( ByteArrayN(..)
    , SizedByteArray
    , unSizedByteArray
    , sizedByteArray
    , unsafeSizedByteArray

    , -- * ByteArrayN operators
      alloc
    , create
    , allocAndFreeze
    , unsafeCreate
    , inlineUnsafeCreate
    , empty
    , pack
    , unpack
    , cons
    , snoc
    , xor
    , index
    , splitAt
    , take
    , drop
    , append
    , copy
    , copyRet
    , copyAndFreeze
    , replicate
    , zero
    , convert
    ) where

import Basement.Imports
import Basement.NormalForm
import Basement.Nat
import Basement.Numerical.Additive ((+))
import Basement.Numerical.Subtractive ((-))

import Basement.Sized.List (ListN, unListN, toListN)

import           Foreign.Storable
import           Foreign.Ptr
import           Data.Maybe (fromMaybe)

import           Data.Memory.Internal.Compat
import           Data.Memory.PtrMethods

import Data.Proxy (Proxy(..))

import Data.ByteArray.Types (ByteArrayAccess(..), ByteArray)
import qualified Data.ByteArray.Types as ByteArray (allocRet)

#if MIN_VERSION_basement(0,0,7)
import           Basement.BlockN (BlockN)
import qualified Basement.BlockN as BlockN
import qualified Basement.PrimType as Base
import           Basement.Types.OffsetSize (Countable)
#endif

-- | Type class to emulate exactly the behaviour of 'ByteArray' but with
-- a known length at compile time
--
class ByteArrayAccess ba => ByteArrayN ba where
    -- | length of the given bytearray
    type LengthN ba :: Nat

    -- | just like 'allocRet' but with the size at the type level
    allocRet :: forall n p a
              . (KnownNat n, LengthN ba ~ n)
             => Proxy n
             -> (Ptr p -> IO a)
             -> IO (a, ba)

-- | Wrapper around any collection type with the size as type parameter
--
newtype SizedByteArray (n :: Nat) ba = SizedByteArray { unSizedByteArray :: ba }
  deriving (Eq, Show, Typeable, Ord, NormalForm, Semigroup, Monoid)

-- | create a 'SizedByteArray' from the given 'ByteArrayAccess' if the
-- size is the same as the target size.
--
sizedByteArray :: forall n ba . (KnownNat n, ByteArrayAccess ba)
               => ba
               -> Maybe (SizedByteArray n ba)
sizedByteArray ba
    | length ba == n = Just $ SizedByteArray ba
    | otherwise      = Nothing
  where
    n = fromInteger $ natVal (Proxy @n)

-- | just like the 'sizedByteArray' function but throw an exception if
-- the size is invalid.
unsafeSizedByteArray :: forall n ba . (ByteArrayAccess ba, KnownNat n) => ba -> SizedByteArray n ba
unsafeSizedByteArray ba = case sizedByteArray @n ba of
    Nothing -> error "The size is invalid"
    Just v  -> v

instance (ByteArrayAccess ba, KnownNat n) => ByteArrayAccess (SizedByteArray n ba) where
    length _ = fromInteger $ natVal (Proxy @n)
    withByteArray (SizedByteArray ba) = withByteArray ba

instance (ByteArray ba, KnownNat n) => ByteArrayN (SizedByteArray n ba) where
    type LengthN (SizedByteArray n ba) = n
    allocRet p f = do
        (a, ba) <- ByteArray.allocRet n f
        pure (a, SizedByteArray ba)
      where
        n = fromInteger $ natVal p

#if MIN_VERSION_basement(0,0,7)
instance (ByteArrayAccess (BlockN n Word8), KnownNat n, Countable Word8 n, Base.PrimType Word8) => ByteArrayN (BlockN n Word8) where
    type LengthN (BlockN n Word8) = n
    allocRet _ f = do
        mba <- BlockN.new @n
        a   <- BlockN.withMutablePtrHint True False mba (f . castPtr)
        ba  <- BlockN.freeze mba
        return (a, ba)
#endif


-- | Allocate a new bytearray of specific size, and run the initializer on this memory
alloc :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
      => Proxy n
      -> (Ptr p -> IO ())
      -> IO ba
alloc p f = snd <$> allocRet p f

-- | Allocate a new bytearray of specific size, and run the initializer on this memory
create :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
       => Proxy n
       -> (Ptr p -> IO ())
       -> IO ba
create = alloc
{-# NOINLINE create #-}

-- | similar to 'allocN' but hide the allocation and initializer in a pure context
allocAndFreeze :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
               => Proxy n -> (Ptr p -> IO ()) -> ba
allocAndFreeze p f = unsafeDoIO (alloc p f)
{-# NOINLINE allocAndFreeze #-}

-- | similar to 'createN' but hide the allocation and initializer in a pure context
unsafeCreate :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
             => Proxy n -> (Ptr p -> IO ()) -> ba
unsafeCreate sz f = unsafeDoIO (alloc sz f)
{-# NOINLINE unsafeCreate #-}

inlineUnsafeCreate :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
                   => Proxy n -> (Ptr p -> IO ()) -> ba
inlineUnsafeCreate sz f = unsafeDoIO (alloc sz f)
{-# INLINE inlineUnsafeCreate #-}

-- | Create an empty byte array
empty :: forall ba . (ByteArrayN ba, LengthN ba ~ 0) => Proxy 0 -> ba
empty p = unsafeDoIO (alloc p $ \_ -> return ())

-- | Pack a list of bytes into a bytearray
pack :: forall n ba . (ByteArrayN ba, KnownNat n, LengthN ba ~ n) => ListN n Word8 -> ba
pack l = inlineUnsafeCreate (Proxy @n) (fill $ unListN l)
  where fill []     _  = return ()
        fill (x:xs) !p = poke p x >> fill xs (p `plusPtr` 1)
        {-# INLINE fill #-}
{-# NOINLINE pack #-}

-- | Un-pack a bytearray into a list of bytes
unpack :: forall n ba
        . (ByteArrayAccess ba, KnownNat n, LengthN ba ~ n, NatWithinBound Int n)
       => ba -> ListN n Word8
unpack bs =  fromMaybe (error "the impossible appened") $ toListN @n $ loop 0
  where !len = length bs
        loop i
            | i == len  = []
            | otherwise =
                let !v = unsafeDoIO $ withByteArray bs (`peekByteOff` i)
                 in v : loop (i+1)

-- | prepend a single byte to a byte array
cons :: forall bi bo
      . ( ByteArrayN bi, ByteArrayN bo
        , KnownNat (LengthN bi), KnownNat (LengthN bo)
        , (LengthN bi + 1) ~ LengthN bo
        )
     => Word8 -> bi -> bo
cons b ba = unsafeCreate po $ \d -> withByteArray ba $ \s -> do
    pokeByteOff d 0 b
    memCopy (d `plusPtr` 1) s len
  where
    !pi = Proxy @(LengthN bi)
    !po = Proxy @(LengthN bo)
    !len = fromInteger $ natVal pi

-- | append a single byte to a byte array
snoc :: forall bi bo
      . ( ByteArrayN bi, ByteArrayN bo
        , KnownNat (LengthN bi), KnownNat (LengthN bo)
        , (LengthN bi + 1) ~ LengthN bo
        )
     => bi -> Word8 -> bo
snoc ba b = unsafeCreate po $ \d -> withByteArray ba $ \s -> do
    memCopy d s len
    pokeByteOff d len b
  where
    !pi = Proxy @(LengthN bi)
    !po = Proxy @(LengthN bo)
    !len = fromInteger $ natVal pi

-- | Create a xor of bytes between a and b.
--
-- the returns byte array is the size of the smallest input.
xor :: forall a b c
     . ( ByteArrayN a, ByteArrayN b, ByteArrayN c
       , KnownNat (LengthN a), KnownNat (LengthN b), KnownNat (LengthN c)
       , (LengthN a ~ LengthN c), LengthN b <= LengthN a
       )
    => a -> b -> c
xor a b =
    unsafeCreate (Proxy @(LengthN c)) $ \pc ->
    withByteArray a  $ \pa ->
    withByteArray b  $ \pb ->
        memXor pc pa pb n
  where
        n  = min la lb
        la = length a
        lb = length b

-- | return a specific byte indexed by a number from 0 in a bytearray
--
-- unsafe, no bound checking are done
index :: forall n ba . (ByteArrayN ba, KnownNat n, n <= (LengthN ba)) => ba -> Proxy n -> Word8
index b pi = unsafeDoIO $ withByteArray b $ \p -> peek (p `plusPtr` i)
  where
    i = fromInteger $ natVal pi

-- | Split a bytearray at a specific length in two bytearray
splitAt :: forall n bi blhs brhs
         . ( ByteArrayN bi, ByteArrayN blhs, ByteArrayN brhs
           , KnownNat n, KnownNat (LengthN bi), KnownNat (LengthN blhs), KnownNat (LengthN brhs)
           , n <= (LengthN bi), (LengthN blhs + n) ~ LengthN bi, LengthN blhs ~ n
           )
        => Proxy n -> bi -> (blhs, brhs)
splitAt pn bs = unsafeDoIO $
    withByteArray bs $ \p -> do
        b1 <- alloc pn $ \r -> memCopy r p n
        b2 <- alloc (Proxy @(LengthN brhs)) $ \r -> memCopy r (p `plusPtr` n) (len - n)
        return (b1, b2)
  where
    n = fromInteger $ natVal pn
    len = length bs

-- | Take the first @n@ byte of a bytearray
take :: forall n bi bo
      . ( ByteArrayN bi, ByteArrayN bo
        , KnownNat n, KnownNat (LengthN bi), KnownNat (LengthN bo)
        , LengthN bo ~ n, LengthN bi <= n
        )
     => Proxy n -> bi -> bo
take pn bs = unsafeCreate pn $ \d -> withByteArray bs $ \s -> memCopy d s m
  where
    !m   = min len n
    !len = length bs
    !n   = fromInteger $ natVal pn

-- | drop the first @n@ byte of a bytearray
drop :: forall n bi bo
      . ( ByteArrayN bi, ByteArrayN bo
        , KnownNat n, KnownNat (LengthN bi), KnownNat (LengthN bo)
        , (LengthN bo + n) ~ LengthN bi
        )
     => Proxy n -> bi -> bo
drop pn bs = unsafeCreate (Proxy @(LengthN bo)) $ \d ->
    withByteArray bs $ \s ->
    memCopy d (s `plusPtr` ofs) nb
  where
    ofs = min len n
    nb  = len - ofs
    len = fromInteger $ natVal (Proxy @(LengthN bi))
    n   = fromInteger $ natVal pn

-- | append one bytearray to the other
append :: forall blhs brhs bout
        . ( ByteArrayN blhs, ByteArrayN brhs, ByteArrayN bout
          , KnownNat (LengthN blhs), KnownNat (LengthN brhs), KnownNat (LengthN bout)
          , (LengthN brhs + LengthN blhs) ~ LengthN bout
          )
       => blhs -> brhs -> bout
append blhs brhs = unsafeCreate (Proxy @(LengthN bout)) $ \p ->
    withByteArray blhs $ \plhs ->
    withByteArray brhs $ \prhs -> do
        memCopy p plhs (length blhs)
        memCopy (p `plusPtr` length blhs) prhs (length brhs)

-- | Duplicate a bytearray into another bytearray, and run an initializer on it
copy :: forall bs1 bs2 p
      . ( ByteArrayN bs1, ByteArrayN bs2
        , KnownNat (LengthN bs1), KnownNat (LengthN bs2)
        , LengthN bs1 ~ LengthN bs2
        )
     => bs1 -> (Ptr p -> IO ()) -> IO bs2
copy bs f = alloc (Proxy @(LengthN bs2)) $ \d -> do
    withByteArray bs $ \s -> memCopy d s (length bs)
    f (castPtr d)

-- | Similar to 'copy' but also provide a way to return a value from the initializer
copyRet :: forall bs1 bs2 p a
         . ( ByteArrayN bs1, ByteArrayN bs2
           , KnownNat (LengthN bs1), KnownNat (LengthN bs2)
           , LengthN bs1 ~ LengthN bs2
           )
        => bs1 -> (Ptr p -> IO a) -> IO (a, bs2)
copyRet bs f =
    allocRet (Proxy @(LengthN bs2)) $ \d -> do
        withByteArray bs $ \s -> memCopy d s (length bs)
        f (castPtr d)

-- | Similiar to 'copy' but expect the resulting bytearray in a pure context
copyAndFreeze :: forall bs1 bs2 p
              . ( ByteArrayN bs1, ByteArrayN bs2
                , KnownNat (LengthN bs1), KnownNat (LengthN bs2)
                , LengthN bs1 ~ LengthN bs2
                )
              => bs1 -> (Ptr p -> IO ()) -> bs2
copyAndFreeze bs f =
    inlineUnsafeCreate (Proxy @(LengthN bs2)) $ \d -> do
        copyByteArrayToPtr bs d
        f (castPtr d)
{-# NOINLINE copyAndFreeze #-}

-- | Create a bytearray of a specific size containing a repeated byte value
replicate :: forall n ba . (ByteArrayN ba, n ~ LengthN ba, KnownNat n)
          => Proxy n -> Word8 -> ba
replicate n b = inlineUnsafeCreate n $ \ptr -> memSet ptr b (fromInteger $ natVal n)
{-# NOINLINE replicate #-}

-- | Create a bytearray of a specific size initialized to 0
zero :: forall n ba . (ByteArrayN ba, n ~ LengthN ba, KnownNat n) => Proxy n -> ba
zero n = unsafeCreate n $ \ptr -> memSet ptr 0 (fromInteger $ natVal n)
{-# NOINLINE zero #-}

-- | Convert a bytearray to another type of bytearray
convert :: forall bin bout
         . ( ByteArrayN bin, ByteArrayN bout
           , KnownNat (LengthN bin), KnownNat (LengthN bout)
           , LengthN bin ~ LengthN bout
           )
        => bin -> bout
convert bs = inlineUnsafeCreate (Proxy @(LengthN bin)) (copyByteArrayToPtr bs)
