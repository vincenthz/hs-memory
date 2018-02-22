-- |
-- Module      : Data.ByteArray.Sized
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : stable
-- Portability : Good
--

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

module Data.ByteArray.Sized
    ( ByteArrayN(..)
    , SizedByteArray
    , unSizedByteArray
    , sizedByteArray
    , unsafeSizedByteArray

    , -- * ByteArrayN operators
      allocN
    , createN
    , allocAndFreezeN
    , unsafeCreateN
    , inlineUnsafeCreateN
    , emptyN
    , packN
    , unpackN
    , consN
    , snocN
    , xorN
    , indexN
    , splitAtN
    , takeN
    , dropN
    , appendN
    , copyN
    , copyRetN
    , copyAndFreezeN
    , replicateN
    , zeroN
    , convertN
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

import Data.ByteArray.Types (ByteArrayAccess(..), ByteArray(..))

-- | Type class to emulate exactly the behaviour of 'ByteArray' but with
-- a known length at compile time
--
class ByteArrayAccess ba => ByteArrayN ba where
    -- | length of the given bytearray
    type LengthN ba :: Nat

    -- | just like 'allocRet' but with the size at the type level
    allocRetN :: forall n p a
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
    allocRetN p f = do
        (a, ba) <- allocRet n f
        pure (a, SizedByteArray ba)
      where
        n = fromInteger $ natVal p

-- | Allocate a new bytearray of specific size, and run the initializer on this memory
allocN :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
       => Proxy n
       -> (Ptr p -> IO ())
       -> IO ba
allocN p f = snd <$> allocRetN p f

-- | Allocate a new bytearray of specific size, and run the initializer on this memory
createN :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
        => Proxy n
        -> (Ptr p -> IO ())
        -> IO ba
createN = allocN
{-# NOINLINE createN #-}

-- | similar to 'allocN' but hide the allocation and initializer in a pure context
allocAndFreezeN :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
                => Proxy n -> (Ptr p -> IO ()) -> ba
allocAndFreezeN p f = unsafeDoIO (allocN p f)
{-# NOINLINE allocAndFreezeN #-}

-- | similar to 'createN' but hide the allocation and initializer in a pure context
unsafeCreateN :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
              => Proxy n -> (Ptr p -> IO ()) -> ba
unsafeCreateN sz f = unsafeDoIO (allocN sz f)
{-# NOINLINE unsafeCreateN #-}

inlineUnsafeCreateN :: forall n ba p . (ByteArrayN ba, KnownNat n, LengthN ba ~ n)
                    => Proxy n -> (Ptr p -> IO ()) -> ba
inlineUnsafeCreateN sz f = unsafeDoIO (allocN sz f)
{-# INLINE inlineUnsafeCreateN #-}

-- | Create an empty byte array
emptyN :: forall ba . (ByteArrayN ba, LengthN ba ~ 0) => Proxy 0 -> ba
emptyN p = unsafeDoIO (allocN p $ \_ -> return ())

-- | Pack a list of bytes into a bytearray
packN :: forall n ba . (ByteArrayN ba, KnownNat n, LengthN ba ~ n) => ListN n Word8 -> ba
packN l = inlineUnsafeCreateN (Proxy @n) (fill $ unListN l)
  where fill []     _  = return ()
        fill (x:xs) !p = poke p x >> fill xs (p `plusPtr` 1)
        {-# INLINE fill #-}
{-# NOINLINE packN #-}

-- | Un-pack a bytearray into a list of bytes
unpackN :: forall n ba
         . (ByteArrayAccess ba, KnownNat n, LengthN ba ~ n, NatWithinBound Int n)
        => ba -> ListN n Word8
unpackN bs =  fromMaybe (error "the impossible appened") $ toListN @n $ loop 0
  where !len = length bs
        loop i
            | i == len  = []
            | otherwise =
                let !v = unsafeDoIO $ withByteArray bs (`peekByteOff` i)
                 in v : loop (i+1)

-- | prepend a single byte to a byte array
consN :: forall bi bo
       . ( ByteArrayN bi, ByteArrayN bo
         , KnownNat (LengthN bi), KnownNat (LengthN bo)
         , (LengthN bi + 1) ~ LengthN bo
         )
      => Word8 -> bi -> bo
consN b ba = unsafeCreateN po $ \d -> withByteArray ba $ \s -> do
    pokeByteOff d 0 b
    memCopy (d `plusPtr` 1) s len
  where
    !pi = Proxy @(LengthN bi)
    !po = Proxy @(LengthN bo)
    !len = fromInteger $ natVal pi

-- | append a single byte to a byte array
snocN :: forall bi bo
       . ( ByteArrayN bi, ByteArrayN bo
         , KnownNat (LengthN bi), KnownNat (LengthN bo)
         , (LengthN bi + 1) ~ LengthN bo
         )
      => bi -> Word8 -> bo
snocN ba b = unsafeCreateN po $ \d -> withByteArray ba $ \s -> do
    memCopy d s len
    pokeByteOff d len b
  where
    !pi = Proxy @(LengthN bi)
    !po = Proxy @(LengthN bo)
    !len = fromInteger $ natVal pi

-- | Create a xor of bytes between a and b.
--
-- the returns byte array is the size of the smallest input.
xorN :: forall a b c
      . ( ByteArrayN a, ByteArrayN b, ByteArrayN c
        , KnownNat (LengthN a), KnownNat (LengthN b), KnownNat (LengthN c)
        , (LengthN a ~ LengthN c), LengthN b <= LengthN a
        )
     => a -> b -> c
xorN a b =
    unsafeCreateN (Proxy @(LengthN c)) $ \pc ->
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
indexN :: forall n ba . (ByteArrayN ba, KnownNat n, n <= (LengthN ba)) => ba -> Proxy n -> Word8
indexN b pi = unsafeDoIO $ withByteArray b $ \p -> peek (p `plusPtr` i)
  where
    i = fromInteger $ natVal pi

-- | Split a bytearray at a specific length in two bytearray
splitAtN :: forall n bi blhs brhs
          . ( ByteArrayN bi, ByteArrayN blhs, ByteArrayN brhs
            , KnownNat n, KnownNat (LengthN bi), KnownNat (LengthN blhs), KnownNat (LengthN brhs)
            , n <= (LengthN bi), (LengthN blhs + n) ~ LengthN bi, LengthN blhs ~ n
            )
         => Proxy n -> bi -> (blhs, brhs)
splitAtN pn bs = unsafeDoIO $
    withByteArray bs $ \p -> do
        b1 <- allocN pn $ \r -> memCopy r p n
        b2 <- allocN (Proxy @(LengthN brhs)) $ \r -> memCopy r (p `plusPtr` n) (len - n)
        return (b1, b2)
  where
    n = fromInteger $ natVal pn
    len = length bs

-- | Take the first @n@ byte of a bytearray
takeN :: forall n bi bo
       . ( ByteArrayN bi, ByteArrayN bo
         , KnownNat n, KnownNat (LengthN bi), KnownNat (LengthN bo)
         , LengthN bo ~ n, LengthN bi <= n
         )
      => Proxy n -> bi -> bo
takeN pn bs = unsafeCreateN pn $ \d -> withByteArray bs $ \s -> memCopy d s m
  where
    !m   = min len n
    !len = length bs
    !n   = fromInteger $ natVal pn

-- | drop the first @n@ byte of a bytearray
dropN :: forall n bi bo
       . ( ByteArrayN bi, ByteArrayN bo
         , KnownNat n, KnownNat (LengthN bi), KnownNat (LengthN bo)
         , (LengthN bo + n) ~ LengthN bi
         )
      => Proxy n -> bi -> bo
dropN pn bs = unsafeCreateN (Proxy @(LengthN bo)) $ \d ->
    withByteArray bs $ \s ->
    memCopy d (s `plusPtr` ofs) nb
  where
    ofs = min len n
    nb  = len - ofs
    len = fromInteger $ natVal (Proxy @(LengthN bi))
    n   = fromInteger $ natVal pn

-- | append one bytearray to the other
appendN :: forall blhs brhs bout
         . ( ByteArrayN blhs, ByteArrayN brhs, ByteArrayN bout
           , KnownNat (LengthN blhs), KnownNat (LengthN brhs), KnownNat (LengthN bout)
           , (LengthN brhs + LengthN blhs) ~ LengthN bout
           )
        => blhs -> brhs -> bout
appendN blhs brhs = unsafeCreateN (Proxy @(LengthN bout)) $ \p ->
    withByteArray blhs $ \plhs ->
    withByteArray brhs $ \prhs -> do
        memCopy p plhs (length blhs)
        memCopy (p `plusPtr` length blhs) prhs (length brhs)

-- | Duplicate a bytearray into another bytearray, and run an initializer on it
copyN :: forall bs1 bs2 p
       . ( ByteArrayN bs1, ByteArrayN bs2
         , KnownNat (LengthN bs1), KnownNat (LengthN bs2)
         , LengthN bs1 ~ LengthN bs2
         )
      => bs1 -> (Ptr p -> IO ()) -> IO bs2
copyN bs f = allocN (Proxy @(LengthN bs2)) $ \d -> do
    withByteArray bs $ \s -> memCopy d s (length bs)
    f (castPtr d)

-- | Similar to 'copy' but also provide a way to return a value from the initializer
copyRetN :: forall bs1 bs2 p a
          . ( ByteArrayN bs1, ByteArrayN bs2
            , KnownNat (LengthN bs1), KnownNat (LengthN bs2)
            , LengthN bs1 ~ LengthN bs2
            )
         => bs1 -> (Ptr p -> IO a) -> IO (a, bs2)
copyRetN bs f =
    allocRetN (Proxy @(LengthN bs2)) $ \d -> do
        withByteArray bs $ \s -> memCopy d s (length bs)
        f (castPtr d)

-- | Similiar to 'copy' but expect the resulting bytearray in a pure context
copyAndFreezeN :: forall bs1 bs2 p
               . ( ByteArrayN bs1, ByteArrayN bs2
                 , KnownNat (LengthN bs1), KnownNat (LengthN bs2)
                 , LengthN bs1 ~ LengthN bs2
                 )
               => bs1 -> (Ptr p -> IO ()) -> bs2
copyAndFreezeN bs f =
    inlineUnsafeCreateN (Proxy @(LengthN bs2)) $ \d -> do
        copyByteArrayToPtr bs d
        f (castPtr d)
{-# NOINLINE copyAndFreezeN #-}

-- | Create a bytearray of a specific size containing a repeated byte value
replicateN :: forall n ba . (ByteArrayN ba, n ~ LengthN ba, KnownNat n)
           => Proxy n -> Word8 -> ba
replicateN n b = inlineUnsafeCreateN n $ \ptr -> memSet ptr b (fromInteger $ natVal n)
{-# NOINLINE replicateN #-}

-- | Create a bytearray of a specific size initialized to 0
zeroN :: forall n ba . (ByteArrayN ba, n ~ LengthN ba, KnownNat n) => Proxy n -> ba
zeroN n = unsafeCreateN n $ \ptr -> memSet ptr 0 (fromInteger $ natVal n)
{-# NOINLINE zeroN #-}

-- | Convert a bytearray to another type of bytearray
convertN :: forall bin bout
          . ( ByteArrayN bin, ByteArrayN bout
            , KnownNat (LengthN bin), KnownNat (LengthN bout)
            , LengthN bin ~ LengthN bout
            )
         => bin -> bout
convertN bs = inlineUnsafeCreateN (Proxy @(LengthN bin)) (copyByteArrayToPtr bs)
