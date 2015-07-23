-- |
-- Module      : Data.ByteArray.Encoding
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- ByteArray base converting
--
module Data.ByteArray.Encoding
    ( convertToBase
    , convertFromBase
    , Base(..)
    ) where

import           Data.ByteArray.Types
import qualified Data.ByteArray.Types        as B
import qualified Data.ByteArray.Methods      as B
import           Data.Memory.Internal.Compat
import           Data.Memory.Encoding.Base16
import           Data.Memory.Encoding.Base32
import           Data.Memory.Encoding.Base64

-- | Different bases that can be used
--
-- See <http://tools.ietf.org/html/rfc4648 RFC4648> for details.
-- In particular, Base64 can be standard or
-- <http://tools.ietf.org/html/rfc4648#section-5 URL-safe>. URL-safe
-- encoding is often used in other specifications without
-- <http://tools.ietf.org/html/rfc4648#section-3.2 padding> characters.
data Base = Base16            -- ^ similar to hexadecimal
          | Base32
          | Base64            -- ^ standard Base64
          | Base64URLUnpadded -- ^ unpadded URL-safe Base64
          deriving (Show,Eq)

-- | Convert a bytearray to the equivalent representation in a specific Base
convertToBase :: (ByteArrayAccess bin, ByteArray bout) => Base -> bin -> bout
convertToBase base b = case base of
    Base16 -> doConvert (binLength * 2) toHexadecimal
    Base32 -> let (q,r)  = binLength `divMod` 5
                  outLen = 8 * (if r == 0 then q else q + 1)
               in doConvert outLen toBase32
    Base64 -> doConvert base64Length toBase64
    -- Base64URL         -> doConvert base64Length (toBase64URL True)
    Base64URLUnpadded -> doConvert base64UnpaddedLength (toBase64URL False)
  where
    binLength = B.length b

    base64Length = let (q,r) = binLength `divMod` 3
                    in 4 * (if r == 0 then q else q+1)

    base64UnpaddedLength = let (q,r) = binLength `divMod` 3
                            in 4 * q + (if r == 0 then 0 else r+1)
    doConvert l f =
        B.unsafeCreate l $ \bout ->
        B.withByteArray b     $ \bin  ->
            f bout bin binLength

-- | Try to Convert a bytearray from the equivalent representation in a specific Base
convertFromBase :: (ByteArrayAccess bin, ByteArray bout) => Base -> bin -> Either String bout
convertFromBase Base16 b
    | odd (B.length b) = Left "base16: input: invalid length"
    | otherwise        = unsafeDoIO $ do
        (ret, out) <-
            B.allocRet (B.length b `div` 2) $ \bout ->
            B.withByteArray b               $ \bin  ->
                fromHexadecimal bout bin (B.length b)
        case ret of
            Nothing  -> return $ Right out
            Just ofs -> return $ Left ("base16: input: invalid encoding at offset: " ++ show ofs)
convertFromBase Base32 b = unsafeDoIO $
    withByteArray b $ \bin -> do
        mDstLen <- unBase32Length bin (B.length b)
        case mDstLen of
            Nothing     -> return $ Left "base32: input: invalid length"
            Just dstLen -> do
                (ret, out) <- B.allocRet dstLen $ \bout -> fromBase32 bout bin (B.length b)
                case ret of
                    Nothing  -> return $ Right out
                    Just ofs -> return $ Left ("base32: input: invalid encoding at offset: " ++ show ofs)
convertFromBase Base64 b = unsafeDoIO $
    withByteArray b $ \bin -> do
        mDstLen <- unBase64Length bin (B.length b)
        case mDstLen of
            Nothing     -> return $ Left "base64: input: invalid length"
            Just dstLen -> do
                (ret, out) <- B.allocRet dstLen $ \bout -> fromBase64 bout bin (B.length b)
                case ret of
                    Nothing  -> return $ Right out
                    Just ofs -> return $ Left ("base64: input: invalid encoding at offset: " ++ show ofs)
convertFromBase Base64URLUnpadded b = unsafeDoIO $
    withByteArray b $ \bin ->
        case unBase64LengthUnpadded (B.length b) of
            Nothing     -> return $ Left "base64URL unpadded: input: invalid length"
            Just dstLen -> do
                (ret, out) <- B.allocRet dstLen $ \bout -> fromBase64URLUnpadded bout bin (B.length b)
                case ret of
                    Nothing  -> return $ Right out
                    Just ofs -> return $ Left ("base64URL unpadded: input: invalid encoding at offset: " ++ show ofs)

