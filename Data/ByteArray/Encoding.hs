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
    , Base(..)
    ) where

import           Data.ByteArray.Types
import qualified Data.ByteArray.Types        as B
import qualified Data.ByteArray.Methods      as B
import           Data.Memory.Encoding.Base16
import           Data.Memory.Encoding.Base64

data Base = Base16
          | Base64
          deriving (Show,Eq)

convertToBase :: (ByteArrayAccess bin, ByteArray bout) => Base -> bin -> bout
convertToBase Base16 b =
    B.unsafeCreate (B.length b * 2) $ \bout ->
    B.withByteArray b               $ \bin  ->
        toHexadecimal bout bin (B.length b)
convertToBase Base64 b =
    B.unsafeCreate (B.length b * 4 `div` 3) $ \bout ->
    withByteArray b                         $ \bin  ->
        toBase64 bout bin (B.length b)
