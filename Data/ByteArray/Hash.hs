-- |
-- Module      : Data.ByteArray.Hash
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- provide the SipHash algorithm.
-- reference: <http://131002.net/siphash/siphash.pdf>
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteArray.Hash
    (
    -- * SipHash
      SipKey(..)
    , SipHash(..)
    , sipHash
    , sipHashWith
    ) where

import           Data.Memory.Internal.Compat
import           Data.Memory.Hash.SipHash
import qualified Data.ByteArray.Types   as B
import qualified Data.ByteArray.Methods as B

-- | Compute the SipHash tag of a byte array for a given key.
--
-- 'sipHash` is equivalent to 'sipHashWith 2 4'
sipHash :: B.ByteArrayAccess ba
        => SipKey
        -> ba
        -> SipHash
sipHash key ba = unsafeDoIO $ B.withByteArray ba $ \p -> hash key p (B.length ba)

-- | Compute the SipHash tag of a byte array for a given key.
--
-- The user can choose the C and D numbers of rounds.
--
-- calling 'sipHash` is equivalent to 'sipHashWith 2 4'
sipHashWith :: B.ByteArrayAccess ba
            => Int    -- ^ c rounds
            -> Int    -- ^ d rounds
            -> SipKey -- ^ key
            -> ba     -- ^ data to hash
            -> SipHash
sipHashWith c d key ba = unsafeDoIO $ B.withByteArray ba $ \p -> hashWith c d key p (B.length ba)
