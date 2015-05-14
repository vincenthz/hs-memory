-- |
-- Module      : Data.Memory.Encoding.Base64
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Base64
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Data.Memory.Encoding.Base64
    ( toBase64
    ) where

import           Data.Memory.Internal.Compat
import           Data.Word
import           GHC.Prim
import           GHC.Types
import           GHC.Word
import           Control.Monad
import           Foreign.Storable
import           Foreign.Ptr (Ptr)

toBase64 :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
toBase64 _ _ _ = return ()
