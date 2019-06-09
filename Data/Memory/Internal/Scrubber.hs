-- |
-- Module      : Data.Memory.Internal.Scrubber
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Compat
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Memory.Internal.Scrubber
    ( getScrubber
    ) where

import GHC.Exts
import GHC.Types
import Data.Memory.PtrMethods

getScrubber :: Int# -> (Addr# -> State# RealWorld -> State# RealWorld)
getScrubber sz addr s =
    case scrubBytes s of
        (# s', _ #) -> s'
  where IO scrubBytes = memSet (Ptr addr) 0 (I# sz)
