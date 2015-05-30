-- |
-- Module      : Data.Memory.Internal.Scrubber
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Compat
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
#include "MachDeps.h"
module Data.Memory.Internal.Scrubber
    ( getScrubber
    ) where

import GHC.Prim
import GHC.IO
import Data.Memory.Internal.CompatPrim (booleanPrim)

getScrubber :: Int# -> (Addr# -> IO ())
getScrubber sz 
    | booleanPrim (sz ==# 4#)  = scrub4
    | booleanPrim (sz ==# 8#)  = scrub8
    | booleanPrim (sz ==# 16#) = scrub16
    | booleanPrim (sz ==# 32#) = scrub32
    | otherwise                = scrubBytes sz
  where
        scrub4 a = IO $ \s -> (# writeWord32OffAddr# a 0# 0## s, () #)
#if WORD_SIZE_IN_BITS == 64
        scrub8 a = IO $ \s -> (# writeWord64OffAddr# a 0# 0## s, () #)
        scrub16 a = IO $ \s1 ->
            let !s2 = writeWord64OffAddr# a  0# 0## s1
                !s3 = writeWord64OffAddr# a  8# 0## s2
             in (# s3, () #)
        scrub32 a = IO $ \s1 ->
            let !s2 = writeWord64OffAddr# a  0# 0## s1
                !s3 = writeWord64OffAddr# a  8# 0## s2
                !s4 = writeWord64OffAddr# a 16# 0## s3
                !s5 = writeWord64OffAddr# a 24# 0## s4
             in (# s5, () #)
#else
        scrub8 a = IO $ \s1 ->
            let !s2 = writeWord32OffAddr# a  0# 0## s1
                !s3 = writeWord32OffAddr# a  4# 0## s2
             in (# s3, () #)
        scrub16 a = IO $ \s1 ->
            let !s2 = writeWord32OffAddr# a  0# 0## s1
                !s3 = writeWord32OffAddr# a  4# 0## s2
                !s4 = writeWord32OffAddr# a  8# 0## s3
                !s5 = writeWord32OffAddr# a 12# 0## s4
             in (# s5, () #)
        scrub32 a = IO $ \s1 ->
            let !s2 = writeWord32OffAddr# a  0# 0## s1
                !s3 = writeWord32OffAddr# a  4# 0## s2
                !s4 = writeWord32OffAddr# a  8# 0## s3
                !s5 = writeWord32OffAddr# a 12# 0## s4
                !s6 = writeWord32OffAddr# a 16# 0## s5
                !s7 = writeWord32OffAddr# a 20# 0## s6
                !s8 = writeWord32OffAddr# a 24# 0## s7
                !s9 = writeWord32OffAddr# a 28# 0## s8
             in (# s9, () #)
#endif

scrubBytes :: Int# -> Addr# -> IO ()
scrubBytes sz8 addr = IO $ \s -> (# loop sz8 addr s, () #)
  where loop :: Int# -> Addr# -> State# RealWorld -> State# RealWorld
        loop n a s
            | booleanPrim (n ==# 0#) = s
            | otherwise              =
                case writeWord8OffAddr# a 0# 0## s of
                    s' -> loop (n -# 1#) (plusAddr# a 1#) s'
