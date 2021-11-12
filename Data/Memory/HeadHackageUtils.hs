{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Data.Memory.HeadHackageUtils where

import GHC.Exts

#if MIN_VERSION_base(4,16,0)
word8ToWordCompat# :: Word8# -> Word#
word8ToWordCompat# = word8ToWord#

wordToWord8Compat# :: Word# -> Word8#
wordToWord8Compat# = wordToWord8#

wordToWord32Compat# :: Word# -> Word32#
wordToWord32Compat# = wordToWord32#

--

narrow32WordCompat# :: Word# -> Word32#
narrow32WordCompat# = wordToWord32#
#else
-- No-ops
word8ToWordCompat# :: Word# -> Word#
word8ToWordCompat# x = x

wordToWord8Compat# :: Word# -> Word#
wordToWord8Compat# x = x

wordToWord32Compat# :: Word# -> Word#
wordToWord32Compat# x = x

-- Actual narrowing
narrow32WordCompat# :: Word# -> Word#
narrow32WordCompat# = narrow32Word#
#endif
