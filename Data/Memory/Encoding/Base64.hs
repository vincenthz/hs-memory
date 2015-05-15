-- |
-- Module      : Data.Memory.Encoding.Base64
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Base64
--
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE Rank2Types        #-}
module Data.Memory.Encoding.Base64
    ( toBase64
    ) where

import           Data.Memory.Internal.Compat
import           Data.Word
import           GHC.Prim
import           GHC.Word
import           Foreign.Storable
import           Foreign.Ptr (Ptr)

toBase64 :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
toBase64 dst src len = loop 0 0
  where
        eqChar = 0x3d

        loop i di
            | i >= len  = return ()
            | otherwise = do
                a <- peekByteOff src i
                b <- if i + 1 >= len then return 0 else peekByteOff src (i+1)
                c <- if i + 2 >= len then return 0 else peekByteOff src (i+2)

                let (w,x,y,z) = convert3 a b c

                pokeByteOff dst di     w
                pokeByteOff dst (di+1) x
                pokeByteOff dst (di+2) (if i + 1 >= len then eqChar else y)
                pokeByteOff dst (di+3) (if i + 2 >= len then eqChar else z)

                loop (i+3) (di+4)

convert3 :: Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
convert3 (W8# a) (W8# b) (W8# c) =
    let !w = narrow8Word# (uncheckedShiftRL# a 2#)
        !x = or# (and# (uncheckedShiftL# a 4#) 0x30##) (uncheckedShiftRL# b 4#)
        !y = or# (and# (uncheckedShiftL# b 2#) 0x3c##) (uncheckedShiftRL# c 6#)
        !z = and# c 0x3f##
     in (index w, index x, index y, index z)
  where
        !set = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#

        index :: Word# -> Word8
        index idx = W8# (indexWord8OffAddr# set (word2Int# idx))

{-
unBase64 :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
unBase64 dst src len
    | len `mod` 4) /= 0 = error ("decoding base64 not proper length: " ++ s)
    | otherwise         = unsafeCreateUptoN maxSz $ \ptr -> do
                                szRemove <- loop s ptr
                                    return (maxSz - szRemove)
  where maxSz = (length s `div` 4) * 3
        loop []               _   = return 0
        loop (w:x:'=':'=':[]) ptr = do
            let w' = rset w
                x' = rset x
            poke ptr ((w' `shiftL` 2) .|. (x' `shiftR` 4))
            return 2
        loop (w:x:y:'=':[])   ptr = do
            let w' = rset w
                x' = rset x
                y' = rset y
            poke ptr               ((w' `shiftL` 2) .|. (x' `shiftR` 4))
            poke (ptr `plusPtr` 1) ((x' `shiftL` 4) .|. (y' `shiftR` 2))
            return 1
        loop (w:x:y:z:r)      ptr = do
            let w' = rset w
                x' = rset x
                y' = rset y
                z' = rset z
            poke ptr               ((w' `shiftL` 2) .|. (x' `shiftR` 4))
            poke (ptr `plusPtr` 1) ((x' `shiftL` 4) .|. (y' `shiftR` 2))
            poke (ptr `plusPtr` 2) ((y' `shiftL` 6) .|. z')
            loop r (ptr `plusPtr` 3)
        loop _                _   = error ("internal error in base64 decoding")

        rset :: Char -> Word8
        rset c
            | cval <= 0xff = B.unsafeIndex rsetTable cval
            | otherwise    = 0xff
          where cval = fromEnum c
        -- dict = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        rsetTable = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3e\xff\xff\xff\x3f\
                    \\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\xff\xff\xff\xff\xff\xff\
                    \\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\
                    \\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\xff\xff\xff\xff\xff\
                    \\xff\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\
                    \\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
-}
