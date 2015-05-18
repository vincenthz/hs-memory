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
    , unBase64Length
    , fromBase64
    ) where

import           Control.Monad
import           Data.Memory.Internal.Compat
import           Data.Memory.Internal.CompatPrim
import           Data.Memory.Internal.Imports
import           Data.Bits ((.|.))
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

-- | Get the length needed for the destination buffer for a base64 decoding.
--
-- if the length is not a multiple of 4, Nothing is returned
unBase64Length :: Ptr Word8 -> Int -> IO (Maybe Int)
unBase64Length src len
    | (len `mod` 4) /= 0 = return Nothing
    | otherwise          = do
        last1Byte <- peekByteOff src (len - 1)
        last2Byte <- peekByteOff src (len - 2)
        let dstLen = if last1Byte == eqAscii
                        then if last2Byte == eqAscii then 2 else 1
                        else 0
        return $ Just $ (len `div` 4) * 3 - dstLen
  where
        eqAscii :: Word8
        eqAscii = fromIntegral (fromEnum '=')

-- | convert from base64 in @src to binary in @dst, using the number of bytes specified
--
-- the user should use unBase64Length to compute the correct length, or check that
-- the length specification is proper. no check is done here.
fromBase64 :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Maybe Int)
fromBase64 dst src len
    | len == 0  = return Nothing
    | otherwise = loop 0 0
  where loop di i
            | i == (len-4) = do
                a <- peekByteOff src i
                b <- peekByteOff src (i+1)
                c <- peekByteOff src (i+2)
                d <- peekByteOff src (i+3)

                let (nbBytes, c',d') =
                        case (c,d) of
                            (0x3d, 0x3d) -> (2, 0x30, 0x30)
                            (0x3d, _   ) -> (0, c, d) -- invalid: automatically 'c' will make it error out
                            (_   , 0x3d) -> (1, c, 0x30)
                            (_   , _   ) -> (0 :: Int, c, d)
                case decode4 a b c' d' of
                    Left ofs -> return $ Just (i + ofs)
                    Right (x,y,z) -> do
                        pokeByteOff dst di x
                        when (nbBytes < 2) $ pokeByteOff dst (di+1) y
                        when (nbBytes < 1) $ pokeByteOff dst (di+2) z
                        return Nothing
            | otherwise    = do
                a <- peekByteOff src i
                b <- peekByteOff src (i+1)
                c <- peekByteOff src (i+2)
                d <- peekByteOff src (i+3)

                case decode4 a b c d of
                    Left ofs      -> return $ Just (i + ofs)
                    Right (x,y,z) -> do
                        pokeByteOff dst di     x
                        pokeByteOff dst (di+1) y
                        pokeByteOff dst (di+2) z
                        loop (di + 3) (i + 4)

        decode4 :: Word8 -> Word8 -> Word8 -> Word8 -> Either Int (Word8, Word8, Word8)
        decode4 a b c d =
            case (rset a, rset b, rset c, rset d) of
                (0xff, _   , _   , _   ) -> Left 0
                (_   , 0xff, _   , _   ) -> Left 1
                (_   , _   , 0xff, _   ) -> Left 2
                (_   , _   , _   , 0xff) -> Left 3
                (ra  , rb  , rc  , rd  ) ->
                    let x = (ra `unsafeShiftL` 2) .|. (rb `unsafeShiftR` 4)
                        y = (rb `unsafeShiftL` 4) .|. (rc `unsafeShiftR` 2)
                        z = (rc `unsafeShiftL` 6) .|. rd
                     in Right (x,y,z)

        rset :: Word8 -> Word8
        rset (W8# w)
            | booleanPrim (w `leWord#` 0xff##) = W8# (indexWord8OffAddr# rsetTable (word2Int# w))
            | otherwise                        = 0xff

        !rsetTable = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
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
                     \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
