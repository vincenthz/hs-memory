module Utils where

import           Data.Word
import           Data.ByteArray               (Bytes, ScrubbedBytes)

unS :: String -> [Word8]
unS = map (fromIntegral . fromEnum)

ascii :: [Word8] -> String
ascii = map (toEnum . fromIntegral)

-- | similar to proxy
data Witness a = Witness

withWitness :: Witness a -> a -> a
withWitness _ a = a

withBytesWitness :: Bytes -> Bytes
withBytesWitness = withWitness (Witness :: Witness Bytes)

withScrubbedBytesWitness :: ScrubbedBytes -> ScrubbedBytes
withScrubbedBytesWitness = id

numberedList :: [a] -> [(Int, a)]
numberedList = zip [1..]
