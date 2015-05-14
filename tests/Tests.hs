{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import           Imports
import           Data.Word
import           Data.ByteArray      (Bytes, ScrubbedBytes, ByteArray)
import qualified Data.ByteArray as B

-- | similar to proxy
data Witness a = Witness

withWitness :: Witness a -> a -> a
withWitness _ a = a

withBytesWitness :: Bytes -> Bytes
withBytesWitness = withWitness (Witness :: Witness Bytes)

withScrubbedBytesWitness :: ScrubbedBytes -> ScrubbedBytes
withScrubbedBytesWitness = id

data Backend = BackendByte | BackendScrubbedBytes
    deriving (Show,Eq,Bounded,Enum)

allBackends :: [Backend]
allBackends = enumFrom BackendByte

data ArbitraryBS = forall a . ByteArray a => ArbitraryBS a

arbitraryBS :: Int -> Gen ArbitraryBS
arbitraryBS n = do
    backend <- elements allBackends
    case backend of
        BackendByte          -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM n arbitrary) :: Gen Bytes)
        BackendScrubbedBytes -> ArbitraryBS `fmap` ((B.pack `fmap` replicateM n arbitrary) :: Gen ScrubbedBytes)

arbitraryBSof :: Int -> Int -> Gen ArbitraryBS
arbitraryBSof minBytes maxBytes = choose (minBytes, maxBytes) >>= arbitraryBS

newtype SmallList a = SmallList [a]
    deriving (Show,Eq)

instance Arbitrary a => Arbitrary (SmallList a) where
    arbitrary = choose (0,8) >>= \n -> SmallList `fmap` replicateM n arbitrary

instance Arbitrary ArbitraryBS where
    arbitrary = arbitraryBS 127

newtype Words8 = Words8 { unWords8 :: [Word8] }
    deriving (Show,Eq)

instance Arbitrary Words8 where
    arbitrary = choose (0, 259) >>= \n -> Words8 <$> replicateM n arbitrary

testGroupBackends :: String -> (forall ba . (Eq ba, ByteArray ba) => (ba -> ba) -> [TestTree]) -> TestTree
testGroupBackends x l =
    testGroup x
        [ testGroup "Bytes" (l withBytesWitness)
        , testGroup "ScrubbedBytes" (l withScrubbedBytesWitness)
        ]

main = defaultMain $ testGroup "memory"
    [ localOption (QuickCheckTests 500) $ testGroupBackends "basic" basicProperties
    ]
  where
    basicProperties witnessID =
        [ testProperty "unpack . pack == id" $ \(Words8 l) -> l == (B.unpack . witnessID . B.pack $ l)
        , testProperty "self-eq" $ \(Words8 l) -> let b = witnessID . B.pack $ l in b == b
        , testProperty "empty-eq" $ \(Words8 l) ->
            let b = witnessID $ B.pack l
             in B.append b B.empty == b
        , testProperty "zero" $ \(Positive n) ->
            let expected = witnessID $ B.pack $ replicate n 0
             in expected == B.zero n
        , testProperty "append (append a b) c == append a (append b c)" $ \(Words8 la) (Words8 lb) (Words8 lc) ->
            let a = witnessID $ B.pack la
                b = witnessID $ B.pack lb
                c = witnessID $ B.pack lc
             in B.append (B.append a b) c == B.append a (B.append b c)
        , testProperty "concat l" $ \(SmallList l) ->
            let chunks   = map (witnessID . B.pack . unWords8) l
                expected = concatMap unWords8 l
             in B.pack expected == B.concat chunks
        ]
