{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import           Imports
import           Utils
import           Data.Word
import           Data.ByteArray               (Bytes, ScrubbedBytes, ByteArray)
import qualified Data.ByteArray          as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray.Parse    as Parse

import qualified SipHash

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
    arbitrary = arbitraryBSof 0 259

newtype Words8 = Words8 { unWords8 :: [Word8] }
    deriving (Show,Eq)

instance Arbitrary Words8 where
    arbitrary = choose (0, 259) >>= \n -> Words8 <$> replicateM n arbitrary

testGroupBackends :: String -> (forall ba . (Show ba, Eq ba, ByteArray ba) => (ba -> ba) -> [TestTree]) -> TestTree
testGroupBackends x l =
    testGroup x
        [ testGroup "Bytes" (l withBytesWitness)
        , testGroup "ScrubbedBytes" (l withScrubbedBytesWitness)
        ]

base64Kats =
    [ ("pleasure.", "cGxlYXN1cmUu")
    , ("leasure.", "bGVhc3VyZS4=")
    , ("easure.", "ZWFzdXJlLg==")
    , ("asure.", "YXN1cmUu")
    , ("sure.", "c3VyZS4=")
    ]

base64URLKats =
    [ ("pleasure.", "cGxlYXN1cmUu")
    , ("leasure.", "bGVhc3VyZS4")
    , ("easure.", "ZWFzdXJlLg")
    , ("asure.", "YXN1cmUu")
    , ("sure.", "c3VyZS4")
    , ("\DC4\251\156\ETX\217~", "FPucA9l-") -- From RFC4648
    , ("\DC4\251\156\ETX\217\DEL", "FPucA9l_")
    , ("", "")
    ]

base16Kats =
    [ ("this is a string", "74686973206973206120737472696e67") ]

base32Kats =
    [ ("-pleasure.", "FVYGYZLBON2XEZJO")
    , ("pleasure.",  "OBWGKYLTOVZGKLQ=")
    , ("leasure.",   "NRSWC43VOJSS4===")
    , ("easure.",    "MVQXG5LSMUXA====")
    , ("asure.",     "MFZXK4TFFY======")
    , ("sure.",      "ON2XEZJO")
    , ("ure.",       "OVZGKLQ=")
    , ("re.",        "OJSS4===")
    , ("e.",         "MUXA====")
    , (".",          "FY======")
    , ("",           "")
    ]

encodingTests witnessID =
    [ testGroup "BASE64"
        [ testGroup "encode-KAT" encodeKats64
        , testGroup "decode-KAT" decodeKats64
        ]
    , testGroup "BASE64URL"
        [ testGroup "encode-KAT" encodeKats64URLUnpadded
        , testGroup "decode-KAT" decodeKats64URLUnpadded
        ]
    , testGroup "BASE32"
        [ testGroup "encode-KAT" encodeKats32
        , testGroup "decode-KAT" decodeKats32
        ]
    , testGroup "BASE16"
        [ testGroup "encode-KAT" encodeKats16
        , testGroup "decode-KAT" decodeKats16
        ]
    ]
  where
        encodeKats64 = map (toTest B.Base64) $ zip [1..] base64Kats
        decodeKats64 = map (toBackTest B.Base64) $ zip [1..] base64Kats
        encodeKats32 = map (toTest B.Base32) $ zip [1..] base32Kats
        decodeKats32 = map (toBackTest B.Base32) $ zip [1..] base32Kats
        encodeKats16 = map (toTest B.Base16) $ zip [1..] base16Kats
        decodeKats16 = map (toBackTest B.Base16) $ zip [1..] base16Kats
        encodeKats64URLUnpadded = map (toTest B.Base64URLUnpadded) $ zip [1..] base64URLKats
        decodeKats64URLUnpadded = map (toBackTest B.Base64URLUnpadded) $ zip [1..] base64URLKats

        toTest :: B.Base -> (Int, (String, String)) -> TestTree
        toTest base (i, (inp, out)) = testCase (show i) $
            let inpbs = witnessID $ B.convertToBase base $ witnessID $ B.pack $ unS inp
                outbs = witnessID $ B.pack $ unS out
             in outbs @=? inpbs
        toBackTest :: B.Base -> (Int, (String, String)) -> TestTree
        toBackTest base (i, (inp, out)) = testCase (show i) $
            let inpbs = witnessID $ B.pack $ unS inp
                outbs = B.convertFromBase base $ witnessID $ B.pack $ unS out
             in Right inpbs @=? outbs

parsingTests witnessID =
    [ testCase "parse" $
        let input = witnessID $ B.pack $ unS "xx abctest"
            abc   = witnessID $ B.pack $ unS "abc"
            est   = witnessID $ B.pack $ unS "est"
            result = Parse.parse ((,,) <$> Parse.take 2 <*> Parse.byte 0x20 <*> (Parse.bytes abc *> Parse.anyByte)) input
         in case result of
                Parse.ParseOK remaining (_,_,_) -> est @=? remaining
                _                               -> assertFailure ""
    ]

main = defaultMain $ testGroup "memory"
    [ localOption (QuickCheckTests 5000) $ testGroupBackends "basic" basicProperties
    , testGroupBackends "encoding" encodingTests
    , testGroupBackends "parsing" parsingTests
    , testGroupBackends "hashing" $ \witnessID ->
        [ testGroup "SipHash" $ SipHash.tests witnessID
        ]
    ]
  where
    basicProperties witnessID =
        [ testProperty "unpack . pack == id" $ \(Words8 l) -> l == (B.unpack . witnessID . B.pack $ l)
        , testProperty "self-eq" $ \(Words8 l) -> let b = witnessID . B.pack $ l in b == b
        , testProperty "add-empty-eq" $ \(Words8 l) ->
            let b = witnessID $ B.pack l
             in B.append b B.empty == b
        , testProperty "zero" $ \(Positive n) ->
            let expected = witnessID $ B.pack $ replicate n 0
             in expected == B.zero n
        , testProperty "Ord" $ \(Words8 l1) (Words8 l2) ->
            compare l1 l2 == compare (witnessID $ B.pack l1) (B.pack l2)
        , testProperty "Monoid(mappend)" $ \(Words8 l1) (Words8 l2) ->
            mappend l1 l2 == (B.unpack $ mappend (witnessID $ B.pack l1) (B.pack l2))
        , testProperty "Monoid(mconcat)" $ \(SmallList l) ->
            mconcat (map unWords8 l) == (B.unpack $ mconcat $ map (witnessID . B.pack . unWords8) l)
        , testProperty "append (append a b) c == append a (append b c)" $ \(Words8 la) (Words8 lb) (Words8 lc) ->
            let a = witnessID $ B.pack la
                b = witnessID $ B.pack lb
                c = witnessID $ B.pack lc
             in B.append (B.append a b) c == B.append a (B.append b c)
        , testProperty "concat l" $ \(SmallList l) ->
            let chunks   = map (witnessID . B.pack . unWords8) l
                expected = concatMap unWords8 l
             in B.pack expected == witnessID (B.concat chunks)
        , testProperty "cons b bs == reverse (snoc (reverse bs) b)" $ \(Words8 l) b ->
            let b1 = witnessID (B.pack l)
                b2 = witnessID (B.pack (reverse l))
                expected = B.pack (reverse (B.unpack (B.snoc b2 b)))
             in B.cons b b1 == expected
        , testProperty "all == Prelude.all" $ \(Words8 l) b ->
            let b1 = witnessID (B.pack l)
                p  = (/= b)
             in B.all p b1 == all p l
        , testProperty "any == Prelude.any" $ \(Words8 l) b ->
            let b1 = witnessID (B.pack l)
                p  = (== b)
             in B.any p b1 == any p l
        , testProperty "singleton b == pack [b]" $ \b ->
            witnessID (B.singleton b) == B.pack [b]
        ]
