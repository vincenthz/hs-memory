import Test.DocTest

main :: IO ()
main = doctest
    [ "-iData"
    , "-fobject-code"
    , "-DWITH_BYTESTRING_SUPPORT"
    , "Data/ByteArray/Encoding.hs"
    ]
