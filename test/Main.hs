module Main (main) where

import Test.QuickCheck.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck.Laws

import Data.Aeson
import Data.Aeson.Possible
import Data.Typeable
import GHC.Generics (Generic)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [laws, unitTests]

newtype A_Possible a = A_Possible {unwrap :: Possible a}
    deriving stock (Show, Generic, Functor)
    deriving newtype (Eq, Applicative)

instance (Arbitrary a) => Arbitrary (A_Possible a) where
    arbitrary =
        A_Possible
            <$> Gen.oneof
                [pure Missing, pure HaveNull, HaveData <$> arbitrary]
    shrink (A_Possible v) = A_Possible <$> genericShrink v

laws :: TestTree
laws =
    testGroup
        "Laws"
        [ testEqLaws (Proxy :: Proxy (A_Possible Bool))
        , testFunctorLaws
            (Proxy :: Proxy (A_Possible))
            (Proxy :: Proxy Int)
            (Proxy :: Proxy Int)
            (Proxy :: Proxy Int)
            (Proxy :: Proxy Int)
            (\_ fu1 fu2 -> fu1 == fu2)
        , testApplicativeLaws
            (Proxy :: Proxy (A_Possible))
            (Proxy :: Proxy Int)
            (Proxy :: Proxy Int)
            (Proxy :: Proxy Int)
            (Proxy :: Proxy Int)
            (\_ fu1 fu2 -> fu1 == fu2)
        ]

data TestData = TestData
    { a :: Possible Bool
    , b :: Possible Bool
    , c :: Possible Bool
    }
    deriving stock (Show, Generic, Eq)

instance FromJSON TestData where
    parseJSON = genericParseJSON $ defaultOptions{omitNothingFields = True}

instance ToJSON TestData where
    toJSON = genericToJSON $ defaultOptions{omitNothingFields = True}

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ let
            objJson = "{\"b\":null,\"c\":true}"
            objValue = TestData{a = Missing, b = HaveNull, c = HaveData True}
           in
            testGroup
                "Object encoding and decoding"
                [ testCase "Decoding works as expected" $
                    eitherDecode objJson @?= Right objValue
                , testCase "Encoding works as expected" $
                    encode objValue @?= objJson
                ]
        , -- Encoding in non-objects leads to "null" as it must encode to something!
          -- Encoding other constructors are covered by object tests
          testGroup
            "Encoding not in an object"
            [ testCase "Bare `Missing`" $
                encode (Missing :: Possible Int) @?= "null"
            , testCase "List of Possible values" $
                encode ([HaveNull, Missing, HaveData True]) @?= "[null,null,true]"
            ]
        , testGroup
            "Bare decoding"
            [ testCase "Decode null" $
                eitherDecode "null" @?= Right (HaveNull :: Possible Bool)
            , testCase "Decode value" $
                eitherDecode "true" @?= Right (HaveData True)
            ]
        ]
