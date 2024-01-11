module Main (main) where

import Test.QuickCheck.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck.Laws

import Data.Aeson
import Data.Aeson.Possible
import Data.Monoid (Sum)
import Data.Typeable
import GHC.Generics (Generic)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [laws, unitTests]

newtype A_Possible a = A_Possible {unwrap :: Possible a}
    deriving stock (Show, Generic, Functor)
    deriving newtype (Eq, Semigroup, Monoid, Applicative)

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
        , testSemigroupLaws (Proxy :: Proxy (A_Possible (Sum Int)))
        , testMonoidLaws (Proxy :: Proxy (A_Possible (Sum Int)))
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
        [ testCase "Decoding works as expected" $
            eitherDecode json @?= Right value
        , testCase "Encoding works as expected" $
            encode value @?= json
        ]
  where
    json = "{\"b\":null,\"c\":true}"
    value = TestData{a = Missing, b = HaveNull, c = HaveData True}
