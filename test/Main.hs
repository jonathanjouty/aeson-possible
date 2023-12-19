module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Aeson
import GHC.Generics (Generic)
import Data.Aeson.Possible

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [laws, unitTests]

laws :: TestTree
laws = testGroup "Laws" [] -- TODO

data TestData = TestData
  { a :: Possible Bool
  , b :: Possible Bool
  , c :: Possible Bool
  } deriving (Show, Generic, Eq)

instance FromJSON TestData where
  parseJSON = genericParseJSON $ defaultOptions { omitNothingFields = True }

instance ToJSON TestData where
  toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Decoding works as expected" $
      eitherDecode json @?= Right value
  , testCase "Encoding works as expected" $
      encode value @?= json
  ]
  where
    json = "{\"b\":null,\"c\":true}"
    value = TestData { a = Missing, b = HaveNull, c = HaveData True }
