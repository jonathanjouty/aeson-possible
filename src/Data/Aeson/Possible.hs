module Data.Aeson.Possible (Possible(..), fromPossible) where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Typeable

data Possible a = Missing | HaveNull | HaveData a
  deriving (Show, Generic, Typeable, Functor)

instance Eq a => Eq (Possible a) where
  Missing    == Missing    = True
  Missing    == _          = False
  HaveNull   == HaveNull   = True
  HaveNull   == _          = False
  HaveData a == HaveData b = a == b
  HaveData _ == _          = False

-- |TODO test laws
instance Semigroup a => Semigroup (Possible a) where
  Missing <> x = x
  HaveNull <> _ = HaveNull
  HaveData a <> _ = HaveData a

-- |TODO test laws
instance Monoid a => Monoid (Possible a) where
  mempty = Missing

-- |TODO test laws
instance Applicative Possible where
    pure = HaveData
    (HaveData f) <*> (HaveData x) = HaveData (f x)
    Missing      <*> Missing      = Missing
    (HaveData _) <*> Missing      = Missing
    Missing      <*> (HaveData _) = Missing
    _            <*> _            = HaveNull

instance ToJSON a => ToJSON (Possible a) where
  toJSON = toJSON . fromPossible
  toEncoding = toEncoding . fromPossible
  omitField Missing = True
  omitField HaveNull = False
  omitField (HaveData _) = False

instance FromJSON a => FromJSON (Possible a) where
  parseJSON Null = pure HaveNull
  parseJSON v = fmap pure . parseJSON $ v
  omittedField = Just Missing

fromPossible :: Possible a -> Maybe a
fromPossible Missing = Nothing
fromPossible HaveNull = Nothing
fromPossible (HaveData a) = Just a
