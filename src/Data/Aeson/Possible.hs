module Data.Aeson.Possible (
    Possible (..),
    toMaybe,
    fromMaybeMaybe,
) where

import Data.Aeson
import GHC.Generics (Generic)

data Possible a = Missing | HaveNull | HaveData a
    deriving stock (Show, Generic, Functor)

instance (Eq a) => Eq (Possible a) where
    Missing == Missing = True
    Missing == _ = False
    HaveNull == HaveNull = True
    HaveNull == _ = False
    HaveData a == HaveData b = a == b
    HaveData _ == _ = False

instance (Semigroup a) => Semigroup (Possible a) where
    Missing <> x = x
    x <> Missing = x
    HaveNull <> _ = HaveNull
    _ <> HaveNull = HaveNull
    HaveData a <> HaveData b = HaveData $ a <> b

instance (Monoid a) => Monoid (Possible a) where
    mempty = Missing

instance Applicative Possible where
    pure = HaveData
    (HaveData f) <*> (HaveData x) = HaveData (f x)
    HaveNull <*> _ = HaveNull
    _ <*> HaveNull = HaveNull
    Missing <*> _ = Missing
    _ <*> Missing = Missing

{- | Uses 'toMaybe' to implement `toJSON` and `toEncoding`, and `aeson`'s
'omitField' to specify when the field should be left out.

/Note/ that unless the 'Possible' value is encoded as an object field it
will be `null` even when you have a 'Missing' value.
_e.g._ `[Missing, HaveNull, HaveData 42]` will be encoded as `[null,null,42]`
-}
instance (ToJSON a) => ToJSON (Possible a) where
    toJSON = toJSON . toMaybe
    toEncoding = toEncoding . toMaybe
    omitField Missing = True
    omitField HaveNull = False
    omitField (HaveData _) = False

instance (FromJSON a) => FromJSON (Possible a) where
    parseJSON Null = pure HaveNull
    parseJSON v = fmap pure . parseJSON $ v
    omittedField = Just Missing

toMaybe :: Possible a -> Maybe a
toMaybe Missing = Nothing
toMaybe HaveNull = Nothing
toMaybe (HaveData a) = Just a

fromMaybeMaybe :: Maybe (Maybe a) -> Possible a
fromMaybeMaybe Nothing = Missing
fromMaybeMaybe (Just Nothing) = HaveNull
fromMaybeMaybe (Just (Just a)) = HaveData a
