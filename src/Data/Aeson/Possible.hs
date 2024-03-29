{- |
Three-valued possible types for use with `aeson`.

Useful for use in PATCH endpoints: use in records which have 'ToJSON' and
'FromJSON' instances.

See the README for suggested usage.

The 'Alternative' instance can be used to update a record (with PATCH data) as
the LHS data is kept unless it is missing:

@
-- PATCH uses new data
'HaveData' new \<|\> old == 'HaveData' new

-- PATCH sets null
'HaveNull' \<|\> old == 'HaveNull'

-- PATCH did not change data
'Missing' \<|\> old == old
@
-}
module Data.Aeson.Possible (
    Possible (..),
    toMaybe,
    fromMaybeMaybe,
) where

import Control.Applicative
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

instance Applicative Possible where
    pure = HaveData
    (HaveData f) <*> (HaveData x) = HaveData (f x)
    HaveNull <*> _ = HaveNull
    _ <*> HaveNull = HaveNull
    Missing <*> _ = Missing
    _ <*> Missing = Missing

{- | Similar to the @Alternative Maybe@ instance, picks the leftmost 'HaveData'
value.
-}
instance Alternative Possible where
    empty = Missing
    HaveNull <|> _ = HaveNull
    Missing <|> r = r
    l@(HaveData _) <|> _ = l

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

-- | Uses `omittedField` to default to 'Missing'
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
