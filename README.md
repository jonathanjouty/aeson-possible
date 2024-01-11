# aeson-possible

[![Build & Test](https://github.com/jonathanjouty/aeson-possible/actions/workflows/ci-haskell.yml/badge.svg)](https://github.com/jonathanjouty/aeson-possible/actions/workflows/ci-haskell.yml)

Three-valued possible types for use with `aeson`.

Useful for use in PATCH endpoints: use in records which have `ToJSON` and
`FromJSON` instances.

Inspired by the [`possible`](https://hackage.haskell.org/package/possible)
package, but additionally provides To/FromJSON instances using `aeson >= 2.2`'s
`omitField` and `omittedField` machinery.

## Usage

Use `Possible a` in your records

```hs
data MyRecord = MyRecord
    { myBool :: Possible Bool
    , myInt  :: Possible Int
    , myStr  :: Possible Text
    }
    deriving (Generic)
```

and then make sure to use `omitNothingFields = True` if you are generically
deriving your To/FromJSON instances

```hs
instance FromJSON MyRecord where
    parseJSON = genericParseJSON $ defaultOptions{omitNothingFields = True}

instance ToJSON MyRecord where
    toJSON = genericToJSON $ defaultOptions{omitNothingFields = True}
```

If you are creating instances any other way, see `aeson`'s documentation for
how to make use of `omitField` and `omittedField`.

_Caveat:_ if you are using `Possible` outside a record, even a `Missing` value
will likely be encoded as `null` (_e.g._ if you have a list of `Possible`
values).
