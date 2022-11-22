module Test.Classless.EncodeJson where

import Prelude

import Classless (class InitRecord, InitRecordField, NoArgs(..), initRecord, initSum, (~))
import Classless.EncodeJson (EncodeJson)
import Classless.EncodeJson as EncJ
import Classless.EncodeJson as Enj
import Classless.EncodeJson.Record (class GEncodeJson, class RecordOf)
import Data.Argonaut (Json)
import Data.Argonaut.Encode.Encoders (encodeChar)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Prim.RowList as RL
import Record (union)
import Type.Proxy (Proxy(..))

--- Sum

data Foo
  = Foo Int Int String
  | Bar1 Boolean
  | Bar2 Boolean
  | Baz

derive instance Generic Foo _

--- Sum / manually

test1 :: EncodeJson Foo
test1 = EncJ.sumOf
  { "Foo": EncJ.int ~ EncJ.int ~ EncJ.string
  , "Bar1": EncJ.boolean
  , "Bar2": EncJ.boolean
  , "Baz": NoArgs
  }

--- Sum / initialized

test2 :: EncodeJson Foo
test2 = EncJ.sumOf $ initSum EncJ.DefaultInit

test3 :: EncodeJson Foo
test3 = EncJ.sumOf $ (initSum EncJ.DefaultInit) { "Bar1" = EncJ.boolean }

---

data Foo2
  = Foo2 Int Int String
  | Bar12 Boolean
  | Bar22 Char

derive instance Generic Foo2 _

test4 :: EncodeJson Foo2
test4 = EncJ.sumOf $ (initSum EncJ.DefaultInit `union` { "Bar22": encodeChar })

---

test5 :: EncodeJson { a :: Int, b :: String }
test5 = EncJ.recordOf
  { "a": EncJ.int
  , "b": EncJ.string
  }

---

class AppEncodeJson a where
  appEncodeJson :: a -> Json

instance AppEncodeJson Int where
  appEncodeJson = EncJ.int

instance AppEncodeJson String where
  appEncodeJson = EncJ.string

instance AppEncodeJson a => AppEncodeJson (Array a) where
  appEncodeJson = EncJ.arrayOf appEncodeJson

instance (AppEncodeJson a, AppEncodeJson b) => AppEncodeJson (Either a b) where
  appEncodeJson = EncJ.eitherOf appEncodeJson appEncodeJson

instance
  ( RecordOf rowSpec row
  , InitRecord EncJ.DefaultInit rowSpec
  ) =>
  AppEncodeJson (Record row) where
  appEncodeJson = EncJ.recordOf (initRecord EncJ.DefaultInit :: { | rowSpec })
