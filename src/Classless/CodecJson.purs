module Classless.Json where

import Prelude

import Classless (class MapSumProp, mapProduct, mapSumProp)
import Classless.DecodeJson (DecodeJson)
import Classless.DecodeJson as DecJ
import Classless.EncodeJson (EncodeJson)
import Classless.EncodeJson as EncJ
import Classless.EncodeJson.Generic (class EncodeRep)
import Classless.EncodeJson.Generic as EncJ
import Data.Generic.Rep (class Generic, NoArguments(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Mapping (class Mapping)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type CodecJson a =
  { encodeJson :: EncodeJson a
  , decodeJson :: DecodeJson a
  }

int :: CodecJson Int
int =
  { encodeJson: EncJ.int
  , decodeJson: DecJ.int
  }

arrayOf :: forall a. CodecJson a -> CodecJson (Array a)
arrayOf x =
  { encodeJson: EncJ.arrayOf x.encodeJson
  , decodeJson: DecJ.arrayOf x.decodeJson
  }

class SumOf spec a | a -> spec where
  sumOf :: { | spec } -> CodecJson a

instance
  ( MapSumProp "encodeJson" spec specEncodeJson
  , EncJ.SumOf specEncodeJson a
  ) =>
  SumOf spec a where
  sumOf x =
    { encodeJson:
        mapSumProp (Proxy :: _ "encodeJson") x
          # EncJ.sumOf

    , decodeJson: unsafeCoerce 1
    }

---

data ABC = Foo Int | Bar

derive instance Generic ABC _

data MyMap = MyMap

-- instance Mapping MyMap Int String where
--   mapping _ _ = ""

-- instance Init MyMap Int String where
--   mapping _ _ = ""

-- x :: CodecJson ABC
-- x = sumOf
--   { "Foo": int
--   , "Bar": unit
--   }