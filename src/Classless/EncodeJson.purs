module Classless.EncodeJson
  ( DefaultInit(..)
  , EncodeJson
  , arrayOf
  , boolean
  , eitherOf
  , int
  , module Exp
  , string
  )
  where

import Classless (class Init)
import Classless.EncodeJson.Generic (sumOf) as Exp
import Classless.EncodeJson.Record (recordOf) as Exp
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Encoders (encodeArray, encodeBoolean, encodeEither, encodeInt, encodeString)
import Data.Either (Either)

type EncodeJson a = a -> Json

int :: EncodeJson Int
int = encodeInt

string :: EncodeJson String
string = encodeString

boolean :: EncodeJson Boolean
boolean = encodeBoolean

arrayOf :: forall a. EncodeJson a -> EncodeJson (Array a)
arrayOf = encodeArray

eitherOf :: forall a b. EncodeJson a -> EncodeJson b -> EncodeJson (Either a b)
eitherOf = encodeEither

data DefaultInit = DefaultInit

instance Init DefaultInit (EncodeJson Int) where
  init _ = int

instance Init DefaultInit (EncodeJson String) where
  init _ = string

instance Init DefaultInit (EncodeJson Boolean) where
  init _ = boolean

