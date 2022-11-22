module Classless.DecodeJson where

import Data.Argonaut (Json, JsonDecodeError)
import Data.Argonaut.Decode.Decoders (decodeArray, decodeInt)
import Data.Either (Either)

type DecodeJson a = Json -> Either JsonDecodeError a

int :: DecodeJson Int
int = decodeInt

arrayOf :: forall a. DecodeJson a -> DecodeJson (Array a) 
arrayOf = decodeArray

