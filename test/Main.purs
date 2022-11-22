module Test.Main where

import Prelude

import Classless.DecodeJson (DecodeJson)
import Classless.DecodeJson.Generic (sumOf) as DecJ
import Data.Argonaut.Decode.Decoders as DecJ
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)

-- main :: Effect Unit
-- main = do
--   log "🍝"
--   log "You should add some tests."




--- Test

data ABC = A Int String | B

derive instance Generic ABC _

-- instance EncodeJson ABC where
--   encodeJson = genericEncodeJson

instance Show ABC where
  show = genericShow

test1 :: DecodeJson ABC
test1 = DecJ.sumOf $ 
  { "A": DecJ.decodeInt /\ DecJ.decodeString
  , "B": unit
  }