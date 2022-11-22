module Classless.EncodeJson.Record where

import Prelude

import Data.Argonaut.Core (Json, fromObject)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object as FO
import Prim.Row (class Cons)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class RecordOf (spec :: Row Type) r | r -> spec where
  recordOf :: { | spec } -> {|r} -> Json

instance encodeRecord ::
  ( GEncodeJson spec row list
  , RL.RowToList row list
  ) =>
  RecordOf spec row where
  recordOf spec rec = fromObject $ gEncodeJson spec rec (Proxy :: Proxy list)

class GEncodeJson (spec :: Row Type) (row :: Row Type) (list :: RL.RowList Type) where
  gEncodeJson :: forall proxy. { | spec } -> Record row -> proxy list -> FO.Object Json

instance gEncodeJsonNil :: GEncodeJson spec row RL.Nil where
  gEncodeJson _ _ _ = FO.empty

instance gEncodeJsonCons ::
  ( -- EncodeJson value
    Cons field (value -> Json) specX spec
  , GEncodeJson spec row tail
  , IsSymbol field
  , Row.Cons field value tail' row
  ) =>
  GEncodeJson spec row (RL.Cons field value tail) where
  gEncodeJson spec row _ = do
    FO.insert
      (reflectSymbol _field)
      (encodeJson $ Record.get _field row)
      (gEncodeJson spec row (Proxy :: Proxy tail))
    where
    _field = Proxy :: Proxy field
    encodeJson =
      Record.get _field spec