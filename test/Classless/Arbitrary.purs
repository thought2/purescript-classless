module Test.Classless.Arbitrary where

import Prelude

import Classless (class Init, class InitIt, initIt, initRecord, initSum, sequenceRecord, (~))
import Classless.Arbitrary (Arbitrary)
import Classless.Arbitrary as Arb
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Record (union)
import Record as R
import Test.QuickCheck.Gen (Gen)
import Unsafe.Coerce (unsafeCoerce)

type Foo =
  { foo :: Int
  , bar :: Maybe String
  , baz :: Array Boolean
  , points :: Array { x :: Int, y :: Int }
  }

sample :: Arbitrary Foo
sample = Arb.record
  { foo: arbitrary -- Arb.int
  , bar: Arb.maybe Arb.string
  , baz: Arb.array Arb.boolean
  , points: Arb.array $ Arb.record { x: Arb.int, y: Arb.int }
  }

data Baz
  = Foo Int (Maybe String)
  | Bar Boolean
  | Baz (Array { x :: Int, y :: Int })

derive instance Generic Baz _

sample3 :: Arbitrary Baz
sample3 = Arb.sum
  { "Foo": Arb.int ~ Arb.maybe Arb.string
  , "Bar": Arb.boolean
  , "Baz": Arb.array $ Arb.record { x: Arb.int , y: Arb.int }
  }

xx :: Gen Int
xx = arbitrary

data Abc = Abc String | Xyz Int

derive instance Generic Abc _

-- sample33 :: Arbitrary Abc
-- sample33 = Arb.sum {
--     "Ab"
-- }

sample2 :: Arbitrary { a :: Int, b :: Int, c :: String }
sample2 = Arb.record $ union { c: Arb.string } myInit

x :: { foo :: Gen Int }
x = initRecord I

data I = I

instance (UserArbitrary a) => Init I (Gen a) where
  init _ = arbitrary

class UserArbitrary a where
  arbitrary :: Gen a

instance UserArbitrary Int where
  arbitrary = Arb.int

class MyInit a where
  myInit :: a

instance (InitIt I a) => MyInit a where
  myInit = initIt I