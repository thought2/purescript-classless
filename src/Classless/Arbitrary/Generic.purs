module Classless.Arbitrary.Generic where

import Prelude

import Classless (class SequenceProduct, sequenceProduct, (~))
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Generic.Rep (class Generic, Argument, Constructor(..), Product, Sum(..), to)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row (class Cons, class Union)
import Record as Record
import Test.QuickCheck.Gen (Gen, chooseInt, frequency)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)

class Sum ri a | a -> ri where
  sum :: { | ri } -> Gen a

instance
  ( Generic a rep
  , GetCases ri rep ro'
  , Homogeneous ro' (Gen rep)
  , HMap (MapRepToA a) { | ro' } { | ro }
  , Homogeneous ro (Gen a)
  , HomRecToNEA ro (Gen a)
  ) =>
  Sum ri a where
  sum sumSpec = homRecToNEA cases
    <#> (Tuple 1.0)
    # frequency
    where
    casesRep = getCases sumSpec (Proxy :: _ rep)
    cases = hmap (D $ Proxy :: _ a) casesRep

data MapRepToA (a :: Type) = D (Proxy a)

instance (Generic a rep, Functor f) => Mapping (MapRepToA a) (f rep) (f a) where
  mapping _ = map to

class GetCases (ri :: Row Type) (rep :: Type) (ro :: Row Type) | rep ri -> ro where
  getCases :: { | ri } -> Proxy rep -> { | ro }

instance
  ( Cons sym (Gen (Constructor sym genRep)) () ro
  , Cons sym prodSpec rix ri
  , SequenceProduct prodSpec genRep Gen
  , IsSymbol sym
  ) =>
  GetCases ri (Constructor sym rep) ro where
  getCases r _ =
    Record.insert (Proxy :: _ sym) (Constructor <$> head) {}
    where
    head = r
      # Record.get (Proxy :: _ sym)
      # sequenceProduct

instance
  ( GetCases ri repA roA
  , GetCases ri repB roB
  , HMap MapInl { | roA } { | roA' }
  , HMap MapInr { | roB } { | roB' }
  , Union roA' roB' ro
  ) =>
  GetCases ri (Sum repA repB) ro where
  getCases r _ = Record.union
    (hmap MapInl xA)
    (hmap MapInr xB)
    where
    xA = getCases r (Proxy :: _ repA) :: { | roA }
    xB = getCases r (Proxy :: _ repB) :: { | roB }

-- 

class HomRecToNEA r a where
  homRecToNEA :: { | r } -> NonEmptyArray a

instance
  ( Homogeneous r a
  , HFoldl ToNEA Unit { | r } (NonEmptyArray a)
  ) =>
  HomRecToNEA r a where
  homRecToNEA r = hfoldl ToNEA unit r

data ToNEA = ToNEA

instance Folding ToNEA Unit a (NonEmptyArray a) where
  folding ToNEA _ = pure

instance Folding ToNEA (NonEmptyArray a) a (NonEmptyArray a) where
  folding ToNEA acc x = x `NEA.cons` acc

--

data MapInl = MapInl

instance Mapping MapInl (Gen a) (Gen (Sum a b)) where
  mapping MapInl = map Inl

data MapInr = MapInr

instance Mapping MapInr (Gen b) (Gen (Sum a b)) where
  mapping MapInr = map Inr

--- Tests

data Foo = Foo Int Int | Bar Int Int

derive instance Generic Foo _

test2 :: Gen Foo
test2 = sum
  { "Foo": chooseInt 0 100 ~ chooseInt 0 100
  , "Bar": chooseInt 0 100 ~ chooseInt 0 100
  }

test1
  :: { "Foo" :: Gen (Constructor "Foo" (Product (Argument Int) (Argument Int)))
     }
test1 = getCases
  { "Foo": chooseInt 0 100 ~ chooseInt 0 100
  }
  (Proxy :: _ (Constructor "Foo" (Product (Argument Int) (Argument Int))))
