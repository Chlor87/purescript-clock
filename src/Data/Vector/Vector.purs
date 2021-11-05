module Data.Vector where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Math (atan2, cos, sin, sqrt, tau, (%))

data Vec a
  = Vec a a

derive instance genericVec :: Generic (Vec a) _

instance showVec :: Show a => Show (Vec a) where
  show a = genericShow a

instance semiringVec :: Semiring a => Semiring (Vec a) where
  zero = Vec zero zero
  one = Vec one zero
  add a b = (+) <$> a <*> b
  mul a b = (*) <$> a <*> b

instance ringVec :: Ring a => Ring (Vec a) where
  sub a b = (-) <$> a <*> b

instance functorVec :: Functor Vec where
  map f (Vec x y) = Vec (f x) (f y)

instance applyVec :: Apply Vec where
  apply (Vec fx fy) (Vec x y) = Vec (fx x) (fy y)

instance applicativeVec :: Applicative Vec where
  pure s = Vec s s

instance semigroupVec :: (Semigroup a) => Semigroup (Vec a) where
  append (Vec x1 y1) (Vec x2 y2) = Vec (x1 <> x2) (y1 <> y2)

instance monoidVec :: (Semigroup a, Semiring a) => Monoid (Vec a) where
  mempty = zero

instance bindVec :: Bind Vec where
  bind v f = Vec x y
    where
    (Vec (Vec x _) (Vec _ y)) = f <$> v

polar :: Vec Number -> Tuple Number Number
polar v = length v /\ dir v

fromPolar :: Number -> Number -> Vec Number
fromPolar mag dir = Vec (cos dir) (sin dir) * pure mag

dir :: Vec Number -> Number
dir (Vec x y) = (atan2 y x + tau) % tau

length :: Vec Number -> Number
length v = sqrt $ v `dot` v

dist :: Vec Number -> Vec Number -> Number
dist a b = length $ b - a

mulScalar :: forall a. Semiring a => a -> Vec a -> Vec a
mulScalar s = map ((*) s)

addScalar :: forall a. Semiring a => a -> Vec a -> Vec a
addScalar s = map ((+) s)

subScalar :: forall a. Ring a => a -> Vec a -> Vec a
subScalar s = map ((-) s)

norm :: Vec Number -> Vec Number
norm v = fromPolar one (dir v)

lerp :: forall a. Ring a => Vec a -> Vec a -> a -> Vec a
lerp a b n = (one - n) `mulScalar` a + n `mulScalar` b

dot :: forall a. Semiring a => Vec a -> Vec a -> a
dot a b = x + y
  where
  (Vec x y) = a * b

main :: Effect Unit
main = do
  logShow $ Vec 1.0 2.0 >>= \x -> Vec (x * 2.0) (x + 2.0)
  logShow $ Vec ((*) 2.0) ((+) 3.0) <*> one

-- logShow $ (Vec 0.0 0.0) + (Vec 0.0 100.0) + (Vec 0.0 100.0)
