{-# LANGUAGE DerivingVia #-}
module Libs.V2 where
import Data.Ix
import Data.Monoid (Ap(..))

data V2 a = V2 !a !a
  deriving stock (Show, Eq, Ord, Bounded, Ix, Functor)
  deriving (Semigroup, Monoid, Num) via (Ap V2 a)

instance Applicative V2 where
  pure x = V2 x x
  V2 f g <*> V2 x y = V2 (f x) (g y)

instance Monad V2 where
  V2 a b >>= f = case (f a, f b) of
    (V2 x _, V2 _ y) -> V2 x y

instance Foldable V2 where
  foldMap f (V2 x y) = f x <> f y

instance Traversable V2 where
  traverse f (V2 x y) = V2 <$> f x <*> f y
  sequenceA (V2 x y) = V2 <$> x <*> y

reduce :: V2 Int -> V2 Int
reduce (V2 x y) = let g = gcd x y in V2 (x `div` g) (y `div` g)