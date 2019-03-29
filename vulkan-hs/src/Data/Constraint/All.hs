{-# language ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Constraint.All where

import Data.Kind (Constraint)

import Data.Type.Function (type (~>), Apply)

data AllSym0 :: (~>) (k ~> Constraint) ((~>) [k] Constraint)
data AllSym1 (f :: k ~> Constraint) :: (~>) [k] Constraint
type AllSym2 (f :: k ~> Constraint) (xs :: [k]) = All f xs

type instance Apply AllSym0 f = AllSym1 f
type instance Apply (AllSym1 f) xs = AllSym2 f xs

type family All (f :: k ~> Constraint) (as :: [k]) :: Constraint where
  All f '[] = ()
  All f (x ': xs) = (Apply f x, All f xs)
