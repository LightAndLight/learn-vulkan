{-# language ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
module Data.Type.Function where

import Data.Kind (Constraint)

data TyFun :: * -> * -> *
type (~>) a b = TyFun a b -> *
infixr 5 ~>

type family Apply (f :: a ~> b) (x :: a) :: b

data EqSym0 :: (~>) * Constraint
type EqSym1 (a :: *) = Eq a

data OrdSym0 :: (~>) * Constraint
type OrdSym1 (a :: *) = Ord a

data ShowSym0 :: (~>) * Constraint
type ShowSym1 (a :: *) = Show a

type instance Apply EqSym0 a = EqSym1 a
type instance Apply OrdSym0 a = OrdSym1 a
type instance Apply ShowSym0 a = ShowSym1 a
