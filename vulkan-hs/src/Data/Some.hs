{-# language GADTs, PolyKinds #-}
module Data.Some where

data Some (f :: k -> *) where
  Some :: f a -> Some f
