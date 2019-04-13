{-# language GADTs, ScopedTypeVariables, TypeFamilies #-}
module Data.Struct where

import Foreign

class Storable a => Struct a where
  data Field a :: * -> *
  get :: Field a v -> a -> v
  set :: Field a v -> v -> a -> a
  offsetOf :: Field a v -> Int

{-# inline peekField #-}
peekField :: (Struct a, Storable v) => Ptr a -> Field a v -> IO v
peekField p = peekByteOff p . offsetOf

{-# inline pokeField #-}
pokeField :: (Struct a, Storable v) => Ptr a -> Field a v -> v -> IO ()
pokeField p = pokeByteOff p . offsetOf
