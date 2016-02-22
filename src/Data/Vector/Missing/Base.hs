{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.Vector.Missing.Base (
    Missing (..)
  , Nullable (..)
) where

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic         as G



class ( Nullable  a
      , G.Vector  v                            a
      , M.MVector (G.Mutable v)                a
      , G.Vector  (BaseVector v a)             (Elem a)
      , M.MVector (G.Mutable (BaseVector v a)) (Elem a)
      ) => Missing v a where
  type BaseVector v a :: * -> *

class Nullable f where
  type Elem  f      :: *
  nullElem          :: f
  toNullable        :: Elem f -> f
  nullable          :: b -> (Elem f -> b) -> f -> b

instance Nullable (Maybe a) where
  type Elem (Maybe a)       = a
  nullElem = Nothing
  toNullable = Just
  nullable = maybe
  {-# INLINE nullElem #-}
  {-# INLINE toNullable #-}
  {-# INLINE nullable #-}
