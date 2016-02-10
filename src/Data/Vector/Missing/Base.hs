{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.Vector.Missing.Base (
    Missing
  , Vector
  , MVector
  , IOVector
  , STVector
  , Nullable (..)
) where

import           Control.Monad.Primitive     (RealWorld)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic         as G
import           Text.Read                   (Read(..), readListPrecDefault)



data family Vector   (v :: * -> *)        a
data family MVector  (v :: * -> * -> *) s a

type IOVector v   = MVector v RealWorld
type STVector v s = MVector v s

type instance G.Mutable (Vector v) = MVector (G.Mutable v)

class ( G.Vector  (Vector  v) a
      , M.MVector (MVector (G.Mutable v)) a
      --, G.Vector  v                      (Elem a)
      --, M.MVector (G.Mutable v)          (Elem a)
      ) => Missing v a

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


{-
instance (NFData (Elem a), NFData (ElemVector a))
  => NFData (MaskedVector a) where
  rnf (MaskedVector (Mask m,v)) = rnf m `seq` rnf v `seq` ()
  rnf (MaskedVector (UseNoData nd,v)) = rnf nd `seq` rnf v `seq` ()
  rnf (MaskedVector (AllValid,v)) = rnf v `seq` ()

instance (NFData (Elem a), NFData (ElemMVector s a))
  => NFData (MaskedMVector s a) where
  rnf (MaskedMVector (Mask m,v)) = rnf m `seq` rnf v `seq` ()
  rnf (MaskedMVector (UseNoData nd,v)) = rnf nd `seq` rnf v `seq` ()
  rnf (MaskedMVector (AllValid,v)) = rnf v `seq` ()
-}

instance (Missing v a, Show a) => Show (Vector v a) where
  showsPrec = G.showsPrec

instance (Missing v a, Read a) => Read (Vector v a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault


{-
#if MIN_VERSION_vector(0,11,0)
instance (G.Vector Vector a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

instance (G.Vector Vector a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT
#else
instance (G.Vector Vector a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Stream.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Stream.eq (G.stream xs) (G.stream ys))

instance (G.Vector Vector a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Stream.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Stream.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Stream.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Stream.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Stream.cmp (G.stream xs) (G.stream ys) /= LT
#endif

instance G.Vector Vector a => IsList (Vector a) where
  type Item (Vector a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList

instance G.Vector Vector a => Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = G.empty

  {-# INLINE mappend #-}
  mappend = (G.++)

  {-# INLINE mconcat #-}
  mconcat = G.concat
-}
