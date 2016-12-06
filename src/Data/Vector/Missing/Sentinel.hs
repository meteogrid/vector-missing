{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Data.Vector.Missing.Sentinel (
    Vector (..)
  , MVector (..)
  , IOVector
  , STVector
  , new
  , toBaseVector
  , fromBaseVector
) where

import           Data.Maybe (fromMaybe)
import           Data.Vector.Missing.Base (Missing, BaseVector, Nullable(..))
import           Control.Monad.Primitive     (PrimMonad(..), RealWorld)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic         as G


new
  :: ( M.MVector v (Elem a)
     , PrimMonad m
     )
  => Maybe (Elem a)
  -> Int
  -> m (MVector v (PrimState m) a)
new s = fmap (MV_Sentinel . (s,)) . M.new

toBaseVector
  :: (Nullable a, Eq (Elem a), Num (Elem a), G.Vector v (Elem a))
  => Vector v a -> v (Elem a)
toBaseVector v@(V_Sentinel (mNd,_)) = G.generate (G.length v) (nullable nd id . G.unsafeIndex v)
  where nd = fromMaybe (-9999) mNd
{-# INLINE toBaseVector #-}

fromBaseVector
  :: Maybe (Elem a)
  -> v (Elem a)
  -> Vector v a
fromBaseVector x v = V_Sentinel (x,v)

data family Vector   (v :: * -> *)        a
data family MVector  (v :: * -> * -> *) s a

type IOVector v   = MVector v RealWorld
type STVector v s = MVector v s

type instance G.Mutable (Vector v) = MVector (G.Mutable v)

newtype instance Vector  v   a = V_Sentinel  (Maybe (Elem a), v   (Elem a))
newtype instance MVector v s a = MV_Sentinel (Maybe (Elem a), v s (Elem a))

instance
  ( Nullable a
  , Eq (Elem a)
  , Num (Elem a)
  , G.Vector v (Elem a)
  ) => Missing (Vector v) a where
  type BaseVector (Vector v) a = v

instance
  ( Nullable a
  , Eq (Elem a)
  , Num (Elem a)
  , G.Vector v (Elem a)
  ) => G.Vector  (Vector v) a where
  basicUnsafeFreeze (MV_Sentinel (x,v)) =
    V_Sentinel . (x,) <$> G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (V_Sentinel (x,v)) =
    MV_Sentinel . (x,) <$> G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}

  basicLength  (V_Sentinel (_,v)) = G.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (V_Sentinel (x,v)) =
    V_Sentinel (x, G.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (V_Sentinel (Just x,v)) i = do
    val <- G.basicUnsafeIndexM v i
    return $! if val /= x then toNullable val else nullElem
  basicUnsafeIndexM (V_Sentinel (Nothing,v)) i =
    toNullable <$> G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}


instance
  ( Nullable a
  , Eq (Elem a)
  , Num (Elem a)
  , M.MVector v (Elem a)
  ) => M.MVector (MVector v) a where
  basicLength (MV_Sentinel (_,v)) = M.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MV_Sentinel (x,v)) =
    MV_Sentinel (x, M.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MV_Sentinel (_,v)) (MV_Sentinel (_,v')) = M.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i = MV_Sentinel . (Nothing,) <$> M.basicUnsafeNew i
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeRead (MV_Sentinel (Just x,v)) i = do
    val <- M.basicUnsafeRead v i
    return $! if val /= x then toNullable val else nullElem
  basicUnsafeRead (MV_Sentinel (Nothing,v)) i =
    toNullable <$> M.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MV_Sentinel (Just x,v)) i =
    M.basicUnsafeWrite v i . nullable x id
  basicUnsafeWrite (MV_Sentinel (Nothing,v)) i =
    M.basicUnsafeWrite v i . nullable (-9999) id  --Might be better to just crash?

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Sentinel (_,v)) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif
