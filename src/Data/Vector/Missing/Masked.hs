{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Data.Vector.Missing.Masked (
    Vector (..)
  , MVector (..)
) where

import           Control.Monad
import           Data.Vector.Missing.Base
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic         as G

newtype instance Vector  v   a = V_Mask  (v   Bool , v   (Elem a))
newtype instance MVector v s a = MV_Mask (v s Bool , v s (Elem a))

type Masked v a = ( Nullable a
      , G.Vector v Bool
      , G.Vector v (Elem a)
      , MMasked (G.Mutable v) a
      )

instance Masked v a => G.Vector  (Vector v) a where
  basicUnsafeFreeze (MV_Mask (x,v)) =
    liftM2 (\x' v' -> V_Mask (x',v')) (G.basicUnsafeFreeze x)
                                      (G.basicUnsafeFreeze v)
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (V_Mask (x,v)) =
    liftM2 (\x' v' -> MV_Mask (x',v')) (G.basicUnsafeThaw x)
                                       (G.basicUnsafeThaw v)
  {-# INLINE basicUnsafeThaw #-}

  basicLength  (V_Mask (_,v)) = G.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (V_Mask (x,v)) =
    V_Mask ((G.basicUnsafeSlice m n x), G.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (V_Mask (x,v)) i = do
    m <- G.basicUnsafeIndexM x i
    if m then liftM toNullable (G.basicUnsafeIndexM v i)
         else return nullElem
  {-# INLINE basicUnsafeIndexM #-}

type MMasked v a = (Nullable a, M.MVector v Bool, M.MVector v (Elem a))

instance MMasked v a => M.MVector (MVector v) a where
  basicLength (MV_Mask (_,v)) = M.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MV_Mask (x,v)) =
    MV_Mask (M.basicUnsafeSlice m n x, M.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MV_Mask (_,v)) (MV_Mask (_,v')) = M.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i =
    liftM2 (\m v -> MV_Mask (m,v)) (M.basicUnsafeNew i) (M.basicUnsafeNew i)
  {-# INLINE basicUnsafeNew #-}


  basicUnsafeRead (MV_Mask (x,v)) i = do
    m <- M.basicUnsafeRead x i
    if m then liftM toNullable (M.basicUnsafeRead v i)
         else return nullElem
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MV_Mask (x,v)) i =
    nullable (M.basicUnsafeWrite x i False)
             (\a -> M.basicUnsafeWrite x i True >> M.basicUnsafeWrite v i a)

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Mask (x,v)) = M.basicInitialize x >> M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif
