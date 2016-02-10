{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Vector.Missing.TH (deriveMissing) where

import Control.Monad
import Language.Haskell.TH

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic         as G

deriveMissing :: String -> TypeQ -> TypeQ -> DecsQ
deriveMissing name vecQ mVecQ  = do
  vec@(AppT vecT@(AppT _ base_@(ConT baseName)) type_) <- vecQ
  mVec@(AppT mVecT@(AppT _ mBase_@(ConT mBaseName)) _)  <- mVecQ
  s <- liftM VarT (newName "s")
  let newtypes = [
          NewtypeInstD [] baseName [type_]
            (NormalC vName [(NotStrict, vecT `AppT` type_)]) []
        , NewtypeInstD [] mBaseName [s, type_]
            (NormalC mvName [ (NotStrict, mVecT `AppT` s `AppT` type_)]) []
        ]
      vName     = mkName ("V_"++name)
      mvName    = mkName ("MV_"++name)
      vCon      = return (ConE vName)
      mvCon     = return (ConE mvName)
      vConP  v' = return (ConP vName [VarP v'])
      mvConP v' = return (ConP mvName [VarP v'])
      var    v' = return (VarE v')
      conBaseQ  = return base_
      conMBaseQ = return mBase_
      typeQ     = return type_
  v <- newName "vec"
  u <- newName "other_vec"
  instanceDecs <- [d|
    instance G.Vector $conBaseQ $typeQ where {
      {-# INLINE basicUnsafeFreeze #-}
    ; {-# INLINE basicUnsafeThaw #-}
    ; {-# INLINE basicLength #-}
    ; {-# INLINE basicUnsafeSlice #-}
    ; {-# INLINE basicUnsafeIndexM #-}
    ; basicUnsafeFreeze $(mvConP v) =
        liftM $vCon (G.basicUnsafeFreeze $(var v))
    ; basicUnsafeThaw $(vConP v) = liftM $mvCon (G.basicUnsafeThaw $(var v))
    ; basicLength  $(vConP v) = G.basicLength $(var v)
    ; basicUnsafeSlice m n $(vConP v) = $vCon (G.basicUnsafeSlice m n $(var v))
    ; basicUnsafeIndexM $(vConP v) = G.basicUnsafeIndexM $(var v)
    }
    instance M.MVector $conMBaseQ $typeQ where {
      {-# INLINE basicLength #-}
    ; {-# INLINE basicUnsafeSlice #-}
    ; {-# INLINE basicOverlaps #-}
    ; {-# INLINE basicUnsafeNew #-}
    ; {-# INLINE basicUnsafeRead #-}
    ; {-# INLINE basicUnsafeWrite #-}
    ; basicLength $(mvConP v) = M.basicLength $(var v)
    ; basicUnsafeSlice m n $(mvConP v) =
        $mvCon (M.basicUnsafeSlice m n $(var v))
    ; basicOverlaps $(mvConP v) $(mvConP u) = M.basicOverlaps $(var v) $(var u)
    ; basicUnsafeNew = liftM $mvCon . M.basicUnsafeNew
    ; basicUnsafeRead $(mvConP v) = M.basicUnsafeRead $(var v)
    ; basicUnsafeWrite $(mvConP v) i = M.basicUnsafeWrite $(var v) i
#if MIN_VERSION_vector(0,11,0)
    ; basicInitialize $(mvConP v) = M.basicInitialize $(var v)
    ; {-# INLINE basicInitialize #-}
#endif
    }
    |]
  return $ concat [instanceDecs, newtypes]
