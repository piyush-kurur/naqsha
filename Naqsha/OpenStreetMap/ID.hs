{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Module capturing ID of elements in open Street Map.
module Naqsha.OpenStreetMap.ID
      ( OsmID(..)
      ) where

import           Control.Monad               ( liftM )
import           Data.Vector.Unboxed         ( MVector(..), Vector)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Data.Word

-------------------- Instance for OsmID --------------------------------------------

-- | The ID of an object in open street map.
newtype OsmID element  = OsmID Word64 deriving (Eq, Ord, Enum)

instance Show (OsmID element) where
  show (OsmID x) = show x

--------------------- Unboxed vector instance for OSMIDs ---------------------------

newtype instance MVector s (OsmID element) = MOsmIDV  (MVector s Word64)
newtype instance Vector    (OsmID element) = OsmIDV   (Vector Word64)

instance GVM.MVector MVector (OsmID element) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          (MOsmIDV v)          = GVM.basicLength v
  basicUnsafeSlice i n (MOsmIDV v)          = MOsmIDV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MOsmIDV v1) (MOsmIDV v2)     = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MOsmIDV v) i            = OsmID `liftM` GVM.basicUnsafeRead v i
  basicUnsafeWrite (MOsmIDV v) i (OsmID x)  = GVM.basicUnsafeWrite v i x

  basicClear (MOsmIDV v)                    = GVM.basicClear v
  basicSet   (MOsmIDV v)         (OsmID x)  = GVM.basicSet v x

  basicUnsafeNew n                        = MOsmIDV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (OsmID x)    = MOsmIDV `liftM` GVM.basicUnsafeReplicate n x
  basicUnsafeCopy (MOsmIDV v1) (MOsmIDV v2)   = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MOsmIDV v)   n           = MOsmIDV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MOsmIDV v)               = GVM.basicInitialize v
#endif

instance GV.Vector Vector (OsmID element) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MOsmIDV v)         = OsmIDV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (OsmIDV v)            = MOsmIDV `liftM` GV.basicUnsafeThaw v
  basicLength (OsmIDV v)                = GV.basicLength v
  basicUnsafeSlice i n (OsmIDV v)       = OsmIDV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (OsmIDV v) i        = OsmID   `liftM`  GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MOsmIDV mv) (OsmIDV v) = GV.basicUnsafeCopy mv v
  elemseq _ (OsmID x)                 = GV.elemseq (undefined :: Vector a) x
