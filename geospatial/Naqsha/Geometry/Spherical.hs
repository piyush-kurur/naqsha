-- |
--
-- Module      : Naqsha.Geometry.Spherical
-- Description : Earth geometry assuming earth is a sphere.
-- Copyright   : (c) Piyush P Kurur, 2017, 2019
-- License     : Apache-2.0 OR BSD-3-Clause
-- Maintainer  : Piyush P Kurur <ppk@iitpkd.ac.in>
-- Stability   : experimental
--
--
-- TODO: Port some calculations from
-- http://www.movable-type.co.uk/scripts/latlong.html
--
module Naqsha.Geometry.Spherical
       (
         -- * Distance calculation.
         distance, distance'
       , rMean
       ) where

import Naqsha.Prelude
import Naqsha.Geometry
import Naqsha.Geometry.LatLon.Internal

--------------------- Distance calculation -------------------------------------

-- | Mean earth radius in meters. This is the radius used in the
-- haversine formula of `dHvs`.
rMean  :: Double
rMean = 6371008


-- | This combinator computes the distance (in meters) between two
-- geo-locations using the haversine distance between two points. For
-- `Position` which have an
distance :: LatLon
         -> LatLon
         -> Double -- ^ Distance in meters.
distance = distance' rMean


-- | A generalisation of `distance` that takes the radius as
-- argument. Will work on Mars for example once we set up a latitude
-- longitude system there. For this function units does not matter ---
-- the computed distance is in the same unit as the input radius. We
-- have
--
-- > distance = distance' rMean
--
distance' :: Double  -- ^ Radius (in whatever unit)
          -> LatLon
          -> LatLon
          -> Double

distance' r (LatLon lat1 lon1) (LatLon lat2 lon2) = r * c
  where p1    = toAngle lat1
        l1    = toAngle lon1
        p2    = toAngle lat2
        l2    = toAngle lon2
        dp    = p2 <> invert p1
        dl    = l2 <> invert l1

        phi1  = toRadian p1
        phi2  = toRadian p2

        a     = hav dp  + cos phi1 * cos phi2 * hav dl
        c     = 2 * atan2 (sqrt a) (sqrt (1 - a))

-- | The haversine functions.
hav :: Angle -> Double
{-# INLINE hav #-}
hav theta = stheta * stheta
  where stheta = sin (toRadian theta/2.0)
