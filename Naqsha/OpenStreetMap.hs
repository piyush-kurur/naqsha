-- | This module exposes types and functions associated with the Open
-- Street Map project <https://www.openstreetmap.org>.
module Naqsha.OpenStreetMap
      ( module Naqsha.OpenStreetMap.Element
      ) where

import Naqsha.OpenStreetMap.Element hiding (unsafeToOsmID, OsmID)
import Naqsha.OpenStreetMap.Element (OsmID)
