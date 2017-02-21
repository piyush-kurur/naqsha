-- | This module exposes types and functions associated with the Open
-- Street Map project <https://www.openstreetmap.org>.
module Naqsha.OpenStreetMap
      ( module Naqsha.OpenStreetMap.Element
      ) where

-- Hide all the meta data lenses.
import Naqsha.OpenStreetMap.Element hiding (unsafeToOsmID, OsmID
                                           , _osmID
                                           , _modifiedUser
                                           , _modifiedUserID
                                           , _timeStamp
                                           , _changeSet
                                           , _version
                                           , _isVisible
                                           )

-- Only expose the type from here.
import Naqsha.OpenStreetMap.Element (OsmID)
