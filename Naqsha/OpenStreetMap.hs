-- | This module exposes types and functions associated with the _Open
-- Street Map_ project https://www.openstreetmap.org
module Naqsha.OpenStreetMap
      ( module Naqsha.OpenStreetMap.ID
      , module Naqsha.OpenStreetMap.Element
      -- ** Streaming interface to Open Street Map XML.
      , module Naqsha.OpenStreetMap.XML.Generate
      ) where


import Naqsha.OpenStreetMap.ID ( OsmID )
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.XML.Generate
