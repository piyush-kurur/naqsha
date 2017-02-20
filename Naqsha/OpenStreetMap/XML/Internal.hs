{-# LANGUAGE OverloadedStrings    #-}
-- | XML Internal module.
module Naqsha.OpenStreetMap.XML.Internal where

import           Data.Version                ( Version(..) )
import           Data.Time
import           Data.Text                   ( Text, pack, unpack )

-- | The namespace in which the given Open Street Map elements reside.
osmNameSpace :: Text
osmNameSpace = "http://openstreetmap.org/osm/0.6"

-- | Current version.
osmVersion :: Version
osmVersion = Version [0,6] []

osmGenerator :: Text
osmGenerator = "CGImap 0.0.2"

-- | Time format used by osm.
osmTimeFmt :: String
osmTimeFmt = "%Y-%m-%dT%T%Q%z"

-- | Text version of show.
showT :: Show a => a -> Text
showT = pack . show

-- | show the time as text
showTime :: FormatTime t => t -> Text
showTime = pack . formatTime defaultTimeLocale osmTimeFmt

-- | read teh time as text
timeParser :: (Monad m, ParseTime t) => Text -> m t
timeParser = parseTimeM True defaultTimeLocale osmTimeFmt . unpack
