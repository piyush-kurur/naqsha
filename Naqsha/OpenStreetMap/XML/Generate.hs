{-# LANGUAGE OverloadedStrings    #-}

-- | This module given conduit based streaming interface to generate a
-- XML encodings of the open street map elements and XML file
-- format. The supported version is 0.6.
--
-- Schema: https://github.com/oschrenk/osm/blob/master/osm-io/src/main/resources/OSMSchema.xsd
module Naqsha.OpenStreetMap.XML.Generate
       ( -- * Root level node creators.
         osm, osmDoc
         -- * Streaming Open Street Map elements.
       , node, way, relation
       -- ** Streaming with metadata
       , osmNode, osmWay, osmRelation
       , withNoTags
       -- ** Helper function
       , xmlDoc
       , printEvents, prettyPrintEvents
       ) where

import           Control.Lens
import qualified Data.ByteString          as BS
import           Data.Conduit             as Conduit
import           Data.Conduit.List        as Conduit
import           Data.HashMap.Lazy        as HM
import           Data.Monoid
import           Data.Version                ( Version(..), showVersion )
import           Data.Time
import           Data.Text                   ( Text, pack )
import           Data.XML.Types  hiding      ( Node )
import           Text.XML.Stream.Render

import Naqsha.Position
import Naqsha.OpenStreetMap.ID
import Naqsha.OpenStreetMap.Element

-- | The namespace in which the given Open Street Map elements reside.
osmNameSpace :: Text
osmNameSpace = "http://openstreetmap.org/osm/0.6"

-- | Current version.
osmVersion :: Version
osmVersion = Version [0,6] []

osmGenerator :: Text
osmGenerator = "CGImap 0.0.2"

--------------- OsmFile ------------------------------------------------------


-- | Generate an stream that corresponds to the osm element. No check
-- is done to ensure that the stream contains only valid osm elements,
-- i.e. nodes, ways and relations. It is the responsibility of the
-- caller to ensure that the body stream only contains valid
-- elements. An easy way to ensure this is to only use the
-- combinators.
osm :: Monad m
    => (Latitude, Latitude)    -- ^ latitude range  (min,max)
    -> (Longitude, Longitude)  -- ^ longitude range (min,max)
    -> Source m Event          -- ^ The stream of osm elements
    -> Source m Event
osm (minLat, maxLat) (minLong, maxLong) bodyStream
  = tag "osm" ats $ do tag "bounds" boundAttr mempty
                       bodyStream

  where ats = attr "xmlns" osmNameSpace
              <> attr "generator" osmGenerator
              <> attr "version" (pack $ showVersion osmVersion)
        boundAttr = mconcat [ attr "minlat" $ showT minLat
                            , attr "maxlat" $ showT maxLat
                            , attr "minlon" $ showT minLong
                            , attr "maxlon" $ showT maxLong
                            ]
-- | Version of `osm` combinator that generates an osm element that is also prefixed by the
-- xml preamble.
osmDoc :: Monad m
       => (Latitude, Latitude)    -- ^ latitude range  (min,max)
       -> (Longitude, Longitude)  -- ^ longitude range (min,max)
       -> Source m Event          -- ^ The stream of osm elements.
       -> Source m Event
osmDoc latR longR = xmlDoc . osm latR longR

------------------------ Node ------------------------------------------------
-- | Helper function to create a node.
nodeP  :: Monad m
       => Attributes -- ^ The metadata for the
       -> Geo
       -> OsmTags
       -> Source m Event
nodeP extraAttrs g = tag "node" allAttr . osmTagsToSource
  where allAttr = attr "lat"    (showT $ latitude g)
                  <> attr "lon" (showT $ longitude g)
                  <> extraAttrs

-- | Stream a given node as XML events.
node :: Monad m
     => Geo
     -> OsmTags
     -> Source m Event
node = nodeP mempty

-- | Create node with OSM metadata.
osmNode :: Monad m
        => OsmMeta Node -- ^ The metadata for the
        -> Geo
        -> OsmTags
        -> Source m Event
osmNode = nodeP . osmAttrs

-------------------------- Ways ------------------------------------------

-- | Helper function to create a way.
wayP :: Monad m
     => Attributes
     -> OsmTags
     -> Source m (OsmID Node)
     -> Source m Event
wayP extraAttrs ots idSrc = tag "way" extraAttrs bodySrc
  where nRSrc     = forEach idSrc mkND
        mkND  oid = tag "nd" (attr "ref" $ showT oid) mempty
        bodySrc   = osmTagsToSource ots <> nRSrc


-- | Stream a way object as a set of XML events.
way :: Monad m
    => OsmTags                -- ^ The tags associated with the way
    -> Source m (OsmID Node)  -- ^ The source for node ids in the way.
    -> Source m Event
way = wayP mempty


-- | Version of `node` with and associated OSM metadata.
osmWay :: Monad m
       => OsmMeta Way           -- ^ The metadata for the
       -> OsmTags               -- ^ The tags associated with the way.
       -> Source m (OsmID Node)
       -> Source m Event
osmWay = wayP . osmAttrs

---------------------------------- Relation ----------------------------------

-- | Helper function to create a relation.
relationP :: Monad m
          => Attributes
          -> OsmTags
          -> Source m Member
          -> Source m Event
relationP attrs ots mSrc  = tag "relation" attrs bodySrc
  where memSrc  = forEach mSrc osmMember
        bodySrc = osmTagsToSource ots <> memSrc

-- | Stream a relation with a given set of tags.
relation :: Monad m
         => OsmTags                -- ^ The tags of this relation
         -> Source m Member        -- ^ The stream of members of this relation.
         -> Source m Event
relation = relationP mempty

-- | Variant of `relation` that adds an OSM meta data.
osmRelation :: Monad m
            => OsmMeta Relation       -- ^ The OSM meta data of this relation.
            -> OsmTags                -- ^ The tags of this relation
            -> Source m Member        -- ^ The stream of members of this relation.
            -> Source m Event
osmRelation = relationP . osmAttrs

-- | Stream a member element
osmMember :: Monad m
          => Member
          -> Source m Event
osmMember (Member oid rl) = tag "member" attrs mempty
  where attrs = attr "ref" (showT oid)
                <> attr "type" ty
                <> attr "role" rl
        ty    = case elementType oid of
                  NodeT     -> "node"
                  WayT      -> "way"
                  RelationT -> "relation"

------------------------- Helper function ----------------------

-- | Combinator to stream an OSM element with no tags. For example,
--
-- > streamDefaultNode        = withNoTags $ node def
-- > streamDefaultOsmNode mta = withNoTags $ osmNode mta def
-- >
-- > streamDefaultWay = withNoTags way $ do yield (fromInt 1)
--
withNoTags :: (OsmTags -> a) -> a
withNoTags = ($ HM.empty)


-- | This combinator wraps the xml stream that corresponds to the root
-- element and wraps a xml preamble around it. No checks are done to
-- ensure that the source consists of a single root element, it is the
-- responsibility of the user to do so.
xmlDoc :: Monad m
       => Source m Event   -- ^ The stream corresponding to the root
                           -- element.
       -> Source m Event
xmlDoc rootSrc = yield EventBeginDocument <> rootSrc <> yield EventEndDocument


-- | Print a set of xml `Event` using the given render settings.
printEvents :: RenderSettings
            -> Source IO Event
            -> IO ()
printEvents rConf src = runConduit $ src =$= renderBytes rConf =$= Conduit.mapM_ BS.putStr

-- | Pretty print a set of xml `Events`
prettyPrintEvents ::Source IO Event
                  -> IO ()
prettyPrintEvents = printEvents $ def { rsPretty = True }

-- | Convert tags to a source of XML events.
osmTagsToSource :: Monad m => OsmTags -> Source m Event
osmTagsToSource = HM.foldrWithKey kvSource mempty
    where kvSource k v = mappend $ tag "tag" (attr "k" k <> attr "v" v) mempty

-- | Text version of show.
showT :: Show a => a -> Text
showT = pack . show


-- | Time format used by osm.
osmTimeFmt :: String
osmTimeFmt = "%Y-%m-%dT%T%Q%z"

showTime :: FormatTime t => t -> Text
showTime = pack . formatTime defaultTimeLocale osmTimeFmt


osmAttrs :: OsmMeta e -> Attributes
osmAttrs om = mconcat [ attr "user"      $ showT $ om    ^. modifiedUser
                      , attr "uid"       $ showT $ om    ^. modifiedUserID
                      , attr "timestamp" $ showTime $ om ^. timeStamp
                      , attr "version"   $ showT  $ om   ^. version
                      , attr "changeset" $ showT  $ om   ^. changeSet
                      ]
              <> attr "visible" (if om ^. isVisible then "true" else "false")


forEach :: Monad m
        => Source m i
        -> (i -> Source m o)
        -> Source m o
forEach inpSrc srcFunc = inpSrc =$= bodyConduit
  where bodyConduit = awaitForever (toProducer . srcFunc)
