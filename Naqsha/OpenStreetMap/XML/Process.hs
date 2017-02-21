{-# LANGUAGE OverloadedStrings    #-}
-- | This module exposes combinators to process Open street map XML
-- objects.
module Naqsha.OpenStreetMap.XML.Process
      ( node, osmNode
      ) where


-- | This module given conduit based interface to process XML
-- encodings of the open street map elements. It also provides
-- combinators to build XML file format used for presenting collection
-- of Open Street Map objects. The supported version is 0.6.
--
-- Schema: https://github.com/oschrenk/osm/blob/master/osm-io/src/main/resources/OSMSchema.xsd
import           Control.Monad.Catch       ( MonadThrow )
import           Control.Monad.State       ( execState )
import           Control.Lens
import           Data.Conduit       as     Conduit
import           Data.Text                 ( Text )
import           Data.XML.Types     hiding ( Node )
import           Text.XML.Stream.Parse


import Naqsha.Position
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.XML.Internal

-- | Parse a single node from the input stream and pass the rest of
-- the stream to its continuation.
node  :: MonadThrow m
      => (Geo -> ConduitM (Text, Text) o m r)
      -> ConduitM Event o m (Maybe r)
node tgsC = tagName "node" geoP handleTags
  where handleTags = processTags . tgsC

-- | Parse a single node that also has Osm meta data.
osmNode :: MonadThrow m
      => (OsmMeta Node -> Geo -> ConduitM (Text, Text) o m r)
      -> ConduitM Event o m (Maybe r)
osmNode tgsC = tagName "node" attrsP handleTags
  where attrsP     = (,) <$> osmMetaP <*> geoP
        handleTags = processTags . uncurry tgsC

geoP :: AttrParser Geo
geoP = Geo <$> latP <*> longP
  where angleP :: Angular a => Name -> AttrParser a
        latP  :: AttrParser Latitude
        longP :: AttrParser Longitude
        angleP nm = deg . readT <$> requireAttr nm
        latP  = angleP "lat"
        longP = angleP "lon"

processTags :: MonadThrow m => ConduitM (Text, Text) o m r -> ConduitM Event o m r
processTags  = fuse $ manyYield kvPair
  where kvPair     = tagName "tag" kvPairAttr $ \ pair -> yield pair >> return pair

        kvPairAttr = (,) <$> requireAttr "k" <*> requireAttr "v"


osmMetaP :: AttrParser (OsmMeta e)
osmMetaP = do oid  <- OsmID . readT <$> requireAttr "id"
              u    <- requireAttr "user"
              uid  <- readT    <$> requireAttr "uid"
              ts   <- requireAttr "timestamp" >>= timeParser
              ver  <- readT    <$> requireAttr "version"
              cset <- readT    <$> requireAttr "changeset"
              t    <- requireAttr "visible"
              let setFields = do osmID          .= oid
                                 modifiedUser   .=  u
                                 modifiedUserID .= uid
                                 timeStamp      .= ts
                                 version        .= ver
                                 changeSet      .= cset
                                 case t of
                                   "true"  -> isVisible .= True
                                   "false" -> isVisible .= False
                                   _       -> fail "parse: visible attribute"
                in return $ execState setFields unsafeOsmMetaUndefined
