-- | This module exposes a streaming interface for open street map elements.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE FlexibleInstances #-}
module Naqsha.OpenStreetMap.Stream
       (
         -- * Streaming interface.
         -- $osmstream$
         OsmSource, OsmEvent(..)
       , OsmEventElement(..), toSource
       , osmFile
       ) where

import Control.Lens                 ( (^.) )
import Data.Conduit
import Data.Conduit.Combinators     ( yieldMany )
import Data.Default                 ( def       )
import Data.HashMap.Lazy         as HM
import Data.Monoid                  ( (<>) )
import Data.Text                    (Text)


import Naqsha.Position
import Naqsha.OpenStreetMap.Element

-- $osmstream$
--
-- The total number of elements that describe a certain portion of a
-- map are often huge. Therefore the only feasible method to process
-- non-trivial regions of the world is by building a streaming
-- interface.
--
-- A streaming interface of elements are provided by using `Source`s,
-- `Sink`s, and `Conduit`s of `OsmEvent`. Types that can streamed as
-- an `OsmSource` (i.e. `Source` of `OsmEvent`s) are captured by the
-- class `OsmEventElement`. Elements of such type can be streamed
-- using the combinator `toSource`. Finally the combinator `osmFile`
-- can be used to put together a complete osm file.

------------------------ Osm events --------------------------

-- | Osm events for streaming.
data OsmEvent = EventGeoBounds GeoBounds
              | EventMember    Member
              | EventTag       Text  Text
              | EventNodeRef   NodeID
              ------------------------- Nested events ---------------------------------------
              | EventBeginOsm
              | EventEndOsm
              | EventNodeBegin (OsmMeta Node) Node
              | EventNodeEnd
              | EventWayBegin  (OsmMeta Way)
              | EventWayEnd
              | EventRelationBegin (OsmMeta Relation)
              | EventRelationEnd deriving (Show, Eq)


-------------- Some combinators for building sources ---------------

-- | Stream an osm file given its bounds and contents. The contents
-- stream should only generated events corresponding to `Node`s,
-- `Way`s and `Relation`s. Otherwise the resulting file will not
-- be wellformed.
osmFile :: Monad m
        => GeoBounds   -- ^ The boundary associated with the file
        -> OsmSource m -- ^ The contents
        -> OsmSource m
osmFile gb contents = yield EventBeginOsm
                      <> yield (EventGeoBounds gb)
                      <> contents
                      <> yield EventEndOsm

-- | Osm event Source
type OsmSource m = Source m OsmEvent

-- | Types that an be converted to a source of osmEvents.
class OsmEventElement a where

  -- | The starting event of the element.
  startEvent :: a -> OsmEvent

  -- | The body source for the given element.
  bodySource :: Monad m => a -> OsmSource m

  -- | The ending event of the element.
  endEvent   :: a -> OsmEvent


-- | Given an event element convert it into a source.
toSource :: (OsmEventElement a, Monad m)
         => a
         -> OsmSource m
toSource a = yield (startEvent a) <> bodySource a <> yield (endEvent a)


------------------- Nested events --------------------------------------
instance OsmEventElement Node where
  startEvent = EventNodeBegin def
  endEvent   = const EventNodeEnd
  bodySource = mempty


instance OsmEventElement Way where
  startEvent   = const $ EventWayBegin def
  endEvent     = const EventWayEnd
  bodySource w = mapOutput EventNodeRef $ yieldMany (w ^. wayNodes)


instance OsmEventElement Relation where
  startEvent   = const $ EventRelationBegin def
  endEvent     = const EventRelationEnd
  bodySource r = mapOutput EventMember $ yieldMany (r ^. relationMembers)

instance OsmEventElement e => OsmEventElement (Tagged e) where
  startEvent te = startEvent (te ^. untagged)
  endEvent   te = endEvent   (te ^. untagged)
  bodySource te = tagsToSource (te ^. tags) <> bodySource (te ^. untagged)

instance OsmEventElement (Osm Node) where
  startEvent og = EventNodeBegin (og ^. meta) (og ^. untagged)
  endEvent      = const EventNodeEnd
  bodySource og = bodySource (og ^. unMeta)

instance OsmEventElement (Osm Way) where
  startEvent ow = EventWayBegin (ow ^. meta)
  endEvent      = const EventWayEnd
  bodySource ow = bodySource (ow ^. unMeta)

instance OsmEventElement (Osm Relation) where
  startEvent orel  = EventRelationBegin (orel ^. meta)
  endEvent         = const EventRelationEnd
  bodySource orel  = bodySource (orel ^. unMeta)

---------------------------- Unnested events. --------------------------

-- | Convert tags to source.
tagsToSource :: Monad m => OsmTags -> OsmSource m
tagsToSource = HM.foldrWithKey tgFunc mempty
  where tgFunc k v  = mappend $ yield $ EventTag k v
