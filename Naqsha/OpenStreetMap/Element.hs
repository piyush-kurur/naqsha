{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | The basic elements of open street map.
module Naqsha.OpenStreetMap.Element
       (

       -- * Open Street Map elements.
         Node, NodeLike(..), nodePosition
       , Way,  WayLike(..), wayNodeIDs
       , Relation, Member(..)
       , RelationLike(..), relationMembers
       -- ** Generic Osm elements.
       , OsmElement(..), Osm, osmElement, osmMeta
       , OsmElementType(..), OsmTags
       -- ** Open Street map Meta information.
       , OsmMeta
       , osmID, modifiedUser, modifiedUserID, timeStamp, changeSet, version
       , isVisible
       ) where

import           Control.Lens
import           Data.Default
import qualified Data.HashMap.Lazy as HM
import           Data.Text   hiding (empty)
import           Data.Time
import           Data.Vector

import Naqsha.OpenStreetMap.ID
import Naqsha.Position


------------------------- Meta data ---------------------------------

-- | The tags of an OSM element.
type OsmTags = HM.HashMap Text Text

-- | The open street map metadata that is associated with each
-- element.
data OsmMeta a = OsmMeta { __osmID          :: OsmID a
                         , __modifiedUser   :: Text
                         , __modifiedUserID :: Integer
                         , __isVisible        :: Bool
                         , __version        :: Integer
                         , __timeStamp      :: UTCTime
                         , __changeSet      :: Integer
                         }

makeLenses ''OsmMeta



-- | Lens to focus on the osmID.
osmID :: Lens' (OsmMeta e) (OsmID e)
{-# INLINE osmID #-}
osmID = _osmID

-- | Lens to focus on the user who last modified.
modifiedUser  :: Lens' (OsmMeta e) Text
{-# INLINE modifiedUser #-}
modifiedUser = _modifiedUser

-- | Lens to focus on the user id of the user that last modified.
modifiedUserID :: Lens' (OsmMeta e) Integer
modifiedUserID = _modifiedUserID

-- | Flag which indicates whether the associated element is visible or
-- not.
isVisible :: Lens' (OsmMeta e) Bool
isVisible  = _isVisible


-- | The version number of the associated entry.
version :: Lens' (OsmMeta e) Integer
version =  _version

-- | The time stamp (utc) when the entry was last changed.
timeStamp :: Lens' (OsmMeta e) UTCTime
timeStamp = _timeStamp

-- | The change set number where the object was changed.
changeSet :: Lens' (OsmMeta e) Integer
changeSet = _changeSet

-- | There types of osm element.
data OsmElementType = NodeT
                    | WayT
                    | RelationT deriving (Eq, Ord)

------------------------ OsmElement class -----------------------------

-- | A class capturing OSM element type.
class OsmElement e where
  -- | The tags for this element.
  tags       :: Lens' e OsmTags

  -- | The type of the element.
  elementType :: OsmID e -> OsmElementType


-- | An element tagged with its Osm metadata.
data Osm e = Osm { __osmElement :: e
                 , __osmMeta    :: OsmMeta e
                 }

makeLenses ''Osm

-- | Lens to focus on the element.
osmElement :: Lens' (Osm e) e
{-# INLINE osmElement #-}
osmElement = _osmElement

-- | Lens to focus on the meta information of an element.
osmMeta :: Lens' (Osm e) (OsmMeta e)
{-# INLINE osmMeta #-}
osmMeta = _osmMeta


---------------------------- The node element ----------------------------

-- | A node.
data Node = Node { __nodePosition   :: Geo
                 , __nodeTags       :: OsmTags
                 }


makeLenses ''Node

-- | Lens to focus on the coordinatis of a node.
nodePosition :: Lens' Node Geo
{-# INLINE nodePosition #-}
nodePosition = _nodePosition


instance Default Node where
  def = Node def HM.empty

instance OsmElement Node where
  tags        = _nodeTags
  elementType = const NodeT

instance Location Node where
  geoPosition = __nodePosition
  {-# INLINE geoPosition #-}

-- | Types that behave are like Node element.
class Location n => NodeLike n where
  toNode   :: n    -> Node
  fromNode :: Node -> n


instance NodeLike Node where
  toNode   = id
  fromNode = id

instance NodeLike Geo where
  toNode   g = def & nodePosition .~ g
  fromNode n = n ^. nodePosition


-------------------------- The Way element -------------------------------------

-- | A way.
data Way  = Way { __wayNodeIDs :: Vector (OsmID Node)
                , __wayTags    :: OsmTags
                }

instance Default Way where
  def = Way empty HM.empty


makeLenses ''Way


-- | Lens to focus on the node Ids that constitute the way.
wayNodeIDs :: Lens' Way (Vector (OsmID Node))
{-# LANGUAGE wayNodeIDs #-}
wayNodeIDs = _wayNodeIDs

instance OsmElement Way where
  tags        = _wayTags
  elementType = const WayT

-- | Types that behave like OSM ways.
class WayLike w where
  toWay   :: w   -> Way
  fromWay :: Way -> w

instance WayLike Way where
  toWay   = id
  fromWay = id

---------------------- A relation --------------------------------------

-- | A member of a relation.
data Member = forall a.  OsmElement a => Member (OsmID a) Text

-- | A relation.
data Relation = Relation { __relationMembers :: [Member]
                         , __relationTags    :: OsmTags
                         }


instance Default Relation where
  def = Relation [] HM.empty

makeLenses ''Relation

-- | Lens to focus on the members of the relation.
relationMembers :: Lens' Relation [Member]
{-# INLINE relationMembers #-}
relationMembers = _relationMembers


instance OsmElement Relation where
  tags        = _relationTags
  elementType = const RelationT

-- | Types that behave like OSM relations.
class RelationLike r where
  toRelation    :: r        -> Relation
  fromRelation  :: Relation -> r
