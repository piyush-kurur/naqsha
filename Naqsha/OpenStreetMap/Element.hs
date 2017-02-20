{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


-- | The basic elements of open street map.
module Naqsha.OpenStreetMap.Element
       (

       -- * Open Street Map elements.
       -- $osm$
         Node, Way, Relation, Osm
       , Member(..)
       , OsmID
       , OsmTags, OsmMeta, OsmElementType(..)
       -- ** Osm elements like types.
       , OsmElement(..), NodeLike(..), WayLike(..), RelationLike(..)
         -- ** Useful Lenses.
       , nodePosition, wayNodeIDs, relationMembers
       , osmElement, osmMeta
       -- *** Lenses for meta information.
       , osmID, modifiedUser, modifiedUserID, timeStamp, changeSet, version
       , isVisible
       ) where

import           Control.Monad               ( liftM )
import           Control.Lens
import           Data.Default
import qualified Data.HashMap.Lazy as HM
import           Data.Text   hiding (empty)
import           Data.Time
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Data.Vector.Unboxed         as UV -- ( MVector(..), Vector, empty, Unbox)

import           Data.Word

import Naqsha.Position



-- $osm$
--
-- The Open street map describes the world using three kinds of
-- elements given by the types `Node`, `Way` and `Relation`
-- respectively. Intuitively, an element of type `Node` captures a
-- location, a `Way` captures a path. Each of these entities can have
-- additional sematic significance. For example a particular node
-- might be a PoI, or a junction, or a bus stop. Similarly, a way
-- could be a road or a boundary of a region. These semantic meaning
-- is usually given in Open Street Map data base by associating a set
-- of key-value pairs called tags. The type `OsmTags` captures such a mapping.
--
--
-- = Element Identifiers and Meta information
--
-- Elements of type e in the data base are uniquely identified via
-- their id's. The type @`OsmID` e@ captures these id's. While the OSM
-- database currently uses 64-bit words as identifiers, the type
-- @`OsmID` e@ should be seen as an opaque type and nothing should be
-- assumed about them other than the fact that they are
-- unique. Parameterising it by the type @e@ given some type safety on
-- the Haskell side and helps in preventing confusion of ids of
-- different types.
--
-- Besides the the ID, elements have some open street map specific
-- meta data captured by the type `OsmMeta`.


-- | The ID of an object in open street map. Currently, the Open
-- Street map project uses 64-bit word for ids. We use the phantom
-- type of the entity for better type safety.
newtype OsmID element  = OsmID Word64 deriving (Eq, Ord)

-- | Convert the word64 into an OsmID. Exposed only for internal
-- modules not to be exported outside.
unsafeToOsmID :: Word64 -> OsmID e
unsafeToOsmID = OsmID

instance Show (OsmID element) where
  show (OsmID x) = show x

instance UV.Unbox (OsmID element)

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

instance OsmElement e => OsmElement (Osm e) where
  tags        = osmElement . tags
  elementType = elementType . coerceID
    where coerceID :: OsmID(Osm a) ->  OsmID a
          coerceID (OsmID x) = OsmID x

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
data Relation = Relation { __relationMembers :: V.Vector Member
                         , __relationTags    :: OsmTags
                         }


instance Default Relation where
  def = Relation V.empty HM.empty

makeLenses ''Relation

-- | Lens to focus on the members of the relation.
relationMembers :: Lens' Relation (V.Vector Member)
{-# INLINE relationMembers #-}
relationMembers = _relationMembers


instance OsmElement Relation where
  tags        = _relationTags
  elementType = const RelationT

-- | Types that behave like OSM relations.
class RelationLike r where
  toRelation    :: r        -> Relation
  fromRelation  :: Relation -> r
