{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}

-- | Rendering and streaming of Open Street Map entities as XML files.
--
--
-- See schema at: https://github.com/oschrenk/osm/blob/master/osm-io/src/main/resources/OSMSchema.xsd
--
module Naqsha.OpenStreetMap.XML.Generate
       ( -- * Open street map XML generation.
         -- $osmXMLStream$
         OsmXMLFile
       , OsmXML(..), eventSource, render, encode, pretty
       , renderSource, encodeSource, prettySource
         -- * Streaming types and combinators.
       , OsmGen, toSource
       , osmGen, osmDoc
       , sourceToTagsGen, osmMetaGen
       ) where


import           Control.Lens
import           Control.Monad.Primitive    ( PrimMonad )
import           Control.Monad.Base         ( MonadBase )
import           Control.Monad.ST
import           Data.ByteString.Lazy        ( ByteString, fromChunks )
import           Data.Conduit             as Conduit
import           Data.Conduit.List        as Conduit
import           Data.HashMap.Lazy        as HM
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   ( Text, pack  )
import           Data.Version                ( showVersion )
import qualified Data.Vector              as V
import           Data.Vector.Generic      as VG
import           Data.XML.Types  hiding      ( Node )
import           Text.XML.Stream.Render
import qualified Paths_naqsha             as NaqshaPaths

import Naqsha.Position
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.XML.Internal


-- $osmXMLStream$
--
-- One of the earliest formats supported by Open Street Map is its
-- XML. This module gives functions to render Open Street Map
-- elements. It also provides combinators for building xml event
-- streams.
--
-- = Basics.
--
-- The type `OsmGen` captures a generator for a single XML element
-- stream. One can use the combinator `toSource` to convert such a
-- generator into a stream of XML events. An element of type `OsmGen`
-- can be converted into the corresponding xml document using the
-- `osmDoc` combinator.
--
-- Types like `Node`, `Way`, `Relation` and their `Tagged` variants
-- are instances of the type class `OsmXML` and can be converted into
-- a generator using the member function `osmXMLGen`. One can also
-- convert them to streams directly using the combinator `eventStream`
-- The type `OsmXMLFile` captures a full xml file and is also an
-- instance of OsmXML.
--
-- The combinators `render`, `encode` and `pretty` can convert an
-- instance of `OsmXML` into ByteString. The combinator `encode` is
-- efficient where as `pretty` creates a pretty printed version.
--
-- = Low level streaming interface
--
-- Xml files that occur in the context of open street map can be quite
-- huge. Hence it is advisable to generate them using the low level
-- streaming interface that we provide.
--
-- The starting point is to build streams of single elements using the
-- `OsmGen` type. One can build complicated types by converting these
-- element generators into event streams and including it in the body
-- of other generators.
--
-- An `OsmGen` consists of
--
-- 1. An element name
-- 2. A set of attributes
-- 3. A body of xml elements.
--
-- Note that a `OsmGen` type only generates a single element. It is a
-- monoid which appends the attribute list and body separately and
-- picks the first name as its tag name. The standard idiom for
-- building a generator is as follows
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > genFoo :: Monad m => Source m Event -> OsmGen m
-- > genFoo body = "foo" <> attributeGen "a" "afoo" <> attributeGen "b" "bfoo" <> bodyGen body
-- >
-- > myFoo :: OsmGen Identity
-- > myFoo = genFoo (toSource genFoo empty)
-- >      -- will generate <foo a="afoo" b=bfoo"><foo a="afoo" b="bfoo"/></foo>
--
-- Notice the use of `toSource` above to convert the inner foo to a source of events.
--
--

-- | A generator for OSM elements.
newtype OsmGen m =  OsmGen (First Name, Attributes, Source m Event) deriving Monoid

instance Monad m => IsString (OsmGen m) where
  fromString = nameGen . fromString

-- | Convert the generator to a source of xml events. This results in
-- an error if the node name is not set.
toSource :: Monad m => OsmGen m -> Source m Event
toSource (OsmGen (nm, ats, bd)) = tag t ats bd
  where t = fromMaybe (error "OpenStreetMap XML Gen toSource: empty name") $ getFirst nm




-------------------------- Stuff that can be converted to osm elements -------------


-- | Types that can be streamed in an OSM XML stream.
class OsmXML a where
  -- | Given the Osm generator for this element.
  osmXMLGen :: Monad m => a -> OsmGen m

-- | Generate an xml event source out of an instance of OsmXML.
eventSource :: (Monad m, OsmXML a) => a -> Source m Event
eventSource = toSource . osmXMLGen

-- | Render a given source into lazy byte string.
renderSource :: ( PrimMonad base
                , MonadBase base m
                )
             => RenderSettings
             -> Source m Event
             -> m ByteString
renderSource rSetting src = fromChunks <$> sourceToList (src =$= renderBytes rSetting)

-- | Encode the given source of events.
encodeSource :: ( PrimMonad base
                , MonadBase base m
                )
             => Source m Event
             -> m ByteString
encodeSource = renderSource def

-- | Pretty print the given source of events.
prettySource :: ( PrimMonad base
                , MonadBase base m
                )
             => Source m Event
             -> m ByteString
prettySource = renderSource $ def { rsPretty = True }

-- | Render an element as a lazy byte string (utf-8 encoding) given a settings.
render :: OsmXML e
       => RenderSettings      -- ^ setting used to render
       -> e                   -- ^ element to render
       -> ByteString
render rSetting e = runST $ renderSource rSetting $ eventSource e

-- | Render the element efficiently i.e. with out wasting spaces.
encode ::  OsmXML e => e -> ByteString
encode = render def

-- | Pretty print a given osm element.
pretty :: OsmXML e => e -> ByteString
pretty = render $ def { rsPretty = True }


------------------------- Osm xml file -------------------------------

-- | Data type that captures an osm xml file.
data OsmXMLFile = OsmXMLFile GeoBounds (V.Vector SomeElement)

-- | Entries of an OsmXML file.
data SomeElement = forall e . OsmXML e => SomeElement e

instance OsmXML SomeElement where
  osmXMLGen (SomeElement e) = osmXMLGen e

instance OsmXML OsmXMLFile where
  osmXMLGen (OsmXMLFile gb vSrc)  = osmGen gb bdy
    where bdy = forEach (vectorToSrc vSrc) eventSource

---------------- Some Helper generators -------------------------------

-- | Generate an osm element given its bounds and body.
osmGen :: Monad m
       => GeoBounds               -- ^ The geographical bounds
       -> Source m Event          -- ^ The body of the osmGen element
       -> OsmGen m
osmGen gbounds bdSrc
  = "osm" <> osmVersionGen <> osmXmlnsGen <> osmGeneratorGen <> bodyGen bdy
  where bdy = eventSource gbounds <> bdSrc


-- | Generate a complete osm document with the given Osm element as the content.
osmDoc :: (Monad m, OsmXML a)
       => a              -- The root entity
       -> Source m Event
osmDoc osg = yield EventBeginDocument <> eventSource osg <> yield EventEndDocument



-------------------------- Basic instances of OsmXML ------------------------------

instance OsmXML Geo where
  osmXMLGen loc = "node"
               <> attributeGen "lat" (showT $ latitude  loc)
               <> attributeGen "lon" (showT $ longitude loc)


instance OsmXML GeoBounds where
  osmXMLGen gb = "bounds" <> mconcat
                 [ attributeGen "minlat" $ showT $ latitude  $ minGeo gb
                 , attributeGen "maxlat" $ showT $ latitude  $ maxGeo gb
                 , attributeGen "minlon" $ showT $ longitude $ minGeo gb
                 , attributeGen "maxlon" $ showT $ longitude $ maxGeo gb
                 ]

instance OsmXML Member where
  osmXMLGen mem = case mem of
                    NodeM     rl oid -> mGen "node" oid rl
                    WayM      rl oid -> mGen "way"  oid rl
                    RelationM rl oid -> mGen "relation" oid rl
    where mGen ty oid rl = "member" <> mconcat [ attributeGen "ref"  $ showT oid
                                               , attributeGen "role" rl
                                               , attributeGen "type" ty
                                               ]

instance OsmXML Way where
  osmXMLGen w = "way" <> bodyGen bdy
    where bdy       = forEach nodeIdSrc ndTag
          nodeIdSrc = vectorToSrc (w ^. wayNodes)
          ndTag oid = toSource $ "nd" <> attributeGen "ref" (showT oid)

instance OsmXML Relation where
  osmXMLGen rel = "relation" <> bodyGen mems
    where mems   = forEach memSrc eventSource
          memSrc = vectorToSrc (rel ^. relationMembers)


instance OsmXML e => OsmXML (Tagged e) where
  osmXMLGen te = osmXMLGen (te  ^. untagged) <> bodyGen tgs
    where tgs          = HM.foldrWithKey tgFunc mempty (te ^. tags)
          tgFunc k v   = mappend $ tagSource k v

instance OsmXML e => OsmXML (Osm e) where
  osmXMLGen oe = osmXMLGen (unMeta oe) <> osmMetaGen (oe ^. meta)


-- | Given a source of tags, i.e. a source of key value pairs, create
-- a generator out of it.
sourceToTagsGen :: Monad m => Source m (Text, Text) -> OsmGen m
sourceToTagsGen src = bodyGen $ src =$= tgConduit
  where tgConduit   = awaitForever $ toProducer . uncurry tagSource


-- | Add osm meta information to the given generator.
osmMetaGen :: Monad m
           => OsmMeta e
           -> OsmGen m
osmMetaGen mt = mconcat [ attributeGen "id"        $ showT    $ mt ^. _osmID
                        , attributeGen "user"      $ showT    $ mt ^. _modifiedUser
                        , attributeGen "uid"       $ showT    $ mt ^. _modifiedUserID
                        , attributeGen "timestamp" $ showTime $ mt ^. _timeStamp
                        , attributeGen "version"   $ showT    $ mt ^. _version
                        , attributeGen "changeset" $ showT    $ mt ^. _changeSet
                        , attributeGen "visible"   $ if         mt ^. _isVisible
                                                     then "true"
                                                     else "false"
                        ]



----------------------- Helper generators -------------------------------------------

-- | Osm namespace attribute
osmXmlnsGen :: Monad m => OsmGen m
osmXmlnsGen = attributeGen "xmlns" osmNameSpace

osmVersionGen :: Monad m => OsmGen m
osmVersionGen = attributeGen "version" (pack $ showVersion osmVersion)

osmGeneratorGen :: Monad m => OsmGen m
osmGeneratorGen = attributeGen "generator" $ "naqsha-" <> pack (showVersion NaqshaPaths.version)


-- | Generator with a given tag name
nameGen :: Monad m => Name -> OsmGen m
nameGen n = OsmGen (First $ Just n, mempty, mempty)

-- | Generator with a given body.
bodyGen     :: Source m Event -> OsmGen m
bodyGen src = OsmGen (mempty, mempty, src)

-- | Convert a tag, i.e. kv pair into a source.
tagSource :: Monad m => Text -> Text -> Source m Event
tagSource k v = toSource $ "tag" <> attributeGen "k" k <> attributeGen "v" v


---------------------- Attribute generators -------------------------------

-- | Generator with a given attributes.
attributeGen :: Monad m => Name -> Text -> OsmGen m
attributeGen a v = OsmGen (mempty, attr a v, mempty)

--------------------- Some source elements ------------------------
vectorToSrc :: (VG.Vector v a, Monad m) => v a -> Source m a
vectorToSrc v = Conduit.enumFromTo 0 (VG.length v - 1) =$= Conduit.mapM (unsafeIndexM v)

forEach :: Monad m
        => Source m i
        -> (i -> Source m o)
        -> Source m o
forEach inpSrc srcFunc = inpSrc =$= bodyConduit
  where bodyConduit = awaitForever (toProducer . srcFunc)
