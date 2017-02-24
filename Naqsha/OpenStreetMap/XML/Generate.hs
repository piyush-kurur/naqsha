{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE Rank2Types                 #-}

-- | Rendering and streaming of Open Street Map entities as XML files.
--
--
-- See schema at: https://github.com/oschrenk/osm/blob/master/osm-io/src/main/resources/OSMSchema.xsd
--
module Naqsha.OpenStreetMap.XML.Generate
       ( -- * Open street map XML generation.
         -- $osmXMLStream$
         OsmXMLFile(..), SomeElement(..)
       , OsmXMLElement(..), eventSource, render, encode, pretty
       , renderSource, encodeSource, prettySource
         -- * Streaming types and combinators.
       , OsmElementGen, toSource
       , osmWrap, wrapDoc, wrapGenDoc
       , sourceToTagsGen, osmMetaGen
       ) where


import           Control.Lens
import           Control.Monad.Primitive    ( PrimMonad )
import           Control.Monad.Base         ( MonadBase )
import           Control.Monad.ST
import           Data.ByteString.Lazy        ( ByteString, fromChunks )
import           Data.Conduit             as Conduit
import           Data.Conduit.Combinators as Conduit
import           Data.HashMap.Lazy        as HM
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   ( Text, pack  )
import           Data.Version                ( showVersion )
import qualified Data.Vector              as V
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
-- The type OsmElementGen captures a generator for a single XML element
-- stream. One can use the combinator `toSource` to convert such a
-- generator into a stream of XML events. An element of type OsmElementGen
-- can be converted into the corresponding xml document using the
-- `wrapDoc` combinator.
--
-- Types like `Node`, `Way`, `Relation` and their `Tagged` variants
-- are instances of the type class `OsmXMLElement` and can be converted into
-- a generator using the member function `osmXMLElementGen`. One can also
-- convert them to streams directly using the combinator `eventStream`
-- The type `OsmXMLFile` captures a full xml file and is also an
-- instance of OsmXMLElement.
--
-- The combinators `render`, `encode` and `pretty` can convert an
-- instance of `OsmXMLElement` into ByteString. The combinator `encode` is
-- efficient where as `pretty` creates a pretty printed version.
--
-- = Low level streaming interface
--
-- Xml files that occur in the context of open street map can be quite
-- huge. Hence it is advisable to generate them using the low level
-- streaming interface that we provide.
--
-- The starting point is to build streams of single elements using the
-- `OsmElementGen` type. One can build complicated types by converting
-- these element generators into event streams and including it in the
-- body of other generators.
--
-- An `OsmElementGen` consists of
--
-- 1. An element name
-- 2. A set of attributes
-- 3. A body of xml elements.
--
-- Note that a `OsmElementGen` type only generates a single element. It is a
-- monoid which appends the attribute list and body separately and
-- picks the first name as its tag name. The standard idiom for
-- building a generator is as follows
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > genFoo :: Monad m => Source m Event -> OsmElementGen m
-- > genFoo body = "foo" <> attributeGen "a" "afoo" <> attributeGen "b" "bfoo" <> bodyGen body
-- >
-- > myFoo :: OsmElementGen Identity
-- > myFoo = genFoo (toSource genFoo empty)
-- >      -- will generate <foo a="afoo" b=bfoo"><foo a="afoo" b="bfoo"/></foo>
--
-- Notice the use of `toSource` above to convert the inner foo to a source of events.
--
--

-- | A generator for OSM elements.
newtype OsmElementGen m =  OsmElementGen (First Name, Attributes, Source m Event) deriving Monoid

instance Monad m => IsString (OsmElementGen m) where
  fromString = nameGen . fromString

-- | Convert the generator to a source of xml events. This results in
-- an error if the node name is not set.
toSource :: Monad m => OsmElementGen m -> Source m Event
toSource (OsmElementGen (nm, ats, bd)) = tag t ats bd
  where t = fromMaybe (error "OpenStreetMap XML Gen toSource: empty name") $ getFirst nm




-------------------------- Stuff that can be converted to osm elements -------------


-- | Types that can be streamed in an OSM XML stream.
class OsmXMLElement a where
  -- | Given the Osm generator for this element.
  osmXMLElementGen :: Monad m => a -> OsmElementGen m

-- | Generate an xml event source out of an instance of OsmXMLElement.
eventSource :: (Monad m, OsmXMLElement a) => a -> Source m Event
eventSource = toSource . osmXMLElementGen

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
render :: OsmXMLElement e
       => RenderSettings      -- ^ setting used to render
       -> e                   -- ^ element to render
       -> ByteString
render rSetting e = runST $ renderSource rSetting $ eventSource e

-- | Render the element efficiently i.e. with out wasting spaces.
encode ::  OsmXMLElement e => e -> ByteString
encode = render def

-- | Pretty print a given osm element.
pretty :: OsmXMLElement e => e -> ByteString
pretty = render $ def { rsPretty = True }


------------------------- Osm xml file -------------------------------

-- | Data type that captures an osm xml file.
data OsmXMLFile = OsmXMLFile GeoBounds (V.Vector SomeElement)

-- | Entries of an OsmXML file.
data SomeElement = forall e . OsmXMLElement e => SomeElement e

instance OsmXMLElement SomeElement where
  osmXMLElementGen (SomeElement e) = osmXMLElementGen e

instance OsmXMLElement OsmXMLFile where
  osmXMLElementGen (OsmXMLFile gb vSrc)  = osmWrap gb bdy
    where bdy = forEach (yieldMany vSrc) eventSource

---------------- Some Helper generators -------------------------------

-- | Wrap a given source of events into a <osm></osm> tag.
osmWrap :: Monad m
        => GeoBounds               -- ^ The geographical bounds
        -> Source m Event          -- ^ The body of the osmGen element
        -> OsmElementGen m
osmWrap gbounds bdSrc
  = "osm" <> osmVersionGen <> osmXmlnsGen <> osmGeneratorGen <> bodyGen bdy
  where bdy = eventSource gbounds <> bdSrc


-- | Generate a complete osm document, i.e. including the xml
-- preamble, with the given element as the root element.
wrapDoc :: (Monad m, OsmXMLElement a)
        => a                    -- ^  The root entity
        -> Source m Event
wrapDoc = wrapGenDoc . osmXMLElementGen


-- | Wrap a root level generator into a complete osm document, i.e. including the xml preamble.
wrapGenDoc :: Monad m
           => OsmElementGen m            -- ^ Generator for the root element.
           -> Source m Event
wrapGenDoc gen = yield EventBeginDocument <> toSource gen <> yield EventEndDocument



-------------------------- Basic instances of OsmXMLElement ------------------------------

instance OsmXMLElement Geo where
  osmXMLElementGen loc = "node"
                         <> attributeGen "lat" (showT $ latitude  loc)
                         <> attributeGen "lon" (showT $ longitude loc)


instance OsmXMLElement GeoBounds where
  osmXMLElementGen gb = "bounds" <> mconcat
                        [ attributeGen "minlat" $ showT $ latitude  $ minGeo gb
                        , attributeGen "maxlat" $ showT $ latitude  $ maxGeo gb
                        , attributeGen "minlon" $ showT $ longitude $ minGeo gb
                        , attributeGen "maxlon" $ showT $ longitude $ maxGeo gb
                        ]

instance OsmXMLElement Member where
  osmXMLElementGen mem = case mem of
                           NodeM     rl oid -> mGen "node" oid rl
                           WayM      rl oid -> mGen "way"  oid rl
                           RelationM rl oid -> mGen "relation" oid rl
    where mGen ty oid rl = "member" <> mconcat [ attributeGen "ref"  $ showT oid
                                               , attributeGen "role" rl
                                               , attributeGen "type" ty
                                               ]

instance OsmXMLElement Way where
  osmXMLElementGen w = "way" <> bodyGen bdy
    where bdy       = forEach nodeIdSrc ndTag
          nodeIdSrc = yieldMany (w ^. wayNodes)
          ndTag oid = toSource $ "nd" <> attributeGen "ref" (showT oid)

instance OsmXMLElement Relation where
  osmXMLElementGen rel = "relation" <> bodyGen mems
    where mems   = forEach memSrc eventSource
          memSrc = yieldMany (rel ^. relationMembers)


instance OsmXMLElement e => OsmXMLElement (Tagged e) where
  osmXMLElementGen te = osmXMLElementGen (te  ^. untagged) <> bodyGen tgs
    where tgs          = HM.foldrWithKey tgFunc mempty (te ^. tags)
          tgFunc k v   = mappend $ tagSource k v

instance OsmXMLElement e => OsmXMLElement (Osm e) where
  osmXMLElementGen oe = osmXMLElementGen (unMeta oe) <> osmMetaGen (oe ^. meta)


-- | Given a source of tags, i.e. a source of key value pairs, create
-- a generator out of it.
sourceToTagsGen :: Monad m => Source m (Text, Text) -> OsmElementGen m
sourceToTagsGen src = bodyGen $ src =$= tgConduit
  where tgConduit   = awaitForever $ toProducer . uncurry tagSource


-- | Add osm meta information to the given generator.
osmMetaGen :: Monad m
           => OsmMeta e
           -> OsmElementGen m
osmMetaGen mt = mconcat [ maybeAttr mt _osmID          $ attributeGen "id"        . showT
                        , maybeAttr mt _modifiedUser   $ attributeGen "user"
                        , maybeAttr mt _modifiedUserID $ attributeGen "uid"       . showT
                        , maybeAttr mt _timeStamp      $ attributeGen "timestamp" . showTime
                        , maybeAttr mt _version        $ attributeGen "version"   . showT
                        , maybeAttr mt _changeSet      $ attributeGen "changeset" . showT
                        , maybeAttr mt _isVisible      $ visibleFunc
                        ]

  where maybeAttr :: Monad m => a -> Lens' a (Maybe b) -> (b -> OsmElementGen m) -> OsmElementGen m
        maybeAttr a lns gen = maybe mempty gen $ a ^. lns
        visibleFunc cond
          | cond          = attributeGen "visible" "true"
          | otherwise     = attributeGen "visible" "false"

----------------------- Helper generators -------------------------------------------

-- | Osm namespace attribute
osmXmlnsGen :: Monad m => OsmElementGen m
osmXmlnsGen = attributeGen "xmlns" osmNameSpace

osmVersionGen :: Monad m => OsmElementGen m
osmVersionGen = attributeGen "version" (pack $ showVersion osmVersion)

osmGeneratorGen :: Monad m => OsmElementGen m
osmGeneratorGen = attributeGen "generator" $ "naqsha-" <> pack (showVersion NaqshaPaths.version)


-- | Generator with a given tag name
nameGen :: Monad m => Name -> OsmElementGen m
nameGen n = OsmElementGen (First $ Just n, mempty, mempty)

-- | Generator with a given body.
bodyGen     :: Source m Event -> OsmElementGen m
bodyGen src = OsmElementGen (mempty, mempty, src)

-- | Convert a tag, i.e. kv pair into a source.
tagSource :: Monad m => Text -> Text -> Source m Event
tagSource k v = toSource $ "tag" <> attributeGen "k" k <> attributeGen "v" v


---------------------- Attribute generators -------------------------------

-- | Generator with a given attributes.
attributeGen :: Monad m => Name -> Text -> OsmElementGen m
attributeGen a v = OsmElementGen (mempty, attr a v, mempty)

forEach :: Monad m
        => Source m i
        -> (i -> Source m o)
        -> Source m o
forEach inpSrc srcFunc = inpSrc =$= bodyConduit
  where bodyConduit = awaitForever (toProducer . srcFunc)
