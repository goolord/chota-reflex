{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Chota 
  ( HexColor (..)
  , RGB (..)
  , Icon (..)
  , encodeRGB
  , iconGram
  , icon
  , dynIcon
  , row
  , col
  , xwide
  )
where

import Data.String (IsString)
import Data.Word (Word64, Word8)
import Reflex
import Reflex.Dom
import Data.Text (Text)
import Data.Default.Class
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder
import Data.Map.Strict (Map)
import Web.FontAwesomeType
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | Chota uses semantic html elements instead of css
--   classes in order to stay small and readable.
--   Learn more:
--   - https://www.w3schools.com/html/html5_semantic_elements.asp
--   - https://jenil.github.io/chota/#docs

newtype HexColor = HexColor Text

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | RGB 0xff 0xff 0xff = #ffffff
data RGB = RGB Word8 Word8 Word8

encodeRGB (RGB r g b) = TL.toStrict $ toLazyText $ decimal r <> decimal g <> decimal b
{-# INLINE encodeRGB #-}

-- | Icons from https://icongr.am/
data Icon = Icon
  { iconName :: Text
  , iconSize :: Word64
  , iconColor :: RGB
  }

fontAwesomeIcon :: FontAwesome -> Word64 -> RGB -> Icon
fontAwesomeIcon fa sz color = Icon (T.drop 3 $ fontAwesomeClass fa) sz color

instance Default Icon where
  def = Icon "clarity/search" 16 (RGB 0x00 0x00 0x00)

iconGram :: Icon -> Text
iconGram (Icon name size color) = 
  "https://icongr.am/fontawesome/" 
  <> name 
  <> ".svg?size=" <> tshow size 
  <> "&color=" <> encodeRGB color

icon :: DomBuilder t m => Icon -> m ()
icon ic = elAttr "img" ("src" =: iconGram ic) blank

dynIcon :: (DomBuilder t m, PostBuild t m) => Dynamic t Icon -> m ()
dynIcon ic = elDynAttr "img" (("src" =:) <$> (iconGram <$> ic)) blank

row :: DomBuilder t m => m a -> m a
row = divClass "row"

col :: DomBuilder t m => m a -> m a
col = divClass "col"

xwide :: DomBuilder t m => Int -> m a -> m a
xwide x = divClass ("col-" <> tshow x)

-- | Helper classes.
data Helper =
    TextPrimary
  | TextLight
  | TextDark
  | TextGrey
  | TextError
  | TextSuccess
  | BgPrimary
  | BgLight
  | BgDark
  | BgGrey
  | BgError
  | BgSuccess
  | PullRight
  | PullLeft
  | TextCenter
  | TextLeft
  | TextRight
  | TextUppercase
  | TextLowercase
  | TextCapitalize
  | FullScreen
  | FullWidth
  | VerticalAlign
  | HorizontalAlign
  | Center
  | IsRight
  | IsLeft
  | Fixed
  | Paddingless
  | Marginless
  | Clearfix 
  | Hidden 
  | HidePhone 
  | HideTablet 
  | BColor BulmaColor

encodeHelper :: Helper -> Text
encodeHelper x = case x of
  TextPrimary     -> "text-primary"
  TextLight       -> "text-light"
  TextDark        -> "text-dark"
  TextGrey        -> "text-grey"
  TextError       -> "text-error"
  TextSuccess     -> "text-success"
  BgPrimary       -> "bg-primary"
  BgLight         -> "bg-light"
  BgDark          -> "bg-dark"
  BgGrey          -> "bg-grey"
  BgError         -> "bg-error"
  BgSuccess       -> "bg-success"
  PullRight       -> "pull-right"
  PullLeft        -> "pull-left"
  TextCenter      -> "is-text-center"
  TextLeft        -> "is-text-left"
  TextRight       -> "is-text-right"
  TextUppercase   -> "is-text-uppercase"
  TextLowercase   -> "is-text-lowercase"
  TextCapitalize  -> "is-text-capitalize"
  FullScreen      -> "is-full-screen"
  FullWidth       -> "is-full-width"
  VerticalAlign   -> "is-vertical-align"
  HorizontalAlign -> "is-horizontal-align"
  Center          -> "is-center"
  IsRight         -> "is-right"
  IsLeft          -> "is-left"
  Fixed           -> "is-fixed"
  Paddingless     -> "is-paddingless"
  Marginless      -> "is-marginless"
  Clearfix        -> "clearfix"
  Hidden          -> "is-hidden"
  HidePhone       -> "hide-phone"
  HideTablet      -> "hide-tablet"
  BColor c        -> encodeBulmaColor c

-- | Classes for Bulma's color variables
data BulmaColor = 
    BCWhite
  | BCBlack
  | BCLight
  | BCDark
  | BCPrimary
  | BCLink
  | BCInfo
  | BCSuccess
  | BCWarning
  | BCDanger

encodeBulmaColor :: BulmaColor -> Text
encodeBulmaColor c = case c of
  BCWhite   -> "ist-white"
  BCBlack   -> "ist-black"
  BCLight   -> "ist-light"
  BCDark    -> "ist-dark"
  BCPrimary -> "ist-primary"
  BCLink    -> "ist-link"
  BCInfo    -> "ist-info"
  BCSuccess -> "ist-success"
  BCWarning -> "ist-warning"
  BCDanger  -> "ist-danger"

chotaButton :: DomBuilder t m => [Helper] -> Map Text Text -> m a -> m a
chotaButton helper attrs child = elAttr "a" (hclass <> attrs) child
  where
  hclass = M.fromList (fmap (\x -> ("class", encodeHelper x)) helper)

chotaButton' :: DomBuilder t m => [Helper] -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
chotaButton' helper attrs child = elAttr' "a" (hclass <> attrs) child
  where
  hclass = M.fromList (fmap (\x -> ("class", encodeHelper x)) helper)
