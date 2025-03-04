{-# LANGUAGE OverloadedStrings #-}


module Web.Larceny.Output
  ( Output(..)
  , toHtml
  , toMarkup
  , toXml
  , toJson
  , toText
  ) where

--------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Key           as Key
import           Data.Aeson.Types          ( Pair )
import           Data.Maybe                ( fromMaybe, listToMaybe )
import qualified Data.Map                 as M
import           Data.Scientific           ( Scientific )
import           Data.Text                 ( Text )
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Lazy           as LT
import           Text.Blaze                ( Markup, (!) )
import qualified Text.Blaze               as Blaze
import           Text.Blaze.Internal       ( customLeaf, customParent )
import qualified Text.Blaze.Renderer.Text as Blaze
import qualified Text.HTML.DOM            as Html
import           Text.XML
import qualified Text.XML                 as Xml
--------------------------------------------------------------------------------
import           Web.Larceny.Types         ( Attributes, Output(..) )
--------------------------------------------------------------------------------



toHtml :: Output -> Text
toHtml output =
  T.replace "=\"\"" ""
    $ LT.toStrict
    $ Blaze.renderMarkup
    $ toMarkup output


toMarkup :: Output -> Markup
toMarkup output =
  case output of
    LeafOutput name attrs ->
      foldr
        ( \(k,v) node -> node ! Blaze.customAttribute (Blaze.textTag k) (Blaze.textValue v))
        ( customLeaf (Blaze.textTag name) True )
        ( M.toList attrs )

    ElemOutput name attrs ls ->
      foldr
        ( \(k,v) node -> node ! Blaze.customAttribute (Blaze.textTag k) (Blaze.preEscapedTextValue v))
        ( customParent (Blaze.textTag name) $ foldMap toMarkup ls )
        ( M.toList attrs )

    TextOutput txt ->
      Blaze.preEscapedText txt

    RawTextOutput txt ->
      Blaze.preEscapedText txt

    RawJsonOutput _ ->
      mempty

    CommentOutput txt ->
      Blaze.textComment txt

    ListOutput ls ->
      foldMap toMarkup ls

    BubbleOutput ls ->
      foldMap toMarkup ls

    HtmlDocType ->
      Blaze.preEscapedText "<!DOCTYPE html>"

    VoidOutput ->
      mempty

    ShortOutput out ->
      toMarkup out

    DelayedOutput _ _ ->
      mempty


toXml :: Output -> [Node]
toXml output =
  let
    toAttrs =
      M.mapKeys (\name -> Name name Nothing Nothing)
  in
  case output of
    LeafOutput name attrs ->
      [ NodeElement
          $ Element (Name name Nothing Nothing) (toAttrs attrs) []
      ]

    ElemOutput name attrs ls ->
      [ NodeElement
          $ Element (Name name Nothing Nothing) (toAttrs attrs)
          $ foldMap toXml ls
      ]

    TextOutput txt ->
      [ NodeContent txt ]

    RawTextOutput txt ->
      Xml.elementNodes
        $ Xml.documentRoot
        $ Html.parseLT ("<div>" <> LT.fromStrict txt <> "</div>")

    RawJsonOutput _ ->
      []

    CommentOutput txt ->
      [ NodeComment txt ]

    ListOutput ls ->
      foldMap toXml ls

    BubbleOutput ls ->
      foldMap toXml ls

    HtmlDocType ->
      []

    VoidOutput ->
      []

    ShortOutput out ->
      toXml out

    DelayedOutput _ _ ->
      []


toJson :: Output -> Value
toJson output =
  case toJsonValue output of
    [val] -> val
    ls    -> toJSON ls


toJsonValue :: Output -> [Value]
toJsonValue output =
  case output of
    ElemOutput "j:object" _ ls ->
      [object $ foldMap toJsonPairs ls]

    ElemOutput "j:array" _ ls ->
      foldMap toJsonValue ls

    LeafOutput "j:value" attrs ->
      pure $
        case M.lookup "number" attrs of
          Just number ->
            numberValue attrs number

          Nothing ->
            case M.lookup "bool" attrs of
              Just bool ->
                boolValue attrs bool

              Nothing ->
                case M.lookup "string" attrs of
                  Just string ->
                    Aeson.String string

                  Nothing ->
                    fromMaybe Null $ fmap fieldValue $ M.lookup "field" attrs

    ListOutput ls ->
      foldMap toJsonValue ls

    BubbleOutput ls ->
      foldMap toJsonValue ls

    RawJsonOutput val ->
      [val]

    _ ->
      []


toJsonPairs :: Output -> [Pair]
toJsonPairs output =
  case output of
    ElemOutput "j:array" attrs ls ->
      case M.toList attrs of
        (key, _) : _ -> [Key.fromText key .= foldMap toJsonValue ls]
        _            -> []

    ElemOutput "j:object" attrs ls ->
      case M.toList attrs of
        (key, "") : _ ->
          [Key.fromText key .= object (foldMap toJsonPairs ls)]

        (key, val) : _ ->
          case Aeson.decodeStrict $ TE.encodeUtf8 val of
            Just json -> [Key.fromText key .= (json :: Value) ]
            Nothing   -> []

        _ ->
          []

    LeafOutput "j:number" attrs ->
      case filter (\(k, _) -> k /= "j:def") $ M.toList attrs of
        (key, val) : _ -> [Key.fromText key .= numberValue attrs val]
        _              -> []

    LeafOutput "j:bool" attrs ->
      case filter (\(k, _) -> k /= "j:def") $ M.toList attrs of
        (key, val) : _ -> [Key.fromText key .= boolValue attrs val]
        _              -> []

    LeafOutput "j:field" attrs ->
      case M.toList attrs of
        (key, val) : _ -> [Key.fromText key .= fieldValue val]
        _              -> []

    LeafOutput "j:string" attrs ->
      case M.toList attrs of
        (key, val) : _ -> [Key.fromText key .= val]
        _              -> []

    ListOutput ls ->
      foldMap toJsonPairs ls

    BubbleOutput ls ->
      foldMap toJsonPairs ls

    _ ->
      []


numberValue :: Attributes -> Text -> Value
numberValue attrs txt =
  case readMaybe txt :: Maybe Scientific of
    Just val ->
      Number val

    Nothing ->
      case M.lookup "j:def" attrs of
        Nothing       -> Null
        Just fallback -> fieldValue fallback


boolValue :: Attributes -> Text -> Value
boolValue attrs txt =
  case txt of
    "True"  -> Bool True
    "False" -> Bool False
    "true"  -> Bool True
    "false" -> Bool False
    _       ->
      case M.lookup "j:def" attrs of
        Nothing       -> Null
        Just fallback -> fieldValue fallback


fieldValue :: Text -> Value
fieldValue val =
  case val of
    "True"  -> Bool True
    "False" -> Bool False
    "true"  -> Bool True
    "false" -> Bool False
    "null"  -> Null
    _       ->
      case readMaybe val :: Maybe Scientific of
        Just n  -> Number n
        Nothing -> Aeson.String val


toText :: Output -> Text
toText output =
  case output of
    LeafOutput _ _    -> ""
    ElemOutput _ _ ls -> foldMap toText ls
    TextOutput txt    -> txt
    ListOutput ls     -> foldMap toText ls
    BubbleOutput ls   -> foldMap toText ls
    RawTextOutput txt -> txt
    RawJsonOutput _   -> ""
    CommentOutput txt -> txt
    HtmlDocType       -> ""
    VoidOutput        -> ""
    ShortOutput out   -> toText out
    DelayedOutput _ _ -> ""


readMaybe :: Read a => Text -> Maybe a
readMaybe =
  fmap fst . listToMaybe . reads . T.unpack
