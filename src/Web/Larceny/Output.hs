{-# LANGUAGE OverloadedStrings #-}


module Web.Larceny.Output
  ( Output(..)
  , toHtml
  , toMarkup
  , toText
  ) where

--------------------------------------------------------------------------------
import qualified Data.Map                 as M
import           Data.Text                 ( Text )
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import           Text.Blaze                ( Markup )
import qualified Text.Blaze               as Blaze
import           Text.Blaze.Internal
import qualified Text.Blaze.Renderer.Text as Blaze
import           Text.XML
import qualified Text.XML                 as Xml
--------------------------------------------------------------------------------
import           Web.Larceny.Types         ( Output(..) )
--------------------------------------------------------------------------------



toHtml :: Output -> Text
toHtml output =
  T.replace "=\"{" "='{"
    $ T.replace "}\"" "}'"
    $ T.replace "=\"[" "='["
    $ T.replace "]\"" "]'"
    $ T.replace "=\"\"" ""
    $ LT.toStrict
    $ Blaze.renderMarkup
    $ toMarkup output


toMarkup :: Output -> Markup
toMarkup output =
  case output of
    LeafOutput name attrs ->
      foldr
        ( \(k,v) node -> node ! customAttribute (textTag k) (textValue v))
        ( customLeaf (textTag name) True )
        ( M.toList attrs )

    ElemOutput name attrs ls ->
      foldr
        ( \(k,v) node -> node ! customAttribute (textTag k) (preEscapedTextValue v))
        ( customParent (textTag name) $ foldMap toMarkup ls )
        ( M.toList attrs )

    TextOutput txt ->
      Blaze.preEscapedText txt

    RawTextOutput txt ->
      Blaze.preEscapedText txt

    CommentOutput txt ->
      Blaze.textComment txt

    ListOutput ls ->
      foldMap toMarkup ls

    HtmlDocType ->
      preEscapedText "<!DOCTYPE html>"


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
      [ NodeContent txt ]

    CommentOutput txt ->
      [ NodeComment txt ]

    HtmlDocType ->
      []



toText :: Output -> Text
toText output =
  case output of
    LeafOutput _ _    -> ""
    ElemOutput _ _ ls -> foldMap toText ls
    TextOutput txt    -> txt
    ListOutput ls     -> foldMap toText ls
    RawTextOutput txt -> txt
    CommentOutput txt -> txt
    HtmlDocType       -> ""
