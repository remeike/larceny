{-# LANGUAGE OverloadedStrings          #-}

module Web.Larceny.Internal ( findTemplate
                            , parse
                            , parseWithOverrides
                            , parseJSON
                            ) where

import           Control.Exception
import           Lens.Micro
import           Control.Monad.Trans (lift)
import           Control.Monad.State (MonadState, StateT, evalStateT, runStateT, get, modify, put)
import qualified Data.HashSet        as HS
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Text.HTML.DOM       as D
import qualified Text.XML            as X
------------
import           Web.Larceny.Types
import           Web.Larceny.Fills
import           Web.Larceny.Html    (html5Nodes, html5SelfClosingNodes)
import           Web.Larceny.Svg     (svgNodes)


-- | Turn lazy text into templates.
parse :: Monad m => LT.Text -> Template s m
parse = parseWithOverrides defaultOverrides

-- | Use overrides when parsing a template.
parseWithOverrides :: Monad m => Overrides -> LT.Text -> Template s m
parseWithOverrides o t =
  let textWithoutDoctype = LT.replace "<!DOCTYPE html>" "<doctype />" t
      (X.Document _ (X.Element _ _ nodes) _) = D.parseLT ("<div>" <> textWithoutDoctype <> "</div>")
  in mk defaultOptions o $! map (toLarcenyNode o) nodes

-- | TODO
parseJSON :: Monad m => LT.Text -> Template s m
parseJSON t =
  let
    X.Document _ (X.Element _ _ nodes) _ =
      D.parseLT ("<div>" <> t <> "</div>")
  in
  mk defaultOptions { optJson = True } defaultOverrides
    $! map (toLarcenyNode defaultOverrides) nodes

-- | Phases of the template parsing/rendering process: 1. Parse the document
--     into HTML (or really, XML) nodes 2. Turn those nodes into Larceny nodes,
--     which encodes more information about the elements, including prefix and
--     whether the node is a regular HTML node, a special Larceny element, or a
--     Larceny blank. 3. Render each node into Text according to its node type.
data Node = NodeElement Element
          | NodeContent Text
          | NodeComment Text

data Element = PlainElement Name Attributes [Node]
             | ApplyElement Attributes [Node]
             | BindElement Attributes [Node]
             | BlankElement Name Attributes [Node]
             | DoctypeElement

toLarcenyName :: X.Name -> Name
toLarcenyName (X.Name tn _ _) =
  case T.stripPrefix "l:" tn of
    Just larcenyTagName -> Name (Just "l") larcenyTagName
    Nothing -> case T.stripPrefix "svg:" tn of
                 Just svgTagName -> Name (Just "svg") svgTagName
                 Nothing -> Name Nothing tn

toLarcenyNode :: Overrides -> X.Node -> Node
toLarcenyNode o (X.NodeElement (X.Element tn atr nodes)) =
  let larcenyNodes = map (toLarcenyNode o) nodes
      attrs = M.mapKeys X.nameLocalName atr
      allPlainNodes = (HS.fromList (customPlainNodes o) `HS.union` html5Nodes `HS.union` svgNodes)
                             `HS.difference` HS.fromList (overrideNodes o)in
  case toLarcenyName tn of

    -- these are our special larceny elements
    Name Nothing "bind" ->
      NodeElement (BindElement attrs larcenyNodes)
    Name Nothing "apply" ->
      NodeElement (ApplyElement attrs larcenyNodes)
    Name Nothing "apply-content" ->
      NodeElement (BlankElement (Name Nothing "apply-content") attrs larcenyNodes)
    Name Nothing "doctype" ->
      NodeElement DoctypeElement

    -- these are the blank and plain elements
    -- if it's in the "svg" prefix, it's definitely a plain node
    -- if there's an "l" prefix, it's definitely a Blank
    -- if there's not a prefix, and the tag is a member of the set of plain nodes, it's plain
    -- otherwise, it's a Blank
    Name (Just "svg") name ->
      NodeElement (PlainElement (Name (Just "svg") name) attrs larcenyNodes)
    Name (Just "l") name ->
      NodeElement (BlankElement (Name (Just "l") name) attrs larcenyNodes)
    Name pf name | HS.member name allPlainNodes ->
      NodeElement (PlainElement (Name pf name) attrs larcenyNodes)
    Name _ name ->
      NodeElement (BlankElement (Name Nothing name) attrs larcenyNodes)
toLarcenyNode _ (X.NodeContent c)  = NodeContent c
toLarcenyNode _ (X.NodeComment c) = NodeComment c
toLarcenyNode _ (X.NodeInstruction _) = NodeContent ""

-- | Turn HTML nodes and overrides into templates.
mk :: Monad m => Options -> Overrides -> [Node] -> Template s m
mk options o =
  let
    f nodes =
      Template $
        \pth m l ->
          if optJson options then
            let
              m' =
                m <> subs [("field", useAttrs (a "value") textFill)]
            in do
            s <- get

            txts <-
              toUserState (ProcessContext pth m' l o f nodes s) (process options nodes)

            case T.intercalate "," txts of
              txt | T.isPrefixOf "[" txt -> return [txt]
              txt                        -> return ["{" <> txt <> "}"]

          else do
            s <- get
            toUserState (ProcessContext pth m l o f nodes s) (process options nodes)
  in
  f


toProcessState :: Monad m => StateT s m a -> StateT (ProcessContext s m) m a
toProcessState f =
  do pc <- get
     (result, s') <- lift $ runStateT f (_pcState pc)
     pcState .= s'
     return result

toUserState :: Monad m => ProcessContext s m -> StateT (ProcessContext s m) m a -> StateT s m a
toUserState pc f =
  do s <- get
     (result, pc') <- lift $ runStateT f (pc { _pcState = s })
     put (_pcState pc')
     return result

fillIn :: Monad m => Blank -> Substitutions s m -> Fill s m
fillIn tn m = fromMaybe (fallbackFill tn m) (M.lookup tn m)

fallbackFill :: Monad m => Blank -> Substitutions s m -> Fill s m
fallbackFill FallbackBlank m =  fromMaybe (textFill "") (M.lookup FallbackBlank m)
fallbackFill (Blank tn) m =
  let fallback = fromMaybe (textFill "") (M.lookup FallbackBlank m) in
  Fill $ \attr (pth, tpl) lib ->
    -- do liftIO $ putStrLn ("Larceny: Missing fill for blank " <> show tn <> " in template " <> show pth)
    do unFill fallback attr (pth, tpl) lib

data ProcessContext s m = ProcessContext { _pcPath          :: Path
                                         , _pcSubs          :: Substitutions s m
                                         , _pcLib           :: Library s m
                                         , _pcOverrides     :: Overrides
                                         , _pcMk            :: [Node] -> Template s m
                                         , _pcNodes         :: [Node]
                                         , _pcState         :: s }

infix  4 .=
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

pcSubs :: Lens' (ProcessContext s m) (Substitutions s m)
pcSubs = lens _pcSubs (\pc s -> pc { _pcSubs = s })

pcNodes :: Lens' (ProcessContext s m) [Node]
pcNodes  = lens _pcNodes (\pc n -> pc { _pcNodes = n })

pcState :: Lens' (ProcessContext s m) s
pcState = lens _pcState (\pc s -> pc { _pcState = s })

type ProcessT s m = StateT (ProcessContext s m) m [Text]

add :: Monad m => Substitutions s m -> Template s m -> Template s m
add mouter tpl =
  Template (\pth minner l -> runTemplate tpl pth (minner `M.union` mouter) l)

process :: Monad m => Options -> [Node] -> ProcessT s m
process _ [] = return []
process _ (NodeElement (BindElement atr kids):nextNodes) = do
  pcNodes .= nextNodes
  processBind atr kids
process options (currentNode:nextNodes) = do
  pcNodes .= nextNodes
  processedNode <-
    if optJson options then
      case currentNode of
        NodeElement (BlankElement (Name _ name) atr kids) ->
          processJson name atr kids
        _ ->
          return []
    else
      case currentNode of
        NodeElement DoctypeElement  ->
          return ["<!DOCTYPE html>"]
        NodeElement (ApplyElement atr kids) ->
          processApply atr kids
        NodeElement (PlainElement tn atr kids) ->
          processPlain options tn atr kids
        NodeElement (BlankElement (Name _ name) atr kids) ->
          processBlank name atr kids
        NodeContent t ->
          return [t]
        NodeComment c ->
          return ["<!--" <> c <> "-->"]

  restOfNodes <- process options nextNodes
  return $ processedNode ++ restOfNodes

-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain :: Monad m =>
                Options ->
                Name ->
                Attributes ->
                [Node] ->
                ProcessT s m
processPlain options tagName atr kids = do
  pc <- get
  atrs <- attrsToText atr
  processed <- process options kids
  return $ tagToText (_pcOverrides pc) tagName atrs processed

selfClosing :: Overrides -> HS.HashSet Text
selfClosing (Overrides _ _ sc) =
  HS.fromList sc <> html5SelfClosingNodes

tagToText :: Overrides
          -> Name
          -> Text
          -> [Text]
          -> [Text]
tagToText overrides (Name mPf name) atrs processed =
  let prefix = fromMaybe "" ((\pf -> pf <> ":") <$> mPf) in
  if name `HS.member` selfClosing overrides
  then ["<" <> prefix <> name <> atrs <> "/>"]
  else ["<" <> prefix <> name <> atrs <> ">"]
           ++ processed
           ++ ["</" <> prefix <> name <> ">"]

attrsToText :: Monad m => Attributes -> StateT (ProcessContext s m) m Text
attrsToText attrs =
  T.concat <$> mapM attrToText (M.toList attrs)
  where attrToText (k,v) = do
          let (unboundK, unboundV) =  eUnboundAttrs (k,v)
          keys <- T.concat <$> mapM fillAttr unboundK
          vals <- T.concat <$> mapM fillAttr unboundV
          return $ toText (keys, vals)
        toText (k, "") = " " <> k
        toText (k, v) =
          if T.any (=='\"') v then
            " " <> k <> "=\'" <> T.strip v <> "\'"
          else
            " " <> k <> "=\"" <> T.strip v <> "\""

fillAttrs :: Monad m => Attributes -> StateT (ProcessContext s m) m Attributes
fillAttrs attrs =  M.fromList <$> mapM fill (M.toList attrs)
  where fill p = do
          let (unboundKeys, unboundValues) = eUnboundAttrs p
          keys <- T.concat <$> mapM fillAttr unboundKeys
          vals <- T.concat <$> mapM fillAttr unboundValues
          return (keys, vals)

fillAttr :: Monad m => Either Text Blank -> StateT (ProcessContext s m) m Text
fillAttr eBlankText = do
  ProcessContext pth m l _ mko _ _ <- get
  case eBlankText of
    Right hole@(Blank txt) | T.isInfixOf "?" txt || T.isInfixOf "->" txt ->
      fmap mconcat $ process defaultOptions $ attrPath hole

    Right hole ->
      fmap T.concat $ toProcessState $ unFill (fillIn hole m) mempty (pth, mko []) l

    Left text ->
      toProcessState $ return text

-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processJson :: Monad m =>
               Text ->
               Attributes ->
               [Node] ->
               ProcessT s m
processJson tagName atr kids = do
  (ProcessContext pth m l _ mko _ _) <- get
  filled <- fillAttrs atr

  txts <- toProcessState $ unFill (fillIn (Blank tagName) m) filled (pth, add m (mko kids)) l
  let name = fromMaybe tagName (M.lookup "name" filled)

  case txts of
    [txt] | M.member "skip" atr && T.isInfixOf "\":" txt ->
      return [T.drop 1 $ T.dropEnd 1 txt]

    [txt] | T.isInfixOf "\":" txt ->
      return ["\"" <> name <> "\":" <> txt]

    [txt] | M.member "number" atr ->
      return ["\"" <> name <> "\":" <> txt]

    [txt] | M.member "bool" atr ->
      return $
        case T.toLower txt of
          "true"  -> ["\"" <> name <> "\":true"]
          "t"     -> ["\"" <> name <> "\":true"]
          "false" -> ["\"" <> name <> "\":false"]
          "f"     -> ["\"" <> name <> "\":false"]
          _       -> ["\"" <> name <> "\":\"" <> txt <> "\""]

    [txt] ->
      return ["\"" <> name <> "\":\"" <> txt <> "\""]

    _ | M.member "skip" atr ->
      return ["[" <> (T.intercalate "," txts) <> "]"]

    _ ->
      return
        [ "[" <>
            ( T.intercalate ","
              $ fmap (\txt -> "{\"" <> name <> "\":" <> txt <> "}")
              $ txts
            ) <>
          "]"
        ]

processBlank :: Monad m =>
                Text ->
                Attributes ->
                [Node] ->
                ProcessT s m
processBlank tagName atr kids = do
  (ProcessContext pth m l _ mko _ _) <- get
  filled <- fillAttrs atr
  toProcessState $ unFill (fillIn (Blank tagName) m) filled (pth, add m (mko kids)) l

processBind :: Monad m =>
               Attributes ->
               [Node] ->
               ProcessT s m
processBind atr kids = do
  (ProcessContext pth m l _ mko nodes _) <- get
  let tagName = atr M.! "tag"
      newSubs = subs [(tagName, Fill $ \_a _t _l ->
                                       runTemplate (mko kids) pth m l)]
  pcSubs .= newSubs `M.union` m
  process defaultOptions nodes

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library.
processApply :: Monad m =>
                Attributes ->
                [Node] ->
                ProcessT s m
processApply atr kids = do
  (ProcessContext pth m l _ mko _ _) <- get
  filledAttrs <- fillAttrs atr
  let (absolutePath, tplToApply) = findTemplateFromAttrs pth l filledAttrs
  contentTpl <- fmap T.concat $ toProcessState $ runTemplate (mko kids) pth m l
  let contentSub = subs [("apply-content",
                         rawTextFill contentTpl)]
  toProcessState $ runTemplate tplToApply absolutePath (contentSub `M.union` m) l

findTemplateFromAttrs :: Monad m =>
                         Path ->
                         Library s m ->
                         Attributes ->
                         (Path, Template s m)
findTemplateFromAttrs pth l atr =
  let tplPath = T.splitOn "/" $ fromMaybe (throw $ AttrMissing "template")
                                          (M.lookup "template" atr) in
  case findTemplate l (init pth) tplPath of
    (_, Nothing) -> throw $ ApplyError tplPath pth
    (targetPath, Just tpl) -> (targetPath, tpl)

findTemplate :: Monad m => Library s m -> Path -> Path -> (Path, Maybe (Template s m))
findTemplate lib [] targetPath = (targetPath, M.lookup targetPath lib)
findTemplate lib pth' targetPath =
  case M.lookup (pth' ++ targetPath) lib of
    Just tpl -> (pth' ++ targetPath, Just tpl)
    Nothing  -> findTemplate lib (init pth') targetPath

eUnboundAttrs :: (Text, Text) -> ([Either Text Blank], [Either Text Blank])
eUnboundAttrs (name, value) = do
  let possibleWords = T.splitOn "${"
  let mWord w =
        case T.splitOn "}" w of
          [_] -> [Left w]
          ["",_] -> [Left ("${" <> w)]
          (word: rest) | T.any (=='{') word -> Left (word <> "}") : map Left rest
          (word: rest) -> Right (Blank word) : map Left rest
          _ -> [Left w]
  ( concatMap mWord (possibleWords name)
    , concatMap mWord (possibleWords value))

{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use first" :: String) #-}


attrNodes :: Blank -> [Node]
attrNodes (Blank txt) =
  case T.splitOn "?" txt of
    [tag, params] ->
      let
        attrs =
          M.fromList $
            fmap
              ( \kv ->
                case T.splitOn "=" kv of
                  [k, v] -> (k, v)
                  _      -> (kv, "")
              )
              $ T.splitOn "&" params
      in
      [NodeElement $ BlankElement (Name Nothing tag) attrs []]

    _ ->
      []


attrPath :: Blank -> [Node]
attrPath (Blank txt) =
  let
    attrNode t children =
      case T.splitOn "?" t of
        [tag] ->
          [NodeElement $ BlankElement (Name Nothing tag) mempty children]

        [tag, params] ->
          let
            attrs =
              M.fromList $
                fmap
                  ( \kv ->
                    case T.splitOn "=" kv of
                      [k, v] -> (k, v)
                      _      -> (kv, "")
                  )
                  $ T.splitOn "&" params
          in
          [NodeElement $ BlankElement (Name Nothing tag) attrs children]

        _ ->
          []
  in
  foldr attrNode [] $ T.splitOn "->" txt


data Options =
  Options
    { optJson  :: Bool
    , minified :: Bool
    }


defaultOptions :: Options
defaultOptions =
  Options
    { optJson  = False
    , minified = False
    }
