{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module Web.Larceny.Internal
  ( findTemplate
  , parse
  , parseWithSettings
  , parseXml
  , parseTemplate
  ) where

--------------------------------------------------------------------------------
import           Control.Exception
import           Lens.Micro
import           Control.Monad.Trans  ( lift )
import           Control.Monad.State  ( MonadState, StateT, runStateT
                                      , get, modify, put
                                      )
import qualified Data.Char           as Char
import qualified Data.HashSet        as HS
import qualified Data.List           as List
import qualified Data.Map            as M
import           Data.Maybe           ( fromMaybe )
import           Data.Text            ( Text )
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Text.HTML.DOM       as D
import qualified Text.XML            as X
--------------------------------------------------------------------------------
import           Web.Larceny.Output   ( toText )
import           Web.Larceny.Types
import           Web.Larceny.Fills
import           Web.Larceny.Html     ( html5Nodes, html5SelfClosingNodes )
import           Web.Larceny.Svg      ( svgNodes )
--------------------------------------------------------------------------------


-- | Turn lazy text into templates.
parse :: Monad m => LT.Text -> Template s m
parse = parseWithSettings defaultSettings


-- | Use settings when parsing a template.
parseWithSettings :: Monad m => Settings m -> LT.Text -> Template s m
parseWithSettings settings t =
  parseTemplate settings (parseXml settings t)


-- | Parse lazy text into XML nodes with settings.
parseXml :: Monad m => Settings m -> LT.Text -> [X.Node]
parseXml settings t =
  let
    textWithoutDoctype =
      LT.replace "<!DOCTYPE html>" "<doctype />"
        $ if setTrimWhitespace settings then trimWhitespace t else t

    (X.Document _ (X.Element _ _ nodes) _) =
      D.parseLT ("<div>" <> textWithoutDoctype <> "</div>")

  in
  expandElements nodes


-- | Parse XML nodes into template.
parseTemplate :: Monad m => Settings m -> [X.Node] -> Template s m
parseTemplate settings nodes =
  mk settings $! map (toLarcenyNode settings) nodes



-- | Phases of the template parsing/rendering process: 1. Parse the document
-- into HTML (or really, XML) nodes 2. Turn those nodes into Larceny nodes,
-- which encodes more information about the elements, including prefix and
-- whether the node is a regular HTML node, a special Larceny element, or a
-- Larceny blank. 3. Render each node into Text according to its node type.
data Node
  = NodeElement Element
  | NodeContent Text
  | NodeComment Text
  deriving Show


data Element
  = PlainElement Name Attributes [Node]
  | ApplyElement Attributes [Node]
  | BindElement Attributes [Node]
  | BlankElement Name Attributes [Node]
  | DoctypeElement
  deriving Show


toLarcenyName :: X.Name -> Name
toLarcenyName (X.Name tn _ _) =
  case T.stripPrefix "l:" tn of
    Just larcenyTagName ->
      Name (Just "l") larcenyTagName

    Nothing ->
      case T.stripPrefix "svg:" tn of
        Just svgTagName ->
          Name (Just "svg") svgTagName

        Nothing ->
          case T.stripPrefix "x:" tn of
            Just xmlTagName -> Name (Just "x") xmlTagName
            Nothing         -> Name Nothing tn


toLarcenyNode :: Settings m -> X.Node -> Node
toLarcenyNode settings node =
  case node of
    X.NodeContent _ | setIgnoreContent settings ->
      NodeContent ""

    X.NodeContent c ->
      NodeContent c

    X.NodeComment _ | setIgnoreComments settings ->
      NodeContent ""

    X.NodeComment c ->
      NodeComment c

    X.NodeInstruction _ ->
      NodeContent ""

    X.NodeElement (X.Element tn atr nodes) ->
      let
        larcenyNodes =
          map (toLarcenyNode settings) nodes

        attrs =
          M.mapKeys X.nameLocalName atr

        allPlainNodes =
          ( HS.fromList (customPlainNodes $ setOverrides settings)
              `HS.union` html5Nodes
              `HS.union` svgNodes
          ) `HS.difference` HS.fromList (overrideNodes $ setOverrides settings)
      in
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

        Name (Just "x") name ->
          NodeElement (PlainElement (Name Nothing name) attrs larcenyNodes)

        Name (Just "l") name ->
          NodeElement (BlankElement (Name (Just "l") name) attrs larcenyNodes)

        Name pf name | HS.member name allPlainNodes && not (setIgnoreHtml settings) ->
          NodeElement (PlainElement (Name pf name) attrs larcenyNodes)

        Name _ name ->
          NodeElement (BlankElement (Name Nothing name) attrs larcenyNodes)


-- | Turn HTML nodes and overrides into templates.
mk :: Monad m => Settings m -> [Node] -> Template s m
mk settings =
  let
    f nodes =
      Template $
        \pth m l ->
          let
            splices =
              subs [("h:fragment", fillChildren)] <> m <> jsonSplices

            pc =
              ProcessContext pth splices l (setOverrides settings) f nodes
          in do
          s <- get
          (ls, _) <- toUserState (pc s) (process settings nodes)

          case ls of
            [node] -> return node
            _      -> return $ ListOutput ls
  in
  f


toProcessState :: Monad m => StateT s m Output -> StateT (ProcessContext s m) m (Output, Bool)
toProcessState f = do
  pc <- get
  (result, s') <- lift $ runStateT f (_pcState pc)
  pcState .= s'

  case result of
    FragmentOutput _ -> return (result, True)
    _                -> return (result, False)


toUserState ::
  Monad m =>
  ProcessContext s m -> StateT (ProcessContext s m) m a -> StateT s m a
toUserState pc f = do
  s <- get
  (result, pc') <- lift $ runStateT f (pc { _pcState = s })
  put (_pcState pc')
  return result


fillIn :: Monad m => Settings m -> Blank -> Substitutions s m -> Fill s m
fillIn settings tn m =
  fromMaybe (fallbackFill settings tn m) (M.lookup tn m)


fallbackFill :: Monad m => Settings m -> Blank -> Substitutions s m -> Fill s m
fallbackFill settings blank splices =
  case blank of
    FallbackBlank ->
      fromMaybe (textFill "") $ M.lookup FallbackBlank splices

    Blank tn ->
      Fill $ \attr (pth, tpl) lib ->
        let
          message =
            T.pack $
              "Larceny: Missing fill for blank "
                <> show tn
                <> " in template "
                <> show pth

          fallback =
            case M.lookup "def" attr of
              Just txt ->
                rawTextFill txt

              Nothing ->
                fromMaybe
                  ( if setDebugComments settings then
                      commentFill message
                    else
                      voidFill
                  )
                  ( M.lookup FallbackBlank splices )
        in do
        lift $ setDebugLogger settings $ message
        unFill fallback attr (pth, tpl) lib


data ProcessContext s m =
  ProcessContext
    { _pcPath      :: Path
    , _pcSubs      :: Substitutions s m
    , _pcLib       :: Library s m
    , _pcOverrides :: Overrides
    , _pcMk        :: [Node] -> Template s m
    , _pcNodes     :: [Node]
    , _pcState     :: s
    }


infix  4 .=
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}


pcSubs :: Lens' (ProcessContext s m) (Substitutions s m)
pcSubs = lens _pcSubs (\pc s -> pc { _pcSubs = s })


pcNodes :: Lens' (ProcessContext s m) [Node]
pcNodes = lens _pcNodes (\pc n -> pc { _pcNodes = n })


pcState :: Lens' (ProcessContext s m) s
pcState = lens _pcState (\pc s -> pc { _pcState = s })


type ProcessT s m =
  StateT (ProcessContext s m) m ([Output], Bool)


add :: Monad m => Substitutions s m -> Template s m -> Template s m
add mouter tpl =
  Template (\pth minner l -> runTemplate tpl pth (minner `M.union` mouter) l)


process :: Monad m => Settings m -> [Node] -> ProcessT s m
process settings nodes =
  case nodes of
    [] ->
      return ([], False)

    NodeElement (BindElement atr kids) : nextNodes -> do
      pcNodes .= nextNodes
      processBind settings atr kids

    currentNode : nextNodes -> do
      pcNodes .= nextNodes

      (processedNode, bubble) <-
        case currentNode of
          NodeElement DoctypeElement  ->
            return ([HtmlDocType], False)

          NodeElement (ApplyElement atr kids) ->
            processApply settings atr kids

          NodeElement (PlainElement tn atr kids) ->
            processPlain settings tn atr kids

          NodeElement (BlankElement (Name _ "h:fragment") atr kids) -> do
            processBlankFragment settings "h:fragment" atr kids

          NodeElement (BlankElement (Name _ name) atr kids) ->
            processBlank settings name atr kids

          NodeContent t ->
            return ([TextOutput t], False)

          NodeComment c ->
            return ([CommentOutput c], False)

      if bubble then
        return (processedNode, bubble)

      else do
        (restOfNodes, bubble') <- process settings nextNodes

        if bubble' then
          return (restOfNodes, True)
        else
          return (processedNode <> restOfNodes, False)


-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain ::
  Monad m => Settings m -> Name -> Attributes -> [Node] -> ProcessT s m
processPlain settings tagName atr kids = do
  pc <- get
  atrs <- processAttrs settings atr
  (processed, bubble) <- process settings kids

  if bubble then
    return (processed, bubble)
  else
    return (elemOutput (_pcOverrides pc) tagName atrs processed, bubble)


selfClosing :: Overrides -> HS.HashSet Text
selfClosing (Overrides _ _ sc) =
  HS.fromList sc <> html5SelfClosingNodes


elemOutput :: Overrides -> Name -> Attributes -> [Output] -> [Output]
elemOutput overrides (Name mPf name) atrs processed =
  let
    prefix =
      fromMaybe "" ((\pf -> pf <> ":") <$> mPf)
  in
  if name `HS.member` selfClosing overrides
    then [LeafOutput (prefix <> name) atrs ]
    else [ElemOutput (prefix <> name) atrs processed]


processAttrs ::
  Monad m => Settings m -> Attributes -> StateT (ProcessContext s m) m Attributes
processAttrs settings attrs =
  let
    attrToText (k,v) =
      let
        (unboundK, unboundV) =  eUnboundAttrs (k,v)
      in do
      keys <- T.concat <$> mapM (fillAttr settings) unboundK
      vals <- T.concat <$> mapM (fillAttr settings) unboundV
      return [(keys, vals)]
  in
  M.fromList . mconcat <$> mapM attrToText (M.toList attrs)


fillAttrs ::
  Monad m =>
  Settings m -> Attributes -> StateT (ProcessContext s m) m Attributes
fillAttrs settings attrs =
  let
    fill p = do
      let (unboundKeys, unboundValues) = eUnboundAttrs p
      keys <- T.concat <$> mapM (fillAttr settings) unboundKeys
      vals <- T.concat <$> mapM (fillAttr settings) unboundValues
      return (keys, vals)
  in
  M.fromList <$> mapM fill (M.toList attrs)


fillAttr ::
  Monad m =>
  Settings m -> Either Text Blank -> StateT (ProcessContext s m) m Text
fillAttr settings eBlankText = do
  ProcessContext pth m l _ mko _ _ <- get

  case eBlankText of
    Right hole@(Blank txt) | T.isInfixOf "?" txt || T.isInfixOf "." txt -> do
      (ls, _) <- process settings $ attrPath hole
      return $ T.concat $ fmap toText ls

    Right hole ->
      fmap (toText . fst)
        $ toProcessState
        $ unFill (fillIn settings hole m) mempty (pth, mko []) l

    Left text ->
      fmap (toText . fst)
        $ toProcessState
        $ return
        $ TextOutput text


-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processBlank ::
  Monad m => Settings m -> Text -> Attributes -> [Node] -> ProcessT s m
processBlank settings tagName atr kids = do
  (ProcessContext pth m l _ mko _ _) <- get
  filled <- fillAttrs settings atr
  (output, bubble) <-
    toProcessState
      $ unFill (fillIn settings (Blank tagName) m) filled (pth, add m (mko kids)) l
  return ([output], bubble)


-- Same as `processBlank` but checking the attributes to determine whether it
-- should be bubbled up.
processBlankFragment ::
  Monad m => Settings m -> Text -> Attributes -> [Node] -> ProcessT s m
processBlankFragment settings tagName atr kids = do
  (ProcessContext pth m l _ mko _ _) <- get
  filled <- fillAttrs settings atr

  (output, bubble) <-
    toProcessState
      $ unFill (fillIn settings (Blank tagName) m) filled (pth, add m (mko kids)) l

  if bubble then
    return ([output], bubble)
  else
    case M.lookup "condition" filled of
      Just "True" ->
        return ([FragmentOutput [output]], True)

      _ ->
        case (M.lookup "key" filled, M.lookup "match" filled) of
          (Just k, Just v) | k == v ->
            return ([FragmentOutput [output]], True)

          _ ->
            return ([output], False)


processBind ::
  Monad m => Settings m -> Attributes -> [Node] -> ProcessT s m
processBind settings atrs kids = do
  atr <- fillAttrs settings atrs

  (ProcessContext pth m l _ mko nodes _) <- get

  let
    tagName =
      atr M.! "tag"

    defArgs =
      M.mapKeys (\k -> Blank $ "arg:" <> k)
        $ M.map rawTextFill
        $ M.filterWithKey (\k _ -> k /= "tag") atr

    newSubs =
      case M.lookup "assign" atr of
        Just k ->
          case M.lookup (Blank k) m of
            Nothing ->
              mempty

            Just fill ->
              subs
                [ ( tagName
                  , fill
                  )
                ]

        Nothing ->
          subs
            [ ( tagName
              , Fill $ \atr' _t _l ->
                  let
                    args =
                      M.mapKeys (\k -> Blank $ "arg:" <> k)
                        $ M.map rawTextFill atr'
                  in
                  runTemplate (mko kids) pth (args <> defArgs <> m) l
              )
            ]

  pcSubs .= newSubs `M.union` m
  process settings nodes


-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library.
processApply :: Monad m => Settings m -> Attributes -> [Node] -> ProcessT s m
processApply settings atr kids = do
  (ProcessContext pth m l _ mko _ _) <- get
  filledAttrs <- fillAttrs settings atr
  let (absolutePath, tplToApply) = findTemplateFromAttrs pth l filledAttrs
  (contentTpl, bubble) <- toProcessState $ runTemplate (mko kids) pth m l

  if bubble then
    return ([contentTpl], bubble)
  else
    let
      contentSub =
        subs [("apply-content", outputFill contentTpl)]
    in do
    (output, bubble') <-
      toProcessState
        $ runTemplate tplToApply absolutePath (contentSub `M.union` m) l
    return ([output], bubble')


findTemplateFromAttrs ::
  Monad m => Path -> Library s m -> Attributes -> (Path, Template s m)
findTemplateFromAttrs pth l atr =
  let
    tplPath =
      T.splitOn "/"
        $ fromMaybe (throw $ AttrMissing "template")
        $ M.lookup "template" atr
  in
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


attrPath :: Blank -> [Node]
attrPath FallbackBlank = []
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
  foldr attrNode [] $ T.splitOn "." txt


trimWhitespace :: LT.Text -> LT.Text
trimWhitespace txt =
  snd $
    List.foldl'
      ( \(prev, acc) t ->
        case LT.strip t of
            -- Start of <pre> tags
            t' | LT.isPrefixOf "<pre>" t' ->
              (t', acc <> LT.stripStart t)
            t' | LT.isPrefixOf "<pre " t' ->
              (t', acc <> LT.stripStart t)
            -- End of <pre> tags
            t' | LT.isPrefixOf "</pre>" t ->
              (t', acc <> "\n" <> LT.stripEnd t)
            -- Inside <pre> tags
            _ | LT.isPrefixOf "<pre>" prev ->
              (prev, acc <> "\n" <> t)
            _ | LT.isPrefixOf "<pre " prev ->
              (prev, acc <> "\n" <> t)

            --
            t' | LT.isPrefixOf "<" t' ->
              (t', acc <> trimInnerWhitespace t')

            t' | LT.isSuffixOf ">" prev ->
              (t', acc <> trimInnerWhitespace t')

            t' ->
              (t', acc <> " " <> trimInnerWhitespace t')
      )
      ("", "")
      (LT.lines txt)


trimInnerWhitespace :: LT.Text -> LT.Text
trimInnerWhitespace txt =
  snd $
    LT.foldr
      ( \char (whitespace, acc) ->
          if Char.isSpace char && whitespace then
            (True, acc)
          else
            (Char.isSpace char, LT.cons char acc)

      )
      (False, "")
      txt


jsonSplices :: Monad m => Substitutions s m
jsonSplices =
  subs
    [ ("j:object", objectFill)
    , ("j:array", arrayFill)
    , ("j:value", leafFill "j:value")
    ]


objectSplices :: Monad m => Substitutions s m
objectSplices =
  subs
    [ ("j:number", leafFill "j:number")
    , ("j:bool", leafFill "j:bool")
    , ("j:string", leafFill "j:string")
    , ("j:field", leafFill "j:field")
    , ("j:object", objectFill)
    ]


objectFill :: Monad m => Fill s m
objectFill =
  Fill $ \attrs (pth, tpl) lib -> do
    ctxt <- get
    (op, ctxt') <- lift $ runStateT (runTemplate tpl pth objectSplices lib) ctxt
    put ctxt'
    return $ ElemOutput "j:object" attrs [op]


arrayFill :: Monad m => Fill s m
arrayFill =
  Fill $ \attrs (pth, tpl) lib -> do
    ctxt <- get
    (op, ctxt') <- lift $ runStateT (runTemplate tpl pth jsonSplices lib) ctxt
    put ctxt'
    return $ ElemOutput "j:array" attrs [op]


expandElements :: [X.Node] -> [X.Node]
expandElements =
  fmap
    ( \node ->
        case node of
          X.NodeElement (X.Element (X.Name name n p) attrs nodes) ->
            case T.splitOn "." name of
              name' : rest@(_ : _) ->
                X.NodeElement $ X.Element (X.Name name' n p) mempty $
                  List.foldr
                    ( \tag children ->
                        [ X.NodeElement
                            $ X.Element (X.Name tag Nothing Nothing) attrs
                            $ children
                        ]
                    )
                    ( expandElements nodes )
                    rest

              _ ->
                X.NodeElement
                  $ X.Element (X.Name name n p) attrs
                  $ expandElements nodes

          _ ->
            node
    )
