{-# LANGUAGE OverloadedStrings #-}

{-|

A long description with example use with Scotty.

It'll be something like:

Write some templates.

Here's _post_base.tpl:
@
<html>
  <head><title>Post: <pageTitle /></title></head>
  <body>
    <header>
      <h1><pageTitle /></h1>
    </header>

    <apply-content />

  </body>
</html>
@

And post.tpl:
@
<apply template="_base">
  <bind tag="pageTitle"><someTitle /></bind>
  <h
</apply>
@

Add a `Library` to your Scotty state with `loadTemplates`.

Write some `Substitutions`.

Write some Larceny/Scotty glue.

Use `render` and `renderWith` in your Scotty handlers.

Here's a `larcenyServe`.

Admire your lovely app!!

-}

module Web.Larceny
  ( Blank(..)
  , Fill(..)
  , Attributes
  , Name(..)
  , Substitutions
  , Template(..)
  , Path
  , Library
  , Overrides(..)
  , defaultOverrides
  , render
  , renderWith
  , renderRelative
  , loadTemplates
  , getAllTemplates
  , subs
  , fallbackSub
  , textFill
  , textFill'
  , rawTextFill
  , rawTextFill'
  , outputFill
  , outputFill'
  , shortFill
  , mapSubs
  , mapSubs'
  , leafFill
  , voidFill
  , fillChildren
  , fillChildrenWith
  , fillChildrenWith'
  , maybeFillChildrenWith
  , maybeFillChildrenWith'
  , ifFill
  , useAttrs
  , FromAttribute(..)
  , AttrError(..)
  , ApplyError(..)
  , a
  , (%)
  , parse
  , parseWithSettings
  , parseXml
  , parseTemplate
  , Settings(..)
  , defaultSettings
  , Output(..)
  , toHtml
  , toMarkup
  , toXml
  , toJson
  , toText
  ) where

--------------------------------------------------------------------------------
import           Control.Monad           ( filterM )
import           Control.Monad.IO.Class  ( MonadIO(..) )
import           Control.Monad.State     ( evalStateT )
import qualified Data.Map               as M
import           Data.Text               ( Text )
import qualified Data.Text              as T
import qualified Data.Text.IO           as ST
import qualified Data.Text.Lazy         as LT
import           System.Directory        ( doesDirectoryExist, listDirectory )
import           System.FilePath         ( dropExtension, takeExtension )
--------------------------------------------------------------------------------
import           Web.Larceny.Fills
import           Web.Larceny.Internal
import           Web.Larceny.Output
import           Web.Larceny.Types
--------------------------------------------------------------------------------


-- | Render a template from the library by path.
--
-- @
-- render appTemplates appState ["path", "to", "template"]
-- @
render :: Monad m => Library s m -> s -> Path -> m (Maybe Text)
render l = renderWith l mempty


-- | Render a template from the library by path, with some additional
-- substitutions.
--
-- @
-- renderWith appTemplates extraSubs appState ["path", "to", "template"]
-- @
renderWith ::
  Monad m => Library s m -> Substitutions s m -> s -> Path -> m (Maybe Text)
renderWith l sub s = renderRelative l sub s []


-- | Render a template found relative to current template's path.
--
-- This will attempt to find the target template starting at the same
-- level as the given path, then will traverse up the directory tree
-- until it finds a template with the target path.
--
-- For example: Given these templates: ["current"], ["current",
-- "dashboard"], ["current", "private", "dashboard"], ["private",
-- "dashboard"], `renderRelative` called with a given path of
-- ["current"] and target path of ["private", "dashboard"] will find
-- ["current", "private", "dashboard"]. If there /wasn't/ a ["current",
-- "private", "dashboard"], it would render ["private", "dashboard"].
renderRelative ::
  Monad m =>
  Library s m -> Substitutions s m -> s -> Path -> Path -> m (Maybe Text)
renderRelative l sub s givenPath targetPath =
  case findTemplate l givenPath targetPath of
    (pth, Just (Template run)) ->
      Just . toHtml . fst <$> evalStateT (run pth sub l) s

    (_, Nothing) ->
      return Nothing


-- | Load all the templates in some directory into a Library.

loadTemplates :: MonadIO m => FilePath -> Settings m -> m (Library s m)
loadTemplates path settings =
  let
    mkPath p =
      T.splitOn "/" $ T.pack $ dropExtension p
  in do
  tpls <- liftIO $ getAllTemplates path

  lib <-
    M.fromList <$>
      mapM
        ( \file -> do
            content <- liftIO $ ST.readFile (path <> "/" <> file)
            return
              ( mkPath file
              , parseXml settings (LT.fromStrict content)
              )
        ) tpls

  case setPreprocessor settings of
    Nothing ->
      return $ fmap (parseTemplate settings) lib

    Just preprocess -> do
      lib' <- preprocess lib
      return $ fmap (parseTemplate settings) lib'


getAllTemplates :: FilePath -> IO [FilePath]
getAllTemplates path = do
  cExist <- doesDirectoryExist path
  cs <- if cExist then listDirectory path else return []
  let tpls = filter ((== ".tpl") . takeExtension) cs
  dirs <- filterM (doesDirectoryExist . (\d -> path <> "/" <> d)) cs
  rs <-
    mapM
      ( \dir -> do
          r <- getAllTemplates (path <> "/" <> dir)
          return $ map (\p -> dir <> "/" <> p) r
      ) dirs
  return $ tpls ++ concat rs
