{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Workspaces where

-- module Workspaces
--     ( getWorkspaces
--     , filterWS
--     , workspaceShift
--     , workspaceSort
--     , getPPInfo
--     , PPWS
--     , PPInfo (..)
--     , Workspace (..)
--     , Tweaks (..)
--     ) where

import Control.Monad
import Control.Monad.List
import Control.Monad.Writer
import Data.Monoid
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import System.Environment (getEnvironment)
import System.Directory (getDirectoryContents)
import System.FilePath

import XMonad
import XMonad.Actions.TopicSpace
import XMonad.Hooks.ManageHelpers

import SortWindows
import Utils

import qualified Data.Map as M

data Tweaks = Tweaks
    { mainWidth  :: Rational
    , imWidth    :: Rational
    , imGrid     :: Double
    , masterN    :: Int
    }

defaultTweaks = Tweaks
    { mainWidth = 1/2
    , imWidth   = 1/5
    , imGrid    = 2/3
    , masterN   = 2
    }

class Workspace a where
    action :: a -> X ()
    rules  :: a -> [Query Bool]
    rules _ = []

data Tag = forall a. (Workspace a) => Tag a
         | forall a. (Workspace a) => a :> [Query Bool]

instance Workspace Tag where
    action (Tag  a) = action a
    action (a :> _) = action a

    rules (Tag  a) = rules a
    rules (_ :> r) = r

type WSGenT = WriterT [(WorkspaceId, Tag)]

tag1 :: (MonadWriter [(WorkspaceId, Tag)] m, Workspace w) => String -> w -> m ()
tag1 n t = tell [(n, Tag t)]

tag :: MonadWriter [(WorkspaceId, Tag)] m => String -> Tag -> m ()
tag  n t = tell [(n, t)]

data Resources = Resources
    { layoutIcon    :: String -> FilePath
    , workspaceData :: String -> Maybe (Int, Maybe FilePath)
    }

tagSet :: [(WorkspaceId, Tag)] -> [WorkspaceId]
tagSet = map fst

buildTags :: MonadIO m => WSGenT m () -> m [(WorkspaceId, Tag)]
buildTags = execWriterT

mkResources :: MonadIO m => [(WorkspaceId, Tag)] -> m Resources
mkResources info = do
    root <- liftM (</> ".xmonad/") $ liftIO getHome
    return Resources
        { layoutIcon    = (root </>) . ("icons/" </>) . ("layout-" ++) . (<.> ".xbm")
        , workspaceData = (`M.lookup` wsData root info)
        }
  where
    wsData r info = M.fromList [ (x, (i, f r x)) | (i, x) <- zip [1..] $ map fst info ]
    f root = Just . (root </>) . ("icons/" </>) . (<.> ".xbm")

workspaceShift :: [(WorkspaceId, Tag)] -> ManageHook
workspaceShift = foldr (\(w, t) -> (composeAll [ r --> doShift w | r <- rules t ] <+>)) idHook

workspaceSort :: WorkspaceId -> [(WorkspaceId, Tag)] -> Query Any
workspaceSort w ws = composeAs Any . fromMaybe [] $ rules <$> lookup w ws
