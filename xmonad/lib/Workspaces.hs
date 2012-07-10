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
import Control.Monad.Writer
import Data.Monoid
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.SortWindows
import qualified XMonad.StackSet as W

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

class WorkspaceI a where
    action :: a -> X () -> X ()
    action = const id
    rules  :: a -> [Query Bool]
    rules = const []

data Tag = forall a. (WorkspaceI a) => Tag a
         | forall a. (WorkspaceI a) => a :> [Query Bool]

instance WorkspaceI Tag where
    action (Tag  a) = action a
    action (a :> _) = action a

    rules (Tag  a) = rules a
    rules (_ :> r) = r

type WSGenT = WriterT [(WorkspaceId, Tag)]

tag1 :: (MonadWriter [(WorkspaceId, Tag)] m, WorkspaceI w) => String -> w -> m ()
tag1 n t = tell [(n, Tag t)]

tag :: MonadWriter [(WorkspaceId, Tag)] m => String -> Tag -> m ()
tag  n t = tell [(n, t)]

tagSet :: [(WorkspaceId, Tag)] -> [WorkspaceId]
tagSet = map fst

buildTags :: MonadIO m => WSGenT m () -> m [(WorkspaceId, Tag)]
buildTags = execWriterT

workspaceShift :: [(WorkspaceId, Tag)] -> ManageHook
workspaceShift = foldr (\(w, t) -> (composeAll [ r --> doShift w | r <- rules t ] <+>)) idHook

workspaceSort :: WorkspaceId -> [(WorkspaceId, Tag)] -> Query Any
workspaceSort w ws = composeAs Any . fromMaybe [] $ rules <$> lookup w ws

workspaceAction :: WorkspaceId -> X () -> [(WorkspaceId, Tag)] -> X ()
workspaceAction w x ws = fromMaybe x $ (`action` x) <$> lookup w ws

currentAction :: X () -> [(WorkspaceId, Tag)] -> X ()
currentAction x ws = (\w -> workspaceAction w x ws) =<< gets (W.tag . W.workspace . W.current . windowset)
