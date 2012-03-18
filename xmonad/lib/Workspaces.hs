{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE FlexibleContexts #-}

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
import Data.Monoid
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import System.Environment (getEnvironment)
import System.Directory (getDirectoryContents)
import System.FilePath
import qualified Data.Map as M

import Data.Map (Map)

import XMonad (XConfig, Query, X, LayoutClass, Window, WorkspaceId)
import XMonad.Actions.TopicSpace
import XMonad.Hooks.ManageHelpers
import qualified XMonad as X

import SortWindows
import Utils

data Tweaks = Tweaks
    { mainWidth  :: Rational
    , imWidth    :: Rational
    , imGrid     :: Double
    , masterN    :: Int
    }

data WS = WS
    { wsIndex  :: Int
    , wsIcon   :: Maybe String
    , wsRules  :: [Query Bool]
    , wsDir    :: Maybe Dir
    , wsAction :: Maybe (X ())
    }

defaultTweaks = Tweaks
    { mainWidth = 1/2
    , imWidth   = 1/5
    , imGrid    = 2/3
    , masterN   = 2
    }

class Profile a where
    getTweaks     :: a -> Tweaks
    getWSNames    :: a -> [WorkspaceId]
    getWorkspace  :: a -> WorkspaceId -> Maybe WS
    getTerminal   :: a -> String
    getLayoutIcon :: a -> String -> String

applyProfile :: (LayoutClass l Window, Profile c) => c -> XConfig l -> XConfig l
applyProfile conf xconf = xconf
    { X.workspaces = to9 $ getWSNames conf
    , X.terminal   = getTerminal conf
    }
