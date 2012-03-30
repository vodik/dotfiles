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
import Control.Monad.Writer
import Data.Monoid
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import System.Environment (getEnvironment)
import System.Directory (getDirectoryContents)
import System.FilePath

import XMonad (XConfig, Query, X, LayoutClass, Window, WorkspaceId)
import XMonad.Actions.TopicSpace
import XMonad.Hooks.ManageHelpers
import qualified XMonad as X

import SortWindows
import Utils

import qualified Data.Map as M

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

data WSOp = WSOp
    { getIndex  :: String -> Int
    , getIcon   :: String -> Maybe String
    , getRules  :: String -> [Query Bool]
    , getDir    :: String -> Maybe Dir
    , getAction :: String -> Maybe (X ())
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

class Workspace a where
    action :: a -> X ()

data Tag = forall a. (Workspace a) => Tag a
         | forall a. (Workspace a) => a :> [Query Bool]

-- infixr 0 $=, $>
-- infixr 1 :>

instance Workspace Tag where
    action (a :> _) = action a

type WSGenT = WriterT [(String, Tag)]
type WSGen  = Writer  [(String, Tag)]

tag1 n t = tell [(n, Tag t)]
tag  n t = tell [(n, t)]

buildTags = execWriter

-- mkSpawner :: [Tag] -> M.Map String (X ())
-- mkSpawner = M.fromList . map (\(n :>> a) -> (n, action a))
