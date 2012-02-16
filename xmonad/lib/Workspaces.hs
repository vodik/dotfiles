module Workspaces
    ( getWorkspaces
    , filterWS
    , workspaceShift
    , workspaceSort
    , getPPInfo
    , PPWS
    , PPInfo (..)
    , Workspace (..)
    ) where

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

import XMonad hiding (trace)
import XMonad.Hooks.ManageHelpers

type PPWS      = (Int, String)
type PPInfoMap = M.Map String PPWS

data PPInfo = PPInfo
    { getWSInfo :: String -> Maybe PPWS
    , getLayout :: String -> String
    }

data Workspace = Workspace
    { getWSName :: String
    , getRules  :: [Query Bool]
    }

getWorkspaces :: [Workspace] -> [String]
getWorkspaces = map getWSName

filterWS :: String -> [Workspace] -> [Workspace]
filterWS name = filter $ (name /=) . getWSName

workspaceShift :: [Workspace] -> ManageHook
workspaceShift (Workspace n prop:xs) =
    composeAll [ p --> doShift n | p <- prop ] <+> workspaceShift xs
workspaceShift [] = idHook

workspaceSort :: Workspace -> Query Any
workspaceSort (Workspace _ prop) = composeAll [ Any `fmap` p | p <- prop ]

buildWSInfo :: FilePath -> [Workspace] -> [(WorkspaceId, PPWS)]
buildWSInfo root ws = do
    (Workspace n _, pos) <- zip ws [1..]
    let icon = root </> n ++ ".xbm"
    return (n, (pos, icon))

getPPInfo :: [Workspace] -> IO PPInfo
getPPInfo ws = do
    root <- (++ "/.xmonad/icons") . fromMaybe "/home/simongmzlj" . lookup "HOME" <$> getEnvironment
    return PPInfo
        { getWSInfo = \l -> M.lookup l . M.fromList $ buildWSInfo root ws
        , getLayout = (root </>) . ("layout-" ++) . (++ ".xbm")
        }
