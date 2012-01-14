module Workspaces
    ( getWSName
    , getWorkspaces
    , filterWS
    , workspaceRules
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
import XMonad.Util.WindowProperties

type PPWS      = (Int, String)
type PPInfoMap = M.Map String PPWS

data PPInfo = PPInfo
    { getInfo   :: String -> Maybe PPWS
    , getLayout :: String -> Maybe String
    }

data Workspace = Workspace String String [String]

getWSName :: Workspace -> String
getWSName (Workspace n _ _) = n

getWorkspaces:: [Workspace] -> [String]
getWorkspaces = map getWSName

filterWS :: String -> [Workspace] -> [Workspace]
filterWS name = filter $ (name /=) . getWSName

workspaceRules :: (String -> Property) -> [Workspace] -> ManageHook
workspaceRules c (Workspace n _ prop:xs) =
    composeAll [ propertyToQuery (c p) --> doShift n | p <- prop ] <+> workspaceRules c xs
workspaceRules _ [] = idHook

findLayoutIcons :: FilePath -> ListT IO (String, String)
findLayoutIcons root = do
    icons <- ListT $ getDirectoryContents root
    guard $ "layout-" `isPrefixOf` icons
    let icon = root </> icons
    return (icons, icon)

buildWSInfo :: FilePath -> [Workspace] -> [(WorkspaceId, PPWS)]
buildWSInfo root ws = do
    (Workspace n i _, pos) <- zip ws [1..]
    let icon = root </> i ++ ".xbm"
    return (n, (pos, icon))

getPPInfo :: [Workspace] -> IO PPInfo
getPPInfo ws = do
    root <- (++ "/.xmonad/icons") . fromMaybe "/home/simongmzlj" . lookup "HOME" <$> getEnvironment
    list <- runListT $ findLayoutIcons root
    return PPInfo
        { getInfo   = \l -> M.lookup l . M.fromList $ buildWSInfo root ws
        , getLayout = \l -> M.lookup ("layout-" ++ l ++ ".xbm") $ M.fromList list
        }
