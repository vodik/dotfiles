module Workspaces
    ( getWSName
    , getWorkspaces
    , filterWS
    , workspaceRules
    , getIconSet
    , PPWS
    , PPInfo (..)
    , Workspace (..)
    ) where

import Data.Monoid
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import System.Environment (getEnvironment)
import qualified Data.Map as M

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties

type PPWS      = (Int, String)
type PPInfoMap = M.Map String PPWS

data PPInfo  = PPInfo { getInfo :: String -> Maybe PPWS }
data Workspace = Workspace String String [String]

getWSName :: Workspace -> String
getWSName (Workspace n _ _) = n

getWorkspaces :: [Workspace] -> [String]
getWorkspaces = map getWSName

filterWS :: String -> [Workspace] -> [Workspace]
filterWS name = filter $ (name /=) . getWSName

workspaceRules :: (String -> Property) -> [Workspace] -> ManageHook
workspaceRules c (Workspace n _ prop:xs) =
    composeAll [ propertyToQuery (c p) --> doShift n | p <- prop ] <+> workspaceRules c xs
workspaceRules _ [] = idHook

getIconSet :: [Workspace] -> IO PPInfo
getIconSet ws = do
    home <- fromMaybe "/home/simongmzlj" . lookup "HOME" <$> getEnvironment
    return . PPInfo $ wrapIcon (getIconMap ws) $ iconPath home
  where
    getIconMap ws = M.fromList [ (n, (x, i)) | (Workspace n i _, x) <- zip ws [1..] ]
    iconPath      = (++ "/.xmonad/icons/")

wrapIcon :: PPInfoMap -> FilePath -> String -> Maybe PPWS
wrapIcon m path t = M.lookup t m >>= \(n, i) -> Just (n, path ++ i ++ ".xbm")
