module Workspaces ( getWSName
                  , getWorkspaces
                  , filterWS
                  , workspaceRules
                  , getIconSet
                  , Icon
                  , Icons (..)
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

type Icon      = String
type IconMap   = M.Map String Icon

data Icons     = Icons { getIcon :: String -> Maybe Icon }
data Workspace = Workspace String String [String]

getWSName :: Workspace -> String
getWSName (Workspace n _ _) = n

getWorkspaces :: [Workspace] -> [String]
getWorkspaces = map getWSName

filterWS :: String -> [Workspace] -> [Workspace]
filterWS name = filter $ (name /=) . getWSName

workspaceRules :: (String -> Property) -> [Workspace] -> Query (Endo WindowSet)
workspaceRules c (Workspace n _ prop:xs) =
    composeAll [ propertyToQuery (c p) --> doShift n | p <- prop ] <+> workspaceRules c xs
workspaceRules _ [] = idHook

getIconSet :: [Workspace] -> IO Icons
getIconSet ws = do
    home <- fromMaybe "/home/simongmzlj" . lookup "HOME" <$> getEnvironment
    return $ Icons $ wrapIcon (getIconMap ws) $ iconPath home
    where
        getIconMap ws = M.fromList [ (n, i) | (Workspace n i _) <- ws ]
        iconPath      = (++ "/.xmonad/icons/")

wrapIcon :: IconMap -> FilePath -> String -> Maybe Icon
wrapIcon m path t = M.lookup t m >>= \i -> Just $ path ++ i ++ ".xbm"
