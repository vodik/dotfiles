module Workspaces ( getWSName
                  , filterWS
                  , workspaceRules
                  , getIconSet
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

type IconMap = M.Map String String

data Icons = Icons {
    getIcon :: String -> Maybe String
    }

data Workspace = Workspace String String [String]

getWSName :: Workspace -> String
getWSName (Workspace n _ _) = n

filterWS :: String -> [Workspace] -> [Workspace]
filterWS name = filter $ (name /=) . getWSName

workspaceRules :: (String -> Property) -> [Workspace] -> Query (Endo WindowSet)
workspaceRules c (Workspace n _ prop:xs) =
    composeAll [ propertyToQuery (c p) --> doShift n | p <- prop ] <+> workspaceRules c xs
workspaceRules _ [] = idHook

getIconMap :: [Workspace] -> IconMap
getIconMap ws = M.fromList [ (n, i) | (Workspace n i _) <- ws ]

getIconSet :: [Workspace] -> IO Icons
getIconSet ws = do
    home <- fromMaybe "/home/simongmzlj" . lookup "HOME" <$> getEnvironment
    return $ Icons $ wrapIcon (getIconMap ws) $ iconPath home
    where
        iconPath = (++"/etc/xmonad/icons/")

wrapIcon :: IconMap -> FilePath -> String -> Maybe String
wrapIcon m path t = M.lookup t m >>= \i -> Just $ "^i(" ++ path ++ i ++ ".xbm)"
