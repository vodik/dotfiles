module Workspaces ( getWSName
                  , getWSIcon
                  , getIconMap
                  , filterWS
                  , workspaceRules
                  , Workspace (..) ) where

import Data.Monoid
import qualified Data.Map as M

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties

data Workspace = Workspace String String [String]

getWSName :: Workspace -> String
getWSName (Workspace n _ _) = n

getWSIcon :: Workspace -> String
getWSIcon (Workspace _ i _) = i

filterWS :: String -> [Workspace] -> [Workspace]
filterWS name = filter $ (name /=) . getWSName

getIconMap :: [Workspace] -> M.Map String String
getIconMap ws = M.fromList [ (n, i) | (Workspace n i _) <- ws ]

workspaceRules :: (String -> Property) -> [Workspace] -> Query (Endo WindowSet)
workspaceRules c (Workspace n _ prop:xs) =
    composeAll [ propertyToQuery (c p) --> doShift n | p <- prop ] <+> workspaceRules c xs
workspaceRules _ [] = idHook
