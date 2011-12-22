module Workspaces ( getWSName
                  , getWSIcon
                  , getIconMap
                  , workspaceRules
                  , Workspace(..) ) where

import qualified Data.Map as M

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties

data Workspace = Workspace String String [Property]

getWSName :: Workspace -> String
getWSName (Workspace n _ _) = n

getWSIcon :: Workspace -> String
getWSIcon (Workspace _ i _) = i

getIconMap :: [Workspace] -> M.Map String String
getIconMap ws = M.fromList $ zip names icons
    where names = map getWSName ws
          icons = map getWSIcon ws

workspaceRules ((Workspace n _ prop):xs) =
    composeAll [ propertyToQuery p --> doShift n | p <- prop ] <+> workspaceRules xs
workspaceRules [] = idHook
