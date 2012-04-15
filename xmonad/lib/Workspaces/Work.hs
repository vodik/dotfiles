module Workspaces.Work where

import XMonad
import XMonad.Util.Run
import Workspaces

data Work = Work

instance Workspace Work where
    action _ = safeSpawn "firefox" []
