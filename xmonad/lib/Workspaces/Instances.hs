module Workspaces.Instances where

import XMonad
import XMonad.Util.Run
import Workspaces

data Workspace = Workspace [ String ]
data Terminals = Terminals (Maybe String)

instance WorkspaceI Workspace where
    action (Workspace cmds) = const $ mapM_ (`safeSpawn` []) cmds

instance WorkspaceI Terminals where
    action _ cmd = cmd >> cmd >> cmd
