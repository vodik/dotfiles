module Workspaces.Instances where

import XMonad
import Workspaces

data Workspace = Workspace [ String ]
data Terminals = Terminals (Maybe String)

instance WorkspaceI Workspace where
    action (Workspace cmds) = const $ mapM_ spawn cmds

instance WorkspaceI Terminals where
    action _ cmd = cmd >> cmd >> cmd
