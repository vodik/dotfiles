module Workspaces.Terminals where

import XMonad
import Workspaces

data Terminals = Terminals (Maybe String)

instance Workspace Terminals where
    action _ = spawn "firefox"
