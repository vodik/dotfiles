module Workspaces.Work where

import XMonad
import Workspaces

data Work = Work

instance Workspace Work where
    action _ = spawn "firefox"
