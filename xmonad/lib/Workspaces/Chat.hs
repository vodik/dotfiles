module Workspaces.Chat where

import XMonad
import Workspaces

data Chat = Chat

instance Workspace Chat where
    action _ = spawn "firefox"
