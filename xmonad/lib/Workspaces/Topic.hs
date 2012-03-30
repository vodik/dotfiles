module Workspaces.Topic where

import XMonad
import Workspaces

data Topic = Topic String

instance Workspace Topic where
    action _ = spawn "firefox"
