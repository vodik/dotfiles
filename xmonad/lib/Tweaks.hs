module Tweaks ( Tweaks (..)
              , defaultTweaks
              ) where

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties

import Workspaces

pidgin :: Property
pidgin  = ClassName "Pidgin"  `And` Role "buddy_list"

data Tweaks = Tweaks
    { imClient :: Property
    , imWidth  :: Rational
    , imGrid   :: Double
    , masterN  :: Int
    , wsMod    :: [Workspace] -> [Workspace]
    }

defaultTweaks = Tweaks
    { imClient = pidgin
    , imWidth  = 2/10
    , imGrid   = 2/3
    , masterN  = 1
    , wsMod    = id
    }
