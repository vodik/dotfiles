{-# LANGUAGE ExistentialQuantification #-}

module Tweaks ( Tweaks (..)
              , defaultTweaks
              , getTweaks
              ) where

import System.Posix.Unistd (getSystemID, nodeName)

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties

import Workspaces

empathy = ClassName "Empathy" `And` Role "contact_list"
pidgin  = ClassName "Pidgin"  `And` Role "buddy_list"

data Tweaks = Tweaks
    { imClient :: Property
    , imWidth  :: Rational
    , masterN  :: Int
    , wsFilter :: [Workspace] -> [Workspace]
    }

defaultTweaks = Tweaks
    { imClient = pidgin
    , imWidth  = 2/10
    , masterN  = 1
    , wsFilter = id
    }

gmzljTweaks = defaultTweaks
    { imWidth  = 3/10
    , wsFilter = filterWS "virt"
    }

benoTweaks = defaultTweaks
    { imClient = empathy
    , masterN  = 2
    }

getTweaks :: IO Tweaks
getTweaks = do
    hostName <- nodeName `fmap` getSystemID
    return $ case hostName of
        "gmzlj" -> gmzljTweaks
        "beno"  -> benoTweaks
        _       -> defaultTweaks
