{-# LANGUAGE ExistentialQuantification #-}

module Tweaks ( Tweaks (..)
              , defaultTweaks
              , getTweaks
              ) where

import Data.Ratio
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

gmzljTweaks = defaultTweaks
    { imWidth = 3/10
    , imGrid  = 3/2
    , wsMod   = filterWS "virt"
    }

benoTweaks = defaultTweaks
    { imClient = empathy
    , masterN  = 2
    }

-- getAspectRatio :: IO Tweaks
-- getAspectRatio = do
--     let ratio = 1920 % 1200 :: Fractional
--     return $ case ratio of
--         (Rational 6 10) -> defaultTweaks
--         (Rational 4 3)  -> defaultTweaks
--         _               -> defaultTweaks

getTweaks :: IO Tweaks
getTweaks = do
    hostName <- nodeName `fmap` getSystemID
    return $ case hostName of
        "gmzlj" -> gmzljTweaks
        "beno"  -> benoTweaks
        _       -> defaultTweaks
