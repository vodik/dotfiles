{-# LANGUAGE ExistentialQuantification #-}

module Profiles ( AnyProfile
                , Profile (..)
                , getProfile
                , myWorkspaces
                ) where

import System.Posix.Unistd (getSystemID, nodeName)

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties

import Workspaces

myWorkspaces :: [Workspace]
myWorkspaces =
    [ Workspace "work"  "arch"     [ "Firefox", "Chromium", "Zathura" ]
    , Workspace "term"  "terminal" [ ]
    , Workspace "code"  "flask2"   [ ]
    , Workspace "chat"  "balloon"  [ "Empathy", "Pidgin" ]
    , Workspace "virt"  "wrench"   [ "VirtualBox" ]
    , Workspace "games" "ghost"    [ "Sol", "Pychess", "net-minecraft-LauncherFrame", "Wine" ]
    ]

empathy = ClassName "Empathy" `And` Role "contact_list"
pidgin  = ClassName "Pidgin"  `And` Role "buddy_list"

data AnyProfile = forall a. (Profile a) => AnyProfile a

data Vodik = Vodik
data Gmzlj = Gmzlj
data Beno = Beno

class Profile a where
    getIM :: a -> Property
    getIM _ = pidgin
    getIMWidth :: a -> Rational
    getIMWidth _ = 2/10
    getTermM :: a -> Int
    getTermM  _ = 1
    getWS :: a -> [Workspace]
    getWS _ = myWorkspaces

instance Profile Vodik where

instance Profile Gmzlj where
    getIMWidth _ = 3/10
    getWS      _ = filterWS "virt" myWorkspaces

instance Profile Beno where
    getIM      _ = empathy
    getTermM   _ = 2

instance Profile AnyProfile where
    getIM (AnyProfile a)      = getIM a
    getIMWidth (AnyProfile a) = getIMWidth a
    getTermM (AnyProfile a)   = getTermM a
    getWS (AnyProfile a)      = getWS a

getProfile :: IO AnyProfile
getProfile = do
    hostName <- nodeName `fmap` getSystemID
    return $ case hostName of
        "vodik" -> AnyProfile Vodik
        "gmzlj" -> AnyProfile Gmzlj
        "beno"  -> AnyProfile Beno
        _       -> AnyProfile Vodik
