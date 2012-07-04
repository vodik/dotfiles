{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Util.RunOnce (runOnce) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import Control.Monad

data RunOnce = RunOnce { ran :: Bool }
    deriving (Read, Show, Typeable)

instance ExtensionClass RunOnce where
    initialValue  = RunOnce False
    extensionType = PersistentExtension

runOnce :: X () -> X ()
runOnce f = XS.gets ran >>= flip unless (f >> XS.put (RunOnce True))
