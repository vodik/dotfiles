{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Util.Composition where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception
import Control.Monad
import System.Posix.Types (ProcessGroupID(..))
import System.Posix.Process (getProcessStatus)
import System.Posix.Signals

import XMonad hiding (spawn)
import qualified XMonad.Util.ExtensibleState as XS

import Run

data CompositorPID = CompositorPID { pid :: Maybe ProcessGroupID }
    deriving (Read, Show, Typeable)

instance ExtensionClass CompositorPID where
   initialValue  = CompositorPID Nothing
   extensionType = PersistentExtension

startCompositor :: String -> [String] -> X ()
startCompositor prog args = XS.gets pid >>= \pid ->
    case pid of
        Just pid -> running pid >>= flip unless start
        Nothing  -> start
  where
    start       = CompositorPID . Just <$> run prog args >>= XS.put
    running pid = io . handle (\(SomeException _) -> return False) $ do
        void $ getProcessStatus False False pid
        return True

stopCompositor :: X ()
stopCompositor = XS.gets pid >>= flip whenJust (io . signalProcess sigTERM)
