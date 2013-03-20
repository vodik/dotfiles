{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Util.Services
    ( startService
    , stopService
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Map as Map
import Data.Maybe
import System.Posix.Types (ProcessGroupID)
import System.Posix.Process (getProcessStatus)
import System.Posix.Signals

import XMonad hiding (spawn)
import XMonad.Util.Commands
import qualified XMonad.Util.ExtensibleState as XS

data Services = Services { services :: Map String ProcessGroupID }
    deriving (Read, Show, Typeable)

instance ExtensionClass Services where
    initialValue  = Services Map.empty
    extensionType = PersistentExtension

startService :: Command c => String -> c -> X ()
startService name cmd = XS.gets (Map.lookup name . services) >>= \pid ->
    case pid of
        Just pid -> running pid >>= flip unless start
        Nothing  -> start
  where
    start = do
        pid <- run cmd
        XS.modify (Services . Map.insert name pid . services)

    running pid = io . handle (\(SomeException _) -> return False) $
        isNothing <$> getProcessStatus False False pid

stopService :: String -> X ()
stopService name = XS.gets (Map.lookup name . services) >>= \pid ->
    case pid of
        Nothing  -> return ()
        Just pid -> do
            io $ signalProcess sigTERM pid
            XS.modify (Services . Map.delete name . services)
