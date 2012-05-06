{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Util.Services
    ( startService
    , stopService
    ) where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Map as Map
import System.Posix.Types (ProcessGroupID(..))
import System.Posix.Process (getProcessStatus)
import System.Posix.Signals

import XMonad hiding (spawn)
import qualified XMonad.Util.ExtensibleState as XS

import Run

data Services = Services { services :: Map String ProcessGroupID }
    deriving (Read, Show, Typeable)

instance ExtensionClass Services where
    initialValue  = Services Map.empty
    extensionType = PersistentExtension

startService :: String -> [String] -> X ()
startService prog args = XS.gets (Map.lookup prog . services) >>= \pid ->
    case pid of
        Just pid -> running pid >>= flip unless start
        Nothing  -> start
  where
    start = do
        pid <- run prog args
        XS.modify (Services . Map.insert prog pid . services)

    running pid = io . handle (\(SomeException _) -> return False) $ do
        status <- getProcessStatus False False pid
        case status of
            Just _  -> return False
            Nothing -> return True

stopService :: String -> X ()
stopService prog = XS.gets (Map.lookup prog . services) >>= \pid ->
    case pid of
        Nothing  -> return ()
        Just pid -> do
            io $ signalProcess sigTERM pid
            XS.modify (Services . Map.delete prog . services)
