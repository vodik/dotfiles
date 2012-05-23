{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module XMonad.Util.Commands where

import Codec.Binary.UTF8.String
import Control.Concurrent (threadDelay)
import Control.Monad
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Types (ProcessID(..))

import XMonad hiding (spawn)

data Commands = Shell String
              | String :+ [String]

class Command a where
    run :: MonadIO m => a -> m ProcessID

instance Command String where
    run cmd = safeSpawn cmd []

instance Command Commands where
    run (Shell cmd)   = spawnPID cmd
    run (cmd :+ args) = safeSpawn cmd args

spawn :: (MonadIO m, Command c) => c -> m ()
spawn cmd = run cmd >>= const (return ())

delayedSpawn :: (MonadIO m, Command c) => Int -> c -> m ()
delayedSpawn d cmd = io (threadDelay d) >> spawn cmd

execute :: String -> [String] -> IO a
execute cmd args = executeFile (encodeString cmd) True (fmap encodeString args) Nothing

safeSpawn :: MonadIO m => String -> [String] -> m ProcessID
safeSpawn cmd args = xfork $ execute cmd args

-- spawnIn :: (MonadIO m, Command c) => FilePath -> c -> m ()
-- spawnIn dir cmd args = xfork $ do
--     catchIO . setCurrentDirectory $ cleanPath dir
--     execute cmd args
