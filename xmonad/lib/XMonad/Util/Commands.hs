{-# LANGUAGE FlexibleInstances #-}

module XMonad.Util.Commands
    ( Command(..)
    , Commands(..)
    , execute, executeShell
    , run, spawn
    , runWith
    , delayedSpawn
    , spawnIn
    ) where

import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import System.Directory (setCurrentDirectory)
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Types (ProcessID(..))
import XMonad hiding (spawn)

infixr 4 :+
data Commands = Shell String
              | String :+ [String]

class Command a where
    exec :: MonadIO m => a -> m ()

instance Command String where
    exec cmd = execute cmd []

instance Command Commands where
    exec (Shell cmd)   = executeShell cmd
    exec (cmd :+ args) = execute cmd args

execute :: MonadIO m => String -> [String] -> m ()
execute cmd args = io $ executeFile (encodeString cmd) True (encodeString <$> args) Nothing

executeShell :: MonadIO m => String -> m ()
executeShell cmd = io $ executeFile "/bin/sh" False [ "-c", encodeString cmd ] Nothing

run :: (MonadIO m, Command c) => c -> m ProcessID
run = xfork . exec

spawn :: (MonadIO m, Command c) => c -> m ()
spawn = void_ . run

runWith :: (MonadIO m, Command c) => IO () -> c -> m ProcessID
runWith f c = xfork $ f >> exec c

delayedSpawn :: (MonadIO m, Command c) => Int -> c -> m ()
delayedSpawn d cmd = io (threadDelay d) >> spawn cmd

spawnIn :: (MonadIO m, Command c) => FilePath -> c -> m ()
spawnIn dir = void_ . runWith (setDir dir)
  where
    setDir = catchIO . setCurrentDirectory

void_ :: Monad m => m a -> m ()
void_ = (>>= const (return ()))
