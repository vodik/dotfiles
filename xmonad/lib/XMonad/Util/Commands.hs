{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module XMonad.Util.Commands
    ( Command
    , Executable(..)
    , run, spawn
    , runWith
    , delayedSpawn
    , spawnIn
    , spawnWithEnv
    ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.ByteString.UTF8 (fromString)
import System.Directory (setCurrentDirectory)
import System.Posix.Env (putEnv)
import System.Posix.Process.ByteString (createSession, executeFile, forkProcess)
import System.Posix.Types (ProcessID(..))
import XMonad hiding (spawn)

infixr 4 :+
data Executable = Shell String
                | String :+ [String]

class Command a where
    exec :: a -> IO ()

instance Command String where
    exec cmd = execute cmd []

instance Command Executable where
    exec (Shell cmd)   = executeShell cmd
    exec (cmd :+ args) = execute cmd args

execute :: String -> [String] -> IO ()
execute cmd args = executeFile (fromString cmd) True (fromString <$> args) Nothing

executeShell :: String -> IO ()
executeShell cmd = executeFile "/bin/sh" False [ "-c", fromString cmd ] Nothing

run :: (MonadIO m, Command c) => c -> m ProcessID
run = xfork . exec

spawn :: (MonadIO m, Command c) => c -> m ()
spawn = void_ . run

runWith :: (MonadIO m, Command c) => IO () -> c -> m ProcessID
runWith f c = xfork $ catchIO f >> exec c

delayedSpawn :: (MonadIO m, Command c) => Int -> c -> m ()
delayedSpawn d cmd = io (threadDelay d) >> spawn cmd

spawnIn :: (MonadIO m, Command c) => FilePath -> c -> m ()
spawnIn dir = void_ . runWith (setCurrentDirectory dir)

spawnWithEnv :: (MonadIO m, Command c) => [String] -> c -> m ()
spawnWithEnv env = void_ . runWith (mapM_ putEnv env)

void_ :: Monad m => m a -> m ()
void_ = (>>= const (return ()))
