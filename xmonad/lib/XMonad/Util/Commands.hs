{-# LANGUAGE FlexibleInstances #-}

module XMonad.Util.Commands
    ( Command(..)
    , Commands(..)
    , execute, executeShell
    , run, spawn
    , runWith
    , delayedSpawn
    , spawnIn
    , spawnWithEnv
    ) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import System.Directory (setCurrentDirectory)
import System.Posix.Env
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Types (ProcessID(..))
import XMonad hiding (spawn)

infixr 4 :+
data Commands = Shell String
              | String :+ [String]

class Command a where
    exec :: a -> IO ()

instance Command String where
    exec cmd = execute cmd []

instance Command Commands where
    exec (Shell cmd)   = executeShell cmd
    exec (cmd :+ args) = execute cmd args

execute :: String -> [String] -> IO ()
execute cmd args = executeFile (encodeString cmd) True (encodeString <$> args) Nothing

executeShell :: String -> IO ()
executeShell cmd = executeFile "/bin/sh" False [ "-c", encodeString cmd ] Nothing

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
