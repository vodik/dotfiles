module Run
    ( fork, run
    , spawn, spawnIn
    ) where

import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Monad
import System.Directory (setCurrentDirectory)
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Types (ProcessGroupID(..))

import XMonad hiding (spawn)

execute :: FilePath -> [String] -> IO ()
execute prog args = executeFile (encodeString prog) True (map encodeString args) Nothing

fork :: MonadIO m => IO () -> m ProcessGroupID
fork f = io . forkProcess $ uninstallSignalHandlers >> void createSession >> f

run :: MonadIO m => FilePath -> [String] -> m ProcessGroupID
run = fork .: execute

spawn :: (Functor f, MonadIO f) => FilePath -> [String] -> f ()
spawn = void .: run

spawnIn :: (Functor f, MonadIO f) => FilePath -> FilePath -> [String] -> f ()
spawnIn dir cmd args = void . fork $ do
    catchIO . setCurrentDirectory $ cleanPath dir
    execute cmd args

cleanPath :: FilePath -> FilePath
cleanPath ('~':xs) = dropWhile (== '/') xs
cleanPath      xs  = xs

infixr 8 .:
(.:) = (.) . (.)
