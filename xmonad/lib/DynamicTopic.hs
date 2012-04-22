{-# LANGUAGE DeriveDataTypeable #-}

module DynamicTopic where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

import XMonad
import XMonad.Prompt ( XPConfig )
import XMonad.Prompt.Directory (directoryPrompt)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import Run

topics :: Map String FilePath
topics = M.fromList
    [ ("7", "~/projects")
    , ("8", "~/.build")
    ]

data TopicDirs = TopicDirs { dirs :: Map String FilePath } deriving (Read, Show, Typeable)

instance ExtensionClass TopicDirs where
    initialValue  = TopicDirs topics
    extensionType = PersistentExtension

spawnShell :: X ()
spawnShell = currentTopicDir >>= spawnShellIn

spawnShellIn :: FilePath -> X ()
spawnShellIn dir = asks (terminal . config) >>= \t -> spawnIn dir t []

cleanPath :: FilePath -> FilePath
cleanPath ('~':xs) = dropWhile (== '/') xs
cleanPath      xs  = xs

currentTopicDir :: X String
currentTopicDir = do
    tag <- gets $ W.tag . W.workspace . W.current . windowset
    fromMaybe "" . M.lookup tag . dirs <$> XS.get

updateTopicDir :: String -> X ()
updateTopicDir dir = do
    tag <- gets $ W.tag . W.workspace . W.current . windowset
    XS.modify $ TopicDirs . M.insert tag dir . dirs

changeDir :: XPConfig -> X ()
changeDir c = directoryPrompt c "Set working directory: " updateTopicDir
