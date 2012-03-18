module Machine where

import Control.Monad
import Control.Monad.List
import Data.Monoid
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import System.Environment (getEnvironment)
import System.Directory (getDirectoryContents)
import System.FilePath
import System.Posix.Unistd (getSystemID, nodeName)
import qualified Data.Map as M
import Data.Map (Map)

import XMonad hiding (workspaces)
import XMonad.Actions.TopicSpace
import XMonad.Hooks.ManageHelpers

import SortWindows
import Utils
import Workspaces

data Machine = Machine
    { rootDir     :: String
    , workspaces  :: Map WorkspaceId WS
    , tweaks      :: Tweaks
    , defaultTerm :: String
    }

instance Profile Machine where
    getTweaks         = tweaks
    getWSNames        = M.keys . workspaces
    getWorkspace p ws = M.lookup ws (workspaces p)
    getLayoutIcon p   = (rootDir p </>) . ("/icons" </>) . ("layout-" ++) . (++ ".xbm")
    getTerminal       = defaultTerm

getMachine :: IO Machine
getMachine = do
    host <- nodeName <$> getSystemID
    root <- (</> "/.xmonad/") <$> getHome
    return $ case host of
        _ -> gmzlj root

data W = W String [Query Bool] (Maybe Dir) (Maybe (X ()))

defaultMachine root w = Machine
    { rootDir       = root
    , workspaces    = M.fromList $ [ mkWS i ws | (ws, i) <- zip w [1..] ]
    , tweaks        = defaultTweaks
    , defaultTerm   = "urxvtc"
    }
  where
    mkWS i (W name r d x) = (name, WS
        { wsIndex  = i
        , wsIcon   = Nothing
        , wsRules  = r
        , wsDir    = d
        , wsAction = x
        })

gmzlj :: Dir -> Machine
gmzlj root = defaultMachine root
    [ ( W "work"   [ className `queryAny` [ "Firefox", "Chromium", "Zathura", "Thunar", "Gimp" ]
                   , title     =? "MusicBrainz Picard"
                   , className ~? "^[Ll]ibre[Oo]ffice" ]
                   Nothing
                   (Just $ spawn "firefox") )
    , ( W "term"   [] Nothing Nothing )
    , ( W "code"   [] (Just "~/projects") Nothing )
    , ( W "chat"   [ className `queryAny` [ "Empathy", "Pidgin", "Skype" ] ]
                   Nothing
                   (Just $ spawn "pidgin" >> spawn "skype") )
    , ( W "virt"   [ className =? "VirtualBox" ]
                   Nothing
                   (Just $ spawn "VirtualBox --startvm 'Windows 7'") )
    , ( W "games"  [ className `queryAny` [ "Sol", "Pychess", "net-minecraft-LauncherFrame", "zsnes", "Wine", "Dwarf_Fortress" ] ]
                   Nothing
                   (Just $ spawn "sol") )
    ]
