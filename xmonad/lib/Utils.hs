{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses #-}

module Utils where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import System.Environment (getEnvironment)
import Text.Regex.Posix ((=~))
import qualified Data.Set as S

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

import Proc

data TNBFULL = TNBFULL deriving (Read, Show, Eq, Typeable)

instance Transformer TNBFULL Window where
    transform TNBFULL x k = k (tag "Triggered" $ noBorders Full) (const x)
      where tag t = renamed [ PrependWords t ]

data BorderUrgencyHook = BorderUrgencyHook !String
    deriving (Show, Read)

instance UrgencyHook BorderUrgencyHook where
    urgencyHook (BorderUrgencyHook cs) w = withDisplay $ \dpy -> io $
        initColor dpy cs >>= maybe (return ()) (setWindowBorder dpy w)

to9 :: [String] -> [String]
to9 ws = (ws ++) . drop (length ws) $ map show [1..9]

queryAny :: Eq a => Query a -> [a] -> Query Bool
queryAny q xs = foldl1 (<||>) $ (q =?) <$> xs

(~?) :: (Functor f) => f String -> String -> f Bool
q ~? x = (=~ x) <$> q

prefixed :: (Functor f) => f String -> String -> f Bool
q `prefixed` x = (x `isPrefixOf`) <$> q

composeOneCaught :: ManageHook -> [MaybeManageHook] -> ManageHook
composeOneCaught f h = composeOne $ h ++ [ Just <$> f ]

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

isFirefoxPreferences :: Query Bool
isFirefoxPreferences = className =? "Firefox" <&&> role =? "Preferences"

withFocused' :: (Window -> X ()) -> X ()
withFocused' f = withWindowSet $ \ws -> whenJust (W.peek ws) $
    \w -> hasResource [ "scratchpad" ] w >>= flip unless (f w)

hasResource :: [String] -> Window -> X Bool
hasResource ign w = withDisplay $ \d -> io $ (`elem` ign) . resName <$> getClassHint d w

getSortByIndexWithoutNSP :: X WorkspaceSort
getSortByIndexWithoutNSP = getSortByIndex >>= \s -> return $ s . filter ((/= "NSP") . W.tag)

delayedSpawn :: Int -> String -> X ()
delayedSpawn d cmd = liftIO (threadDelay d) >> spawn cmd

env :: String -> IO (Maybe String)
env = (<$> getEnvironment) . lookup

getBrowser :: String -> IO String
getBrowser = (<$> env "BROWSER") . fromMaybe

getHome :: IO String
getHome = fromMaybe "/home/simongmzlj" <$> env "HOME"

primaryX :: IO Bool
primaryX = maybe False (== ":0") <$> env "DISPLAY"

startServices :: [String] -> X ()
startServices cmds = whenPrimaryX $
    io (service <$> pidSet) >>= forM_ cmds
  where
    service pm cmd = when (S.null $ findCmd cmd pm) $ spawn cmd

whenPrimaryX :: X () -> X ()
whenPrimaryX f = io primaryX >>= flip when f
