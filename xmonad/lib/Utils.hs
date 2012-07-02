module Utils where

import Control.Applicative
import Control.Monad
import Data.List
import Text.Regex.Posix ((=~))

import XMonad hiding (spawn)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W

data BorderUrgencyHook = BorderUrgencyHook !String
    deriving (Show, Read)

instance UrgencyHook BorderUrgencyHook where
    urgencyHook (BorderUrgencyHook cs) w = withDisplay $ \dpy -> io $
        initColor dpy cs >>= maybe (return ()) (setWindowBorder dpy w)

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
