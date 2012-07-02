{-# LANGUAGE FlexibleContexts #-}

module Utils where

import Control.Applicative
import Control.Monad
import Data.List
import Graphics.X11.Xlib.Display
import Graphics.X11.Xinerama (getScreenInfo)
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

applyUrgency :: LayoutClass l Window => String -> XConfig l -> XConfig l
applyUrgency color = withUrgencyHookC (BorderUrgencyHook color) conf
    where conf = urgencyConfig { suppressWhen = Focused }

getScreen :: IO Rectangle
getScreen = openDisplay "" >>= fmap head . getScreenInfo

positionRationalRect :: Rational -> Rectangle -> W.RationalRect
positionRationalRect bh (Rectangle sx sy sw sh) =
    let h  = (2 * fi sh / 5) - bh
        ry = (fi sh - h - bh) / fi sh
        rh = h / fi sh
    in W.RationalRect 0 ry 1 rh

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
