module Utils where

import Control.Monad
import Data.Monoid
import Text.Regex.Posix ((=~))
import qualified Data.Map as M

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

import Workspaces

data Tweaks = Tweaks
    { mainWidth  :: Rational
    , imWidth    :: Rational
    , imGrid     :: Double
    , masterN    :: Int
    , wsModifier :: [Workspace] -> [Workspace]
    }

to9 :: [String] -> [String]
to9 ws = (ws ++) . drop (length ws) $ map show [1..9]

queryAny :: Eq a => Query a -> [a] -> Query Bool
queryAny q xs = foldl1 (<||>) $ map (q =?) xs

(-|>) :: (Monad m, Monoid a) => m Bool -> (m a, m a) -> m a
p -|> (f1, f2) = p >>= \b -> if b then f1 else f2

(~?) :: (Functor f) => f String -> String -> f Bool
q ~? x = fmap (=~ x) q

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

isFirefoxPreferences :: Query Bool
isFirefoxPreferences = className =? "Firefox" <&&> role =? "Preferences"

withFocused' :: (Window -> X ()) -> X ()
withFocused' f = withWindowSet $ \ws -> whenJust (W.peek ws) $
    \w -> hasResource ["scratchpad"] w >>= \ign -> unless ign $ f w

hasResource :: [String] -> Window -> X Bool
hasResource ign w = withDisplay $ \d -> fmap ((`elem` ign) . resName) .
    io $ getClassHint d w

skipNSP :: WSType
skipNSP = WSIs . return $ ("NSP" /=) . W.tag

nextWS' = moveTo Next skipNSP
prevWS' = moveTo Prev skipNSP
shiftToNext' = shiftTo Next skipNSP
shiftToPrev' = shiftTo Prev skipNSP

getSortByIndexWithoutNSP :: X WorkspaceSort
getSortByIndexWithoutNSP = getSortByIndex >>= \s ->
    return $ s . filter (\(W.Workspace tag _ _) -> tag /= "NSP")
