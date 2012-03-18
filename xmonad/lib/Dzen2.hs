module Dzen2
    ( dzenWSIcon
    , dzenPPLayout
    , dzenIcon
    , dzenAction
    ) where

import Data.Maybe
import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)

import Debug.Trace
import XMonad hiding (trace)
import XMonad.Hooks.DynamicLog

import Workspaces

dzenWSIcon :: Profile p => p -> Bool -> String -> String
dzenWSIcon profile showAll name =
    fromMaybe without $ do
        info <- getWorkspace profile name
        icon <- wsIcon info
        let index = wsIndex info
        return . dzenAction 1 (cmd index) . pad . (++ ' ' : name) $ dzenIcon icon
  where
    cmd n = "xdotool key super+" ++ show n
    without | showAll   = dzenAction 1 (cmd name) $ pad name
            | otherwise = ""

-- TODO: first arg was PPInfo
dzenPPLayout :: Profile p => p -> String -> String -> String -> [String] -> String
dzenPPLayout profile tc fc bg (x:xs) =
    let (fg, l) = if x == "Triggered"
                     then (tc, head xs)
                     else (fc, x)
    in dzenAction 1 "xdotool key super+space"
     . dzenAction 3 "xdotool key super+a"
     . dzenColor fg bg . pad . dzenIcon
     $ getLayoutIcon profile l

dzenIcon :: String -> String
dzenIcon = wrap "^i(" ")"

dzenAction :: Int -> String -> String -> String
dzenAction m f = concat [ "^ca(", show m, ",", f, ")" ] `wrap` "^ca()"
