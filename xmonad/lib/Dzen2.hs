module Dzen2
    ( dzenWSIcon
    , dzenPPLayout
    , dzenIcon
    , dzenAction
    ) where

import Data.Maybe
import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)

import XMonad hiding (trace)
import XMonad.Hooks.DynamicLog

import Workspaces

-- TODO: Don't like how i'm handleing index
dzenWSIcon :: Resources -> Bool -> String -> String
dzenWSIcon res showAll name =
    fromMaybe without $ do
        (index, i) <- workspaceData res name
        icon <- i
        return . dzenAction 1 (xDoTool True index)
               . pad . (++ ' ' : name) $ dzenIcon icon
  where
    without | showAll   = dzenAction 1 (xDoTool False $ read name) $ pad name
            | otherwise = ""

xDoTool True  n | n <= 9    = "xdotool key super+" ++ show n
                | otherwise = ""
xDoTool False n | n <= 9    = "xdotool key super+ctrl+" ++ show n
                | otherwise = ""

dzenPPLayout :: Resources -> String -> String -> String -> [String] -> String
dzenPPLayout res tc fc bg (x:xs) =
    let (fg, l) = if x == "Triggered"
                     then (tc, head xs)
                     else (fc, x)
    in dzenAction 1 "xdotool key super+space"
     . dzenAction 3 "xdotool key super+a"
     . dzenColor fg bg . pad . dzenIcon
     $ layoutIcon res l

dzenIcon :: String -> String
dzenIcon = wrap "^i(" ")"

dzenAction :: Int -> String -> String -> String
dzenAction m f = concat [ "^ca(", show m, ",", f, ")" ] `wrap` "^ca()"
