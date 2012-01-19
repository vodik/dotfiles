module Dzen2
    ( dzenWSIcon
    , dzenPPLayout
    , dzenIcon
    , dzenAction
    ) where

import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)

import XMonad
import XMonad.Hooks.DynamicLog

import Workspaces

dzenWSIcon :: PPInfo -> Bool -> WorkspaceId -> String
dzenWSIcon info showAll c =
    maybe without (\(n, i) -> dzenAction 1 (cmd $ show n) . pad . (++ ' ' : c) $ dzenIcon i) $ getWSInfo info c
  where
    cmd n = "xdotool key super+" ++ n
    without | showAll   = dzenAction 1 (cmd c) $ pad c
            | otherwise = ""

dzenPPLayout :: PPInfo -> String -> String -> String -> [String] -> String
dzenPPLayout info tc fc bg (x:xs) =
    let (fg, l) = if x == "Triggered"
                     then (tc, head xs)
                     else (fc, x)
    in dzenAction 1 "xdotool key super+n"
     . dzenAction 3 "xdotool key super+a"
     . dzenColor fg bg . pad . dzenIcon
     $ getLayout info l

dzenIcon :: String -> String
dzenIcon = wrap "^i(" ")"

dzenAction :: Int -> String -> String -> String
dzenAction m f = concat [ "^ca(", show m, ",", f, ")" ] `wrap` "^ca()"
