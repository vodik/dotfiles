module Dzen2
    ( dzenify
    , matchIcon
    , dzenIcon
    , dzenAction
    ) where

import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)

import XMonad
import XMonad.Hooks.DynamicLog

import Workspaces

dzenify :: PPInfo -> Bool -> WorkspaceId -> String
dzenify icons showAll c =
    maybe without (\(n, i) -> dzenAction 1 (cmd $ show n) . pad . (++ ' ' : c) $ dzenIcon i) $ getInfo icons c
  where
    cmd n = "xdotool key super+" ++ n
    without | showAll   = dzenAction 1 (cmd c) $ pad c
            | otherwise = ""

matchIcon :: PPInfo -> String -> String -> String -> [String] -> String
matchIcon icons t f b (x:xs)
    | x == "Triggered" = matchIcon' icons t $ head xs
    | otherwise        = matchIcon' icons f x
  where
    matchIcon' icons c i =
        maybe "" (dzenColor c b . pad . dzenIcon) $ getLayout icons i

dzenIcon :: String -> String
dzenIcon = wrap "^i(" ")"

dzenAction :: Int -> String -> String -> String
dzenAction = flip flip "^ca()" . (wrap .) . flip wrap ")" . wrap "^ca(" "," . show
