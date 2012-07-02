{-# LANGUAGE FlexibleContexts #-}

module XMonad.Hooks.VodikLog ( vodik, vodikPP ) where

import Control.Applicative
import Data.Maybe
import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)
import System.IO

import XMonad hiding (trace)
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

import Workspaces
import Utils

-- TODO: Move
dzenFont        = "-*-envy code r-medium-r-normal-*-12-*-*-*-*-*-*-*"
colorBlack      = "#000000"
colorBlackAlt   = "#050505"
colorGray       = "#484848"
colorGrayAlt    = "#b8bcb8"
colorDarkGray   = "#161616"
colorWhite      = "#ffffff"
colorWhiteAlt   = "#9d9d9d"
colorDarkWhite  = "#444444"
colorMagenta    = "#8e82a2"
colorMagentaAlt = "#a488d9"
colorBlue       = "#60a0c0"
colorBlueAlt    = "#007b8c"
colorRed        = "#d74b73"

vodik :: Resources -> LayoutClass l Window => XConfig l -> IO (XConfig l)
vodik res conf = do
    dzenbar <- startDzen
    return conf { logHook = myLogHook res dzenbar }
 where
    myLogHook res output =
        dynamicLogWithPP $ (vodikPP res) { ppOutput = hPutStrLn output }

vodikPP :: Resources -> PP
vodikPP res = defaultPP
    { ppCurrent         = dzenColor colorWhite    colorBlue     . dzenWSIcon res True
    , ppUrgent          = dzenColor colorWhite    colorRed      . dzenWSIcon res True
    , ppVisible         = dzenColor colorWhite    colorGray     . dzenWSIcon res True
    , ppHidden          = dzenColor colorGrayAlt  colorGray     . dzenWSIcon res True
    , ppHiddenNoWindows = dzenColor colorGray     colorBlackAlt . dzenWSIcon res False
    , ppTitle           = dzenColor colorWhiteAlt colorBlackAlt . shorten 150
    , ppLayout          = dzenPPLayout res colorRed colorBlue colorBlack . words
    , ppSep             = ""
    , ppWsSep           = ""
    , ppSort            = getSortByIndexWithoutNSP
    , ppOrder           = \(ws:l:t:_) -> [ ws, l, dzenColor colorBlue colorBlackAlt "» ", t ]
    }

startDzen :: MonadIO m => m Handle
startDzen = spawnPipe myDzen

myDzen :: String
myDzen = "dzen2 " ++ unwords
    [ "-y"   , "-16"
    , "-h"   ,  "16"
    , "-fn"  , dzenFont
    , "-fg"  , colorWhite
    , "-bg"  , colorBlackAlt
    , "-ta l"
    , "-e 'onstart=lower'" ]

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

getSortByIndexWithoutNSP :: X WorkspaceSort
getSortByIndexWithoutNSP = (. filter ((/= "NSP") . W.tag)) <$> getSortByIndex

