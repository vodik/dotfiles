{-# LANGUAGE FlexibleContexts #-}

module XMonad.Hooks.VodikLog ( vodik, vodikPP ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)
import System.FilePath
import System.IO
import System.IO.Unsafe

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

type WSMap = [(String, Int)]

xmonadDir :: String
xmonadDir = unsafePerformIO getXMonadDir

vodik :: LayoutClass l Window => XConfig l -> IO (XConfig l)
vodik conf = do
    dzenbar <- startDzen
    let wsMap    = zip (workspaces conf) [1..]
        logHook' = dynamicLogWithPP (vodikPP wsMap) { ppOutput = hPutStrLn dzenbar }
    return conf { logHook = logHook' }

startDzen :: MonadIO m => m Handle
startDzen = spawnPipe myDzen

myDzen :: String
myDzen = "dzen2 " ++ unwords
    [ "-y"   , "-16"
    , "-h"   ,  "16"
    , "-fn"  , "'" ++ dzenFont ++ "'"
    , "-fg"  , "'" ++ colorWhite ++ "'"
    , "-bg"  , "'" ++ colorBlackAlt ++ "'"
    , "-ta l"
    , "-e 'onstart=lower'" ]

vodikPP :: WSMap -> PP
vodikPP m = defaultPP
    { ppCurrent         = dzenColor colorWhite    colorBlue     . dzenWSIcon m True
    , ppUrgent          = dzenColor colorWhite    colorRed      . dzenWSIcon m True
    , ppVisible         = dzenColor colorWhite    colorGray     . dzenWSIcon m True
    , ppHidden          = dzenColor colorGrayAlt  colorGray     . dzenWSIcon m True
    , ppHiddenNoWindows = dzenColor colorGray     colorBlackAlt . dzenWSIcon m False
    , ppTitle           = dzenColor colorWhiteAlt colorBlackAlt . shorten 150
    , ppLayout          = dzenPPLayout colorRed colorBlue colorBlack . words
    , ppSep             = ""
    , ppWsSep           = ""
    , ppSort            = getSortByIndexWithout [ "NSP" ]
    , ppOrder           = \(ws:l:t:_) -> [ ws, l, dzenColor colorBlue colorBlackAlt "» ", t ]
    }

dzenWSIcon :: WSMap -> Bool -> String -> String
dzenWSIcon wsMap showAll name =
    fromMaybe without $ do
        guard . not $ all isDigit name
        index <- lookup name wsMap
        let icon   = xmonadDir </> "icons/" </> name <.> ".xbm"
            action = dzenAction 1 (xDoTool True index)
        return . action . pad $ dzenIcon icon ++ " " ++ name
  where
    without | showAll   = dzenAction 1 (xDoTool False $ read name) $ pad name
            | otherwise = ""

dzenPPLayout :: String -> String -> String -> [String] -> String
dzenPPLayout tc fc bg (x:xs) =
    let (fg, l) = if x == "Triggered"
                     then (tc, head xs)
                     else (fc, x)
    in dzenAction 1 "xdotool key super+space"
     . dzenAction 3 "xdotool key super+a"
     . dzenColor fg bg . pad . dzenIcon
     $ layoutIcon l
  where
    layoutIcon = (xmonadDir </>) . ("icons/" </>) . ("layout-" ++) . (<.> ".xbm")

getSortByIndexWithout :: [String] -> X WorkspaceSort
getSortByIndexWithout tags = do
    sort <- getSortByIndex
    return $ sort . filter ((`notElem` tags) . W.tag)

dzenIcon :: String -> String
dzenIcon = wrap "^i(" ")"

dzenAction :: Int -> String -> String -> String
dzenAction m f = concat [ "^ca(", show m, ",", f, ")" ] `wrap` "^ca()"

xDoTool :: Bool -> Int -> String
xDoTool m n = "xdotool key " ++ modifier m ++ show n
  where
    modifier True  = "super+"
    modifier False = "super+ctrl+"

