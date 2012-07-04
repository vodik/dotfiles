{-# LANGUAGE FlexibleContexts #-}

module XMonad.Hooks.VodikLog ( dzenVodik ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import System.FilePath
import System.IO
import System.IO.Unsafe
import Text.Printf
import qualified Data.Map as M

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

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

type WSMap = M.Map String Int

xmonadIcons :: String
xmonadIcons = unsafePerformIO getXMonadDir </> "icons/"

dzenVodik :: LayoutClass l Window => XConfig l -> IO (XConfig l)
dzenVodik conf = do
    dzenbar <- spawnPipe myDzen
    let wsMap    = M.fromList $ zip (workspaces conf) [1..]
        logHook' = dynamicLogWithPP (vodikPP wsMap) { ppOutput = hPutStrLn dzenbar }
    return conf { logHook = logHook' }

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
vodikPP ws = defaultPP
    { ppCurrent         = dzenColor colorWhite    colorBlue     . dzenWSIcon ws True
    , ppUrgent          = dzenColor colorWhite    colorRed      . dzenWSIcon ws True
    , ppVisible         = dzenColor colorWhite    colorGray     . dzenWSIcon ws True
    , ppHidden          = dzenColor colorGrayAlt  colorGray     . dzenWSIcon ws True
    , ppHiddenNoWindows = dzenColor colorGray     colorBlackAlt . dzenWSIcon ws False
    , ppTitle           = dzenColor colorWhiteAlt colorBlackAlt . shorten 150
    , ppLayout          = dzenLayout colorRed colorBlue colorBlack . words
    , ppSep             = ""
    , ppWsSep           = ""
    , ppSort            = getSortByIndexWithout [ "NSP" ]
    , ppOrder           = \(ws:l:t:_) -> [ ws, l, dzenColor colorBlue colorBlackAlt "» ", t ]
    }

dzenWSIcon :: WSMap -> Bool -> String -> String
dzenWSIcon ws showAll name =
    fromMaybe without $ do
        guard . not $ all isDigit name
        index <- M.lookup name ws
        let icon   = xmonadIcons </> name <.> ".xbm"
            action = dzenAction 1 (toWorkspace True index)
        return . action . pad $ unwords [ dzenIcon icon, name ]
  where
    without | showAll   = dzenAction 1 (toWorkspace False $ read name) $ pad name
            | otherwise = ""

dzenLayout :: String -> String -> String -> [String] -> String
dzenLayout tc fc bg (x:xs) =
    let (fg, l) = if x == "Triggered" then (tc, head xs)
                                      else (fc, x)
        icon    = xmonadIcons </> "layout/" </> l <.> ".xbm"
        actions = dzenAction 1 "xdotool key super+space" . dzenAction 3 "xdotool key super+f"
    in actions . dzenColor fg bg . pad $ dzenIcon icon

getSortByIndexWithout :: [String] -> X WorkspaceSort
getSortByIndexWithout tags = do
    sort <- getSortByIndex
    return $ sort . filter ((`notElem` tags) . W.tag)

dzenIcon :: String -> String
dzenIcon = wrap "^i(" ")"

dzenAction :: Int -> String -> String -> String
dzenAction = printf "^ca(%d,%s)%s^ca()"

toWorkspace :: Bool -> Int -> String
toWorkspace = printf "xdotool key %s+%d" . modifier
  where
    modifier True  = "super"
    modifier False = "super+ctrl"
