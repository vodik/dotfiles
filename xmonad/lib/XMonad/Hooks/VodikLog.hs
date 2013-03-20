{-# LANGUAGE FlexibleContexts #-}

module XMonad.Hooks.VodikLog
    ( dzenVodik
    , VodikConfig(..)
    ) where

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
import XMonad.Util.Commands
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

data VodikConfig = VodikConfig
    { dzenFont     :: String
    , dzenBlack    :: String
    , dzenWhite    :: String
    , dzenWhiteAlt :: String
    , dzenGray     :: String
    , dzenGrayAlt  :: String
    , dzenBlue     :: String
    , dzenRed      :: String
    }

type WSMap = M.Map String Int

xmonadIcons :: String
xmonadIcons = unsafePerformIO getXMonadDir </> "icons/"

dzenVodik :: LayoutClass l Window => VodikConfig -> XConfig l -> IO (XConfig l)
dzenVodik conf xconf = do
    dzenbar <- spawnPipe $ myDzen conf
    let wsMap    = M.fromList $ zip (workspaces xconf) [1..]
        logHook' = dynamicLogWithPP (vodikPP conf wsMap) { ppOutput = hPutStrLn dzenbar }
    return xconf { logHook = logHook' }

myDzen :: VodikConfig -> Executable
myDzen conf = "dzen2" :+ [ "-y", "-16", "-h", "16"
                         , "-fn", dzenFont conf
                         , "-fg", dzenWhite conf
                         , "-bg", dzenBlack conf
                         , "-ta", "l"
                         , "-e", "'onstart=lower'" ]

vodikPP :: VodikConfig -> WSMap -> PP
vodikPP conf ws = defaultPP
    { ppCurrent         = dzenCurrent        . dzenWSIcon ws True
    , ppUrgent          = dzenUrgent         . dzenWSIcon ws True
    , ppVisible         = dzenVisible        . dzenWSIcon ws True
    , ppHidden          = dzenHidden         . dzenWSIcon ws True
    , ppHiddenNoWindows = dzenHiddenNoWindow . dzenWSIcon ws False
    , ppTitle           = dzenTitle          . shorten 150
    , ppLayout          = dzenLayout (dzenRed conf) (dzenBlue conf) (dzenBlack conf) . words
    , ppSep             = ""
    , ppWsSep           = ""
    , ppSort            = getSortByIndexWithout [ "NSP" ]
    , ppOrder           = \(ws:l:t:_) -> [ ws, l, dzenSeperator "» ", t ]
    }
  where
    dzenCurrent        = liftM2 dzenColor dzenWhite    dzenBlue  conf
    dzenUrgent         = liftM2 dzenColor dzenWhite    dzenRed   conf
    dzenVisible        = liftM2 dzenColor dzenWhite    dzenGray  conf
    dzenHidden         = liftM2 dzenColor dzenGrayAlt  dzenGray  conf
    dzenHiddenNoWindow = liftM2 dzenColor dzenGray     dzenBlack conf
    dzenTitle          = liftM2 dzenColor dzenWhiteAlt dzenBlack conf
    dzenSeperator      = liftM2 dzenColor dzenBlue     dzenBlack conf

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
    let (fg, l) = if x == "Triggered" then (tc, head xs) else (fc, x)
        icon    = xmonadIcons </> "layout/" </> l <.> ".xbm"
        actions = dzenAction 1 "xdotool key super+space" . dzenAction 3 "xdotool key super+f"
    in actions . dzenColor fg bg . pad $ dzenIcon icon

getSortByIndexWithout :: [String] -> X WorkspaceSort
getSortByIndexWithout tags = do
    sort <- getSortByIndex
    return $ sort . filter ((`notElem` tags) . W.tag)

dzenIcon :: String -> String
dzenIcon = printf "^i(%s)"

dzenAction :: Int -> String -> String -> String
dzenAction = printf "^ca(%d,%s)%s^ca()"

toWorkspace :: Bool -> Int -> String
toWorkspace = printf "xdotool key %s+%d" . modifier
  where
    modifier True  = "super"
    modifier False = "super+ctrl"
