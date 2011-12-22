{-# LANGUAGE ExistentialQuantification #-}

import Data.Maybe (fromMaybe)
import Text.Regex.Posix ((=~))
import System.Directory (getCurrentDirectory)
import System.Exit
import System.Environment (getEnvironment)
import System.Posix.Unistd (getSystemID, nodeName)
import qualified Data.Map as M

import Graphics.X11 (Rectangle(..))
import Graphics.X11.Xinerama (getScreenInfo)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Master
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S

import Gaps
import Workspaces

myWorkspaces :: [Workspace]
myWorkspaces =
    [ Workspace "work"  "arch"     [ "Firefox", "Chromium", "Zathura" ]
    , Workspace "term"  "terminal" [ ]
    , Workspace "code"  "flask2"   [ ]
    , Workspace "chat"  "balloon"  [ "Empathy", "Pidgin" ]
    , Workspace "virt"  "wrench"   [ "VirtualBox" ]
    , Workspace "games" "ghost"    [ "Sol", "Pychess", "net-minecraft-LauncherFrame", "Wine" ]
    ]

myTerminal      = "urxvtc"
myBorderWidth   = 3
myModMask       = mod4Mask

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

empathy = ClassName "Empathy" `And` Role "contact_list"
pidgin  = ClassName "Pidgin"  `And` Role "buddy_list"


data AnyProfile = forall a. (Profile a) => AnyProfile a

data Vodik = Vodik
data Gmzlj = Gmzlj
data Beno = Beno

class Profile a where
    getIM :: a -> Property
    getIM _ = pidgin
    getIMWidth :: a -> Rational
    getIMWidth _ = 2/10
    getTermM :: a -> Int
    getTermM  _ = 1
    getWS :: a -> [Workspace]
    getWS _ = myWorkspaces

instance Profile Vodik where

instance Profile Gmzlj where
    getIMWidth _ = 3/10
    getWS      _ = filterWS "virt" myWorkspaces

instance Profile Beno where
    getIM      _ = empathy
    getTermM   _ = 2

instance Profile AnyProfile where
    getIM (AnyProfile a)      = getIM a
    getIMWidth (AnyProfile a) = getIMWidth a
    getTermM (AnyProfile a)   = getTermM a
    getWS (AnyProfile a)      = getWS a
    -- getIcons (AnyProfile a)   = getIcons a


to9 ws = to9' ws 1
    where
        to9' (x:xs) c = x : to9' xs (c + 1)
        to9' [] c | c < 10    = show c : to9' [] (c + 1)
                  | otherwise = []

myLayoutRules p = avoidStruts $
    lessBorders OnlyFloat $
    mkToggle (single NBFULL) $
    onWorkspace "work"  (tabs ||| wtabs ||| tiled ||| full) $
    onWorkspace "term"  (mtiled ||| tiled ||| full) $
    onWorkspace "chat"  (chat ||| tiled ||| full) $
    onWorkspace "virt"  full $
    onWorkspace "games" full $
    tiled ||| Mirror tiled ||| full
    where
        tabs   = noBorders $ tabbed shrinkText myTabTheme
        wtabs  = smartBorders $ mastered (2/100) (1/2) $ tabbed shrinkText myTabTheme
        tiled  = gaps 5 $ ResizableTall 1 (2/100) (1/2) []
        mtiled = gaps 5 $ Mirror $ ResizableTall (getTermM p) (2/100) (1/2) []
        chat   = withIM (getIMWidth p) (getIM p) $ gaps 5 $ GridRatio (2/3)
        full   = noBorders Full

myScratchPads =
    [ NS "term"  scratchpad (resource =? "scratchpad") scatchpadFloating
    , NS "steam" "steam"    (className =? "Wine")      defaultFloating
    ]
    where
        scratchpad = myTerminal ++ " -name scratchpad"
        scatchpadFloating = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

q ~? x = fmap (=~ x) q
myRules = (composeAll . concat $
    [ [ className =? c --> doCenterFloat | c <- floats ]
    , [ className ~? "^[Ll]ibre[Oo]ffice" --> doShift "work"
      , className =? "URxvt"              --> doF W.swapDown
      , className =? "Wine"               --> doFloat
      , resource  =? "desktop_window"     --> doIgnore
      , isFullscreen                      --> doFullFloat
      , isDialog                          --> doCenterFloat
      , (className =? "Firefox" <&&> role =? "Preferences") --> doCenterFloat
      ]
    ])
    where
        role   = stringProperty "WM_WINDOW_ROLE"
        floats = [ "Xmessage", "Mplayer", "Lxappearance", "Nitrogen"
                 , "Gcolor2", "Pavucontrol", "Nvidia-settings" ]

myKeys browser conf = mkKeymap conf $ concat
    [ [ ("M-<Return>", spawn $ XMonad.terminal conf)
      , ("M-w", spawn browser)
      , ("M-`", namedScratchpadAction myScratchPads "term")
      , ("M-p", shellPrompt myXPConfig)

      -- quit, or restart
      , ("M-S-q", io $ exitWith ExitSuccess)
      , ("M-S-c", kill)
      , ("M-q", restart "xmonad" True)

      -- layout
      , ("M-n",   sendMessage NextLayout)
      , ("M-S-n", sendMessage FirstLayout)
      , ("M-a",   sendMessage $ Toggle NBFULL)

      -- resizing
      , ("M-h", sendMessage Shrink)
      , ("M-l", sendMessage Expand)

      -- focus
      , ("M-j", windows W.focusDown)
      , ("M-k", windows W.focusUp)
      , ("M-m", windows W.focusMaster)
      , ("M-f", withFocused $ windows . W.sink)

      -- cycle windows
      , ("M-<Up>",      prevWS)
      , ("M-<Down>",    nextWS)
      , ("M-<Left>",    prevWS)
      , ("M-<Right>",   nextWS)
      , ("M-S-<Up>",    shiftToPrev >> prevWS)
      , ("M-S-<Down>",  shiftToNext >> nextWS)
      , ("M-S-<Left>",  shiftToPrev >> prevWS)
      , ("M-S-<Right>", shiftToNext >> nextWS)
      , ("M-<Tab>",     toggleWS' ["NSP"])

      -- swapping
      , ("M-S-m", windows W.shiftMaster)
      , ("M-S-j", windows W.swapDown)
      , ("M-S-k", windows W.swapUp)

      -- multimedia keys
      , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
      , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master on 5%+")
      , ("<XF86AudioMute>",        spawn "amixer -q set Master toggle")

      -- mpd controls
      , ("<XF86AudioPlay>", spawn "mpc toggle")
      , ("<XF86AudioNext>", spawn "mpc next")
      , ("<XF86AudioPrev>", spawn "mpc prev")

      -- screenshot
      , ("C-<Print>", spawn "sleep 0.1; scrot -s -e 'mv $f ~/pictures/screenshots/'")
      , ("<Print>",   spawn "scrot -e 'mv $f ~/pictures/screenshots/'")

      -- backlight hack
      , ("M-x", spawn "xbacklight -set 100%")
      ]
    , shiftWorkspaceKeys conf
    , [ ("M-C-w " ++ k, spawn $ unwords [ browser, f ]) | (k, f) <- favouritesList ]
    , [ ("M-s "   ++ k, S.promptSearch myXPConfig f)    | (k, f) <- searchList ]
    ]

shiftWorkspaceKeys conf =
    [ (m ++ [i], f w) | (i, w) <- zip ['1'..] $ workspaces conf
                      , (m, f) <- [ ("M-", greedyView'), ("M-S-", windows . W.shift) ]
    ]
    where
        greedyView' = toggleOrDoSkip ["NSP"] W.greedyView

searchList :: [(String, S.SearchEngine)]
searchList =
    [ ("g",   S.google)
    , ("w",   S.wikipedia)
    , ("y",   S.youtube)
    , ("h",   S.hoogle)
    , ("C-w", S.alpha)
    , ("C-a", S.amazon)
    , ("a",   S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
    , ("r",   S.searchEngine "reddit" "http://www.reddit.com/search?q=")
    , ("d",   S.searchEngine "wiktionary" "http://en.wiktionary.org/w/index.php/Special:Search?search=")
    , ("t",   S.searchEngine "piratebay" "http://thepiratebay.org/search/")
    ]

favouritesList :: [(String, String)]
favouritesList =
    [ ("w", "http://www.gmail.com")
    , ("r", "http://www.reddit.com")
    , ("a", "http://www.arstechnica.com")
    ]

myDzen (Rectangle x y sw sh) =
    "dzen2 -x "  ++ show x
      ++ " -w "  ++ show sw
      ++ " -y "  ++ show (sh - 16)
      ++ " -h "  ++ show 16
      ++ " -fn " ++ "'" ++ dzenFont ++ "'"
      ++ " -fg " ++ "'" ++ colorWhite ++ "'"
      ++ " -bg " ++ "'" ++ colorBlackAlt ++ "'"
      ++ " -ta l"
      ++ " -e 'onstart=lower'"

getHost :: IO AnyProfile
getHost = do
    hostName <- nodeName `fmap` getSystemID
    return $ case hostName of
        "vodik" -> AnyProfile Vodik
        "gmzlj" -> AnyProfile Gmzlj
        "beno"  -> AnyProfile Beno
        _       -> AnyProfile Vodik

main = do
    host    <- getHost
    home    <- fmap (fromMaybe "/home/simongmzlj" . lookup "HOME") getEnvironment
    browser <- getBrowser
    dzenbar <- spawnPipe . myDzen . head =<< getScreenInfo =<< openDisplay ""
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook         = manageHook defaultConfig <+> manageDocks <+> workspaceRules (getWS host) <+> myRules <+> namedScratchpadManageHook myScratchPads
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , layoutHook         = myLayoutRules host
        , logHook            = dynamicLogWithPP $ myPP home host dzenbar
        , modMask            = myModMask
        , keys               = myKeys browser
        , terminal           = myTerminal
        , borderWidth        = 2
        , normalBorderColor  = colorGray
        , focusedBorderColor = colorBlue
        , workspaces         = to9 $ map getWSName $ getWS host
        , focusFollowsMouse  = True
        }

myPP path profile output = defaultPP
    { ppCurrent         = dzenColor colorWhite    colorBlue     . iconify True icons path
    , ppUrgent          = dzenColor colorWhite    colorRed      . iconify True icons path
    , ppVisible         = dzenColor colorWhite    colorGray     . iconify True icons path
    , ppHidden          = dzenColor colorGrayAlt  colorGray     . iconify True icons path
    , ppHiddenNoWindows = dzenColor colorGray     colorBlackAlt . iconify False icons path
    , ppTitle           = dzenColor colorWhiteAlt colorBlackAlt . shorten 150
    , ppSep             = dzenColor colorBlue     colorBlackAlt "» "
    , ppSort            = fmap (. namedScratchpadFilterOutWorkspace) getSortByIndex
    , ppWsSep           = ""
    , ppLayout          = const ""
    , ppOrder           = \(ws:_:t:_) -> [ws,t]
    , ppOutput          = hPutStrLn output
    }
    where
        icons = getIconMap $ getWS profile

iconify v icons path c = maybe blank (wrapSpace . wrapIcon) $ M.lookup c icons
    where
        wrapSpace  = wrap " " " "
        wrapIcon i = "^i(" ++ path ++ "/etc/xmonad/icons/" ++ i ++ ".xbm) " ++ c
        blank | v         = wrapSpace c
              | otherwise = ""

myTabTheme = defaultTheme
    { decoHeight          = 18
    , inactiveBorderColor = colorGrayAlt
    , inactiveColor       = colorGray
    , inactiveTextColor   = colorGrayAlt
    , activeBorderColor   = colorGrayAlt
    , activeColor         = colorBlue
    , activeTextColor     = colorDarkGray
    , urgentBorderColor   = colorBlackAlt
    , urgentTextColor     = colorWhite
    }

myXPConfig = defaultXPConfig
    { font     = "xft:Envy Code R:size=11"
    , fgColor  = "#8cedff"
    , bgColor  = "black"
    , bgHLight = "black"
    , fgHLight = "#f92672"
    , position = Bottom
    , promptBorderWidth = 0
    }
