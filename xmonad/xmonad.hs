{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts,
             MultiParamTypeClasses, TypeSynonymInstances, CPP, DeriveDataTypeable #-}

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

-- myWorkspaces  = [ "work", "term", "code", "chat", "virt", "games" ] ++ map show [7..9]
-- myIcons       = [ "arch", "terminal", "flask2", "balloon", "wrench", "ghost" ]
myTerminal    = "urxvtc"
myBorderWidth = 2
myModMask     = mod4Mask

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


class Profile a where
    getIM :: a -> Property
    getIM _ = pidgin

    getWorkspaces :: a -> [String]
    getWorkspaces _ = [ "work", "term", "code", "chat", "virt", "games" ] ++ map show [7..9]

    getIcons :: a -> [String]
    getIcons _ = [ "arch", "terminal", "flask2", "balloon", "wrench", "ghost" ]

instance Profile String where
    getIM "beno" = empathy

    getWorkspaces "gmzlj" = [ "work", "term", "code", "chat", "games" ] ++ map show [6..9]
    getIcons      "gmzlj" = [ "arch", "terminal", "flask2", "balloon", "ghost" ]


myLayoutRules profile = avoidStruts $
    lessBorders OnlyFloat $
    mkToggle (single NBFULL) $
    onWorkspace "work"  (tabs ||| wtabs ||| tiled ||| tiled) $
    onWorkspace "term"  (mtiled ||| tiled ||| full) $
    onWorkspace "chat"  (chat ||| tiled ||| full) $
    onWorkspace "virt"  full $
    onWorkspace "games" full $
    tiled ||| Mirror tiled ||| full
    where
        tabs   = noBorders $ tabbed shrinkText myTabTheme
        wtabs  = smartBorders $ mastered (2/100) (1/2) $ tabbed shrinkText myTabTheme
        tiled  = gaps 5 $ ResizableTall 1 (2/100) (1/2) []
        mtiled = gaps 5 $ Mirror $ ResizableTall 2 (2/100) (1/2) []
        chat   = withIM (3/10) (getIM profile) $ gaps 5 $ GridRatio (2/3)
        full   = noBorders Full

q ~? x = fmap (=~ x) q
myRules = scratchpadManageHook (W.RationalRect 0.1 0.1 0.8 0.8) <+>
    (composeAll . concat $
    [ [ className =? c --> doCenterFloat   | c <- floats ]
    , [ className =? c --> doShift "work"  | c <- work ]
    , [ className =? c --> doShift "chat"  | c <- chat ]
    , [ className =? c --> doShift "virt"  | c <- virt ]
    , [ className =? c --> doShift "games" | c <- games ]
    , [ className =? "URxvt"              --> doF W.swapDown
      , className =? "Wine"               --> doFloat
      , className ~? "^[Ll]ibre[Oo]ffice" --> doShift "work"
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
        work   = [ "Firefox", "Chromium", "Zathura" ]
        chat   = [ "Empathy", "Pidgin" ]
        virt   = [ "VirtualBox" ]
        games  = [ "Sol", "Pychess", "net-minecraft-LauncherFrame"
                 , "Wine" ]

myKeys browser conf = mkKeymap conf $ concat
    [ [ ("M-<Return>", spawn $ XMonad.terminal conf)
      , ("M-w", spawn browser)
      , ("M-`", scratchpadSpawnActionTerminal $ XMonad.terminal conf)
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

main = do
    host    <- fmap nodeName getSystemID
    home    <- fmap (fromMaybe "/home/simongmzlj" . lookup "HOME") getEnvironment
    browser <- getBrowser
    dzenbar <- spawnPipe . myDzen . head =<< getScreenInfo =<< openDisplay ""
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook         = manageHook defaultConfig <+> manageDocks <+> myRules
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , layoutHook         = myLayoutRules host
        , logHook            = dynamicLogWithPP $ myPP home host dzenbar
        , modMask            = myModMask
        , keys               = myKeys browser
        , terminal           = myTerminal
        , borderWidth        = 2
        , normalBorderColor  = colorGray
        , focusedBorderColor = colorBlue
        , workspaces         = getWorkspaces host
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
        icons = M.fromList $ zip (getWorkspaces profile) (getIcons profile)

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
