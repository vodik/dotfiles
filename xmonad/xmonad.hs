-- XMonad modules

import qualified Data.Map as M
import Data.Ratio ((%))
import Text.Regex.Posix ((=~))
import System.Exit

import XMonad
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Prompt.Shell
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.Run

import Gaps

myWorkspaces = [ "web", "term", "code", "chat", "doc", "games", "stuff" ]
myBrowser = "firefox"
myTerm = "urxvtc"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad  $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook = myManageHook
        , layoutHook = myLayoutRules
        , logHook = dynamicLogWithPP $ myPP xmproc
        , modMask = mod4Mask
        , keys = myKeys
        , terminal = myTerm
        , borderWidth = 2
        , normalBorderColor = "#333333"
        , focusedBorderColor = "#bf1e2d"
        , workspaces = myWorkspaces
        , focusFollowsMouse = True
        }

myLayoutRules =
    onWorkspace "web" tabbedLayout $
    onWorkspace "doc" tabbedLayout $
    standardLayouts

standardLayouts = avoidStruts $
    tiled ||| reflectTiled ||| Mirror tiled ||| Grid ||| full

-- Layouts
tiled        = gaps 5 $ smartBorders $ ResizableTall 1 (2/100) (1/2) []
reflectTiled = reflectHoriz tiled
full         = noBorders Full
tabLayout    = tabbedBottom shrinkText myTheme

-- web and doc
tabbedLayout = avoidStruts $ smartBorders $
    tabLayout ||| tiled ||| full

myManageHook :: ManageHook
myManageHook = composeAll
    [ manageHook defaultConfig
    , manageDocks
    , myRules
    ]

q ~? x = fmap (=~ x) q
myRules = scratchpadManageHook (W.RationalRect 0.1 0.1 0.8 0.8)
    <+> (composeAll . concat $
        [ [ className =? c --> doCenterFloat   | c <- floats ]
        , [ className =? c --> doShift "web"   | c <- web ]
        , [ className =? c --> doShift "doc"   | c <- doc ]
        , [ className =? c --> doShift "games" | c <- games ]
        , [ className ~? "^[Ll]ibre[Oo]ffice" --> doShift "doc"
          , resource  =? "desktop_window"     --> doIgnore
          , isFullscreen                      --> doFullFloat
          , isDialog                          --> doCenterFloat
          ]
        ])
    <+> manageDocks

floats = [ "Xmessage", "Mplayer", "Lxappearance", "Nitrogen" ]
web    = [ "Firefox" ]
doc    = [ "Zathura" ]
games  = [ "Sol", "net-minecraft-LauncherFrame"]

myKeys conf = mkKeymap conf $
    -- terminal
    [ ("M-<Return>", spawn $ XMonad.terminal conf)
    , ("M-w", spawn $ myBrowser)
    , ("M-S-w", spawn $ unwords [ myBrowser, "http://www.gmail.com" ])
    -- , ("M-S-w", spawn $ unwords [ myBrowser, "http://www.reddit.com" ])
    -- , ("M-S-w", spawn $ unwords [ myBrowser, "http://www.arstechnica.com" ])
    , ("M-e", scratchpadSpawnActionTerminal $ XMonad.terminal conf)

    -- quit, or restart
    , ("M-C-q", io (exitWith ExitSuccess))
    , ("M-S-c", kill)
    , ("M-S-q", restart "xmonad" True)

    -- screenshot
    , ("C-<Print>", spawn "sleep 0.2; scrot -s -e 'mv $f ~/images/screenshots/'")
    , ("<Print>", spawn "scrot -e 'mv $f ~/images/screenshots/'")

    -- launcher
    , ("M-<Space>", shellPrompt myXPConfig)

    -- layout
    , ("M-n", sendMessage NextLayout)
    , ("M-S-n", sendMessage FirstLayout)

    -- focus
    , ("M-<Tab>", windows W.focusDown)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-m", windows W.focusMaster)

    -- cycle windows
    , ("M-<Up>",      prevWS)
    , ("M-<Down>",    nextWS)
    , ("M-<Left>",    prevWS)
    , ("M-<Right>",   nextWS)
    , ("M-S-<Up>",    shiftToPrev >> prevWS)
    , ("M-S-<Down>",  shiftToNext >> nextWS)
    , ("M-S-<Left>",  shiftToPrev >> prevWS)
    , ("M-S-<Right>", shiftToNext >> nextWS)
    , ("M-q",         toggleWS' ["NSP"])

    -- swapping
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)

    -- resizing
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)

    -- multimedia keys
    , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master on 5%+")
    , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")

    -- backlight hack
    , ("M-x", spawn "xbacklight -set 100%")
    ]
    ++
    [ ("M-s " ++ k, S.promptSearch myXPConfig f) | (k, f) <- searchList ]
    ++
    [ (m ++ [i], f w) | (i, w) <- zip ['1'..] $ workspaces conf
                      , (m, f) <- [ ("M-", windows . W.greedyView)
                                  , ("M-S-", windows . W.shift)
                                  ]
    ]

searchList :: [([Char], S.SearchEngine)]
searchList = [ ("g",   S.google)
             , ("w",   S.wikipedia)
             , ("r",   S.searchEngine "reddit" "http://www.reddit.com/search?q=")
             , ("S-w", S.alpha)
             , ("y",   S.youtube)
             , ("S-a", S.amazon)
             , ("a",   S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
             , ("h",   S.hoogle)
             , ("i",   S.isohunt)
             , ("d",   S.searchEngine "wiktionary" "http://en.wiktionary.org/w/index.php/Special:Search?search=")
             ]

-- themeing for tab layout
myTheme = defaultTheme
    { decoHeight = 18
    , activeColor = "#bf1e2d"
    , activeBorderColor = "#ff0000"
    , activeTextColor = "#000000"
    , inactiveBorderColor = "#000000"
    }

myPP output = defaultPP
    { ppCurrent = xmobarColor "#7B79B1" "#0F141F" . wrap "[" "]"
    , ppVisible = wrap "(" ")"
    , ppHiddenNoWindows = const ""
    , ppSep    = " -> "
    , ppTitle  = xmobarColor "#7B79B1" "" . shorten 50
    , ppUrgent = xmobarColor "#2BA624" "0FA3A3"
    , ppWsSep  = " "
    , ppLayout = const ""
    , ppOrder  = \(ws:_:t:_) -> [ws,t]
    , ppOutput = hPutStrLn output
    }

-- some nice colors for the prompt
myXPConfig = defaultXPConfig
    { font     = "xft:Envy Code R:size=9"
    , fgColor  = "#00FFFF"
    , bgColor  = "#151515"
    , bgHLight = "#151515"
    , fgHLight = "#FF0000"
    , position = Bottom
    }
