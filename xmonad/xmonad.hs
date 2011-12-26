import Control.Applicative ((<$>))
import Text.Regex.Posix ((=~))
import System.Exit
import qualified Data.Map as M

import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
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
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspace)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S

import Gaps
import Workspaces
import Tweaks

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

myLayoutRules p = avoidStruts
    $ lessBorders OnlyFloat
    $ mkToggle (single NBFULL)
    $ onWorkspace "work"  (tabs   ||| wtabs ||| tiled ||| full)
    $ onWorkspace "term"  (mtiled ||| tiled ||| full)
    $ onWorkspace "chat"  (chat   ||| tiled ||| full)
    $ onWorkspace "virt"  full
    $ onWorkspace "games" full
    $ tiled ||| Mirror tiled ||| full
    where
        tabs   = noBorders $ tabbed shrinkText myTabTheme
        wtabs  = smartBorders $ mastered (2/100) (1/2) $ tabbed shrinkText myTabTheme
        tiled  = gaps 5 $ ResizableTall 1 (2/100) (1/2) []
        mtiled = gaps 5 $ Mirror $ ResizableTall (masterN p) (2/100) (1/2) []
        chat   = withIM (imWidth p) (imClient p) $ gaps 5 $ GridRatio (imGrid p)
        full   = noBorders Full

q ~? x = fmap (=~ x) q
myRules ws = manageHook defaultConfig
    <+> manageDocks
    <+> scratchpadManageHook (W.RationalRect (1/6) (1/6) (2/3) (2/3))
    <+> workspaceRules ClassName ws
    <+> (composeAll . concat $
        [ [ className =? c --> doCenterFloat | c <- floats ]
        , [ className ~? "^[Ll]ibre[Oo]ffice" --> doShift "work"
          , className =? "Wine"               --> doFloat
          , isDialog                          --> doCenterFloat
          , isFullscreen                      --> doFullFloat
          , insertPosition Below Newer
          , resource  =? "desktop_window"     --> doIgnore
          , (className =? "Firefox" <&&> role =? "Preferences") --> doCenterFloat
          ]
        ])
    where
        role   = stringProperty "WM_WINDOW_ROLE"
        floats = [ "Xmessage", "Mplayer", "Lxappearance", "Nitrogen", "Gcolor2", "Pavucontrol", "Nvidia-settings" ]

myKeys browser conf = mkKeymap conf $ concat
    [ [ ("M-<Return>", spawn $ XMonad.terminal conf)
      , ("M-w", spawn browser)
      , ("M-`", scratchpadSpawnActionTerminal $ XMonad.terminal conf)
      , ("M-p", shellPrompt myXPConfig)

      -- quit, or restart
      , ("M-S-q", io $ exitWith ExitSuccess)
      , ("M-S-c", kill)
      , ("M-q",   restart "xmonad" True)

      -- layout
      , ("M-n",   sendMessage NextLayout)
      , ("M-S-n", sendMessage FirstLayout)
      , ("M-a",   sendMessage $ Toggle NBFULL)

      -- resizing
      , ("M-h", sendMessage Shrink)
      , ("M-l", sendMessage Expand)
      , ("M-,", sendMessage $ IncMasterN (-1))
      , ("M-.", sendMessage $ IncMasterN 1)

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

main = do
    tweaks  <- getTweaks
    browser <- getBrowser
    icons   <- getIconSet $ ws' tweaks
    dzenbar <- spawnPipe . myDzen . head =<< getScreenInfo =<< openDisplay ""
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook         = myRules $ ws' tweaks
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , layoutHook         = myLayoutRules tweaks
        , logHook            = dynamicLogWithPP $ myPP icons dzenbar
        , modMask            = myModMask
        , keys               = myKeys browser
        , terminal           = myTerminal
        , borderWidth        = 2
        , normalBorderColor  = colorGray
        , focusedBorderColor = colorBlue
        , workspaces         = to9 $ getWorkspaces $ ws' tweaks
        , focusFollowsMouse  = True
        }
    where
        ws' t = wsMod t myWorkspaces

myDzen :: Rectangle -> String
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

to9 :: [String] -> [String]
to9 ws = to9' ws 1
    where
        to9' (x:xs) c = x : to9' xs (c + 1)
        to9' [] c | c < 10    = show c : to9' [] (c + 1)
                  | otherwise = []

myPP icons output = defaultPP
    { ppCurrent         = dzenColor colorWhite    colorBlue     . iconify icons True
    , ppUrgent          = dzenColor colorWhite    colorRed      . iconify icons True
    , ppVisible         = dzenColor colorWhite    colorGray     . iconify icons True
    , ppHidden          = dzenColor colorGrayAlt  colorGray     . iconify icons True
    , ppHiddenNoWindows = dzenColor colorGray     colorBlackAlt . iconify icons False
    , ppTitle           = dzenColor colorWhiteAlt colorBlackAlt . shorten 150
    , ppSep             = dzenColor colorBlue     colorBlackAlt "» "
    , ppSort            = fmap (. namedScratchpadFilterOutWorkspace) getSortByIndex
    , ppWsSep           = ""
    , ppLayout          = const ""
    , ppOrder           = \(ws:_:t:_) -> [ws,t]
    , ppOutput          = hPutStrLn output
    }

iconify :: Icons -> Bool -> String -> String
iconify icons showAll c =
    maybe without (wrapSpace . (++ ' ' : c)) $ getIcon icons c
    where
        wrapSpace = wrap " " " "
        without | showAll   = wrapSpace c
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
