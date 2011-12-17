import Text.Regex.Posix ((=~))
import System.Exit
import System.Directory (getCurrentDirectory)

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
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S

import Gaps
import Dzen2

myTerminal    = "urxvtc"
myBorderWidth = 2
myModMask     = mod4Mask
myWorkspaces  = [ "work", "term", "code", "chat", "virt", "games" ] ++ map show [7..9]
myNormalBorderColor  = "#333333"
myFocusedBorderColor = "#bf1e2d"

myLayoutRules = avoidStruts $
    lessBorders OnlyFloat $
    onWorkspace "work"  (tabbed ||| tiled) $
    onWorkspace "virt"  full $
    onWorkspace "games" full $
    tiled ||| Mirror tiled ||| full
    where
        tiled  = gaps 5 $ ResizableTall 1 (2/100) (1/2) []
        full   = noBorders Full
        tabbed = noBorders $ tabbedBottom shrinkText myTheme

q ~? x = fmap (=~ x) q
myRules = scratchpadManageHook (W.RationalRect 0.1 0.1 0.8 0.8) <+>
    (composeAll . concat $
    [ [ className =? c --> doCenterFloat   | c <- floats ]
    , [ className =? c --> doShift "work"  | c <- work ]
    , [ className =? c --> doShift "virt"  | c <- virt ]
    , [ className =? c --> doShift "games" | c <- games ]
    , [ className ~? "^[Ll]ibre[Oo]ffice" --> doShift "work"
      , resource  =? "desktop_window"     --> doIgnore
      , isFullscreen                      --> doFullFloat
      , isDialog                          --> doCenterFloat
      , (className =? "Firefox" <&&> role =? "Preferences") --> doCenterFloat
      ]
    ])
    where
        role   = stringProperty "WM_WINDOW_ROLE"
        floats = [ "Xmessage", "Mplayer", "Lxappearance", "Nitrogen", "Gcolor2", "Pavucontrol" ]
        work   = [ "Firefox", "Chromium", "Zathura" ]
        virt   = [ "VirtualBox" ]
        games  = [ "Sol", "Pychess", "net-minecraft-LauncherFrame" ]

myKeys browser conf = mkKeymap conf $
    -- terminal
    [ ("M-<Return>", spawn $ XMonad.terminal conf)
    , ("M-`", scratchpadSpawnActionTerminal $ XMonad.terminal conf)
    , ("M-p", shellPrompt myXPConfig)

    -- browser
    , ("M-w",     spawn browser)
    , ("M-C-w w", spawn $ unwords [ browser, "http://www.gmail.com" ])
    , ("M-C-w r", spawn $ unwords [ browser, "http://www.reddit.com" ])
    , ("M-C-w a", spawn $ unwords [ browser, "http://www.arstechnica.com" ])

    -- quit, or restart
    , ("M-S-q", io $ exitWith ExitSuccess)
    , ("M-S-c", kill)
    , ("M-q", restart "xmonad" True)

    -- layout
    , ("M-n",   sendMessage NextLayout)
    , ("M-S-n", sendMessage FirstLayout)

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
    , ("M-S-m", windows W.swapMaster)
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
    ++
    [ ("M-s " ++ k, S.promptSearch myXPConfig f) | (k, f) <- searchList ]
    ++
    [ (m ++ [i], f w) | (i, w) <- zip ['1'..] $ workspaces conf
                      , (m, f) <- [ ("M-", windows . W.greedyView)
                                  , ("M-S-", windows . W.shift)
                                  ]
    ]

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

dzenFont = "-*-envy code r-medium-r-normal-*-11-*-*-*-*-*-*-*"
colorBlack          = "#000000"
colorBlackAlt       = "#050505"
colorGray           = "#484848"
colorGrayAlt        = "#b8bcb8"
colorDarkGray       = "#161616"
colorWhite          = "#ffffff"
colorWhiteAlt       = "#9d9d9d"
colorDarkWhite      = "#444444"
colorMagenta        = "#8e82a2"
colorMagentaAlt     = "#a488d9"
colorBlue           = "#60a0c0"
colorBlueAlt        = "#007b8c"
colorRed            = "#d74b73"

myDzen (Rectangle x y sw sh) =
    "dzen2 -x "  ++ show x
      ++ " -w "  ++ show sw
      ++ " -y "  ++ (show $ sh - 16)
      ++ " -h "  ++ show 16
      ++ " -fn " ++ "'" ++ dzenFont ++ "'"
      ++ " -fg " ++ "'" ++ colorWhite ++ "'"
      ++ " -bg " ++ "'" ++ colorBlackAlt ++ "'"
      ++ " -ta l"
      ++ " -e 'onstart=lower'"

main = do
    cwd     <- getCurrentDirectory
    browser <- getBrowser
    xmproc  <- spawnPipe . myDzen . head =<< getScreenInfo =<< openDisplay ""
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { manageHook = manageHook defaultConfig <+> manageDocks <+> myRules
        , handleEventHook = docksEventHook <+> fullscreenEventHook
        , layoutHook = myLayoutRules
        , logHook = dynamicLogWithPP $ myPP cwd xmproc
        , modMask = myModMask
        , keys = myKeys browser
        , terminal = myTerminal
        , borderWidth = 2
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        , focusFollowsMouse = True
        }

myPP path output = defaultPP
    { ppCurrent         = dzenColor colorWhite colorBlue . iconify True path
    , ppUrgent          = dzenColor colorWhite colorRed . iconify True path
    , ppVisible         = dzenColor colorWhite colorGray . iconify True path
    , ppHidden          = dzenColor colorGrayAlt colorGray . iconify True path
    , ppHiddenNoWindows = dzenColor colorGray colorBlackAlt . iconify False path
    , ppTitle           = dzenColor colorWhiteAlt colorBlackAlt . shorten 150
    , ppSep             = " » "
    , ppSort            = fmap (namedScratchpadFilterOutWorkspace.) (ppSort xmobarPP)
    , ppWsSep           = ""
    , ppLayout          = const ""
    , ppOrder           = \(ws:_:t:_) -> [ws,t]
    , ppOutput          = hPutStrLn output
    }

iconify v path c
    | c == "work" = icon "arch"
    | c == "term" = icon "terminal"
    | c == "code" = icon "binder"
    | c == "chat" = icon "balloon"
    | c == "virt" = icon "wrench"
    | c == "games" = icon "ghost"
    | v == False  = ""
    | otherwise   = wrap " " " " c
    where
        icon i = " ^i(" ++ path ++ "/etc/xmonad/icons/" ++ i ++ ".xbm) " ++ c ++ " "

myTheme = defaultTheme
    { decoHeight = 18
    , activeColor       = "#bf1e2d"
    , activeBorderColor = "#ff0000"
    , activeTextColor   = "#000000"
    }

myXPConfig = defaultXPConfig
    { font     = "xft:Envy Code R:size=9"
    , fgColor  = "#8cedff"
    , bgColor  = "black"
    , bgHLight = "black"
    , fgHLight = "#f92672"
    , position = Bottom
    , promptBorderWidth = 0
    }
