import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Ratio
import System.Directory
import System.Exit
import System.Posix.Env
import System.Posix.Unistd (getSystemID, nodeName)
import qualified Data.Map as M
import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD

import XMonad hiding (spawn)
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS hiding (shiftTo, moveTo, toggleWS)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.VodikLog
import XMonad.Layout.BalancedTile
import XMonad.Layout.Grid
import XMonad.Layout.GuardLayout
import XMonad.Layout.GuardLayout.Instances
import XMonad.Layout.Master
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.SortWindows
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts hiding (Toggle)
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowGaps
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util
import XMonad.Util.Commands
import XMonad.Util.Cursor
import XMonad.Util.CycleWS
import XMonad.Util.Environment
import XMonad.Util.EZConfig
import XMonad.Util.MPD
import XMonad.Util.NamedScratchpad
import XMonad.Util.RunOnce
import XMonad.Util.Scratchpad
import XMonad.Util.Services
import XMonad.Util.TagBuilder
import XMonad.Util.Tmux
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Actions.Search as S

scratchpads :: NamedScratchpads
scratchpads =
    [ NS "scratchpad" scratchpad    (role      =? "scratchpad")  nonFloating
    , NS "volume"     "pavucontrol" (className =? "Pavucontrol") nonFloating
    ]
  where
    scratchpad = "TASK=show termite -r scratchpad"

tmuxSessions :: TmuxSessions
tmuxSessions = [ TS "irc" "weechat-curses" ]

myTerminal     = "termite"
myBorderWidth  = 2
myModMask      = mod4Mask

xftFont        = "xft:Envy Code R:size=9"
colorBlack     = "#000000"
colorGray      = "#484848"
colorGrayAlt   = "#b8bcb8"
colorDarkGray  = "#161616"
colorWhite     = "#ffffff"
colorWhiteAlt  = "#9d9d9d"
colorBlue      = "#439dcA"
colorRed       = "#f54669"

-- Layouts {{{1
myLayoutRules sort tw = avoidStruts . lessBorders OnlyFloat . tfull
    . onWorkspace "work"  (mstr tabs ||| tiled)
    . onWorkspace "term"  (mtiled ||| tiled)
    . onWorkspace "chat"  full
    . onWorkspace "virt"  full
    . onWorkspace "games" full
    $ tiled ||| Mirror tiled
  where
    tfull  = toggleLayouts . tag "Triggered" $ noBorders Full
    mstr l = smartBorders $ ifWider 1200 (work ||| l) l
    work   = tag "Work" $ sortQuery "work" True step (mainWidth tw) sort tabs tabs
    tabs   = trackFloating $ tabbed shrinkText myTabTheme
    tiled  = gaps 5 $ BalancedTall 2 step (11 % 20) [ 31 % 25 ]
    mtiled = gaps 5 . Mirror $ BalancedTall (masterN tw) step (1/2) [ 31 % 25 ]
    panel  = ifTaller 1024 Grid tabs
    full   = noBorders Full
    tag t  = renamed [ PrependWords t ]
    step   = 1 % 50

-- Rules {{{1
myRules ws rect = manageDocks
    <> composeOne
        [ workspaceShift ws
        , role      =? "scratchpad"       -?> customFloating rect
        , className =? "Transmission-gtk" -?> doShift "work"
        ]
    <> composeOneCaught (insertPosition Below Newer)
        [ className =? "Gvim"         -?> idHook
        , className =? "Wine"         -?> doFloat
        , className `queryAny` floats -?> doCenterFloat
        , isDialog                    -?> doCenterFloat
        , isFirefoxWindow             -?> doCenterFloat
        , isFullscreen                -?> doFullFloat
        ]
  where
    isFirefoxWindow = do
        browser <- className `queryAny` [ "Firefox", "Aurora" ]
        if browser
            then role `queryNone` [ "browser", "view-source", "manager" ]
            else return False
    floats = [ "Xmessage", "Pinentry-gtk-2", "MPlayer", "Lxappearance", "Nitrogen", "Qtconfig"
             , "Gcolor2", "Pavucontrol", "Nvidia-settings", "Arandr", "Rbutil", "zsnes"
             , "Dwarf_Fortress" ]

-- Startup {{{1
myStartupHook sort = setDefaultCursor xC_left_ptr
    <> runOnce initHook
    <> setQuery "work" sort
    <> startService "pulse"   "start-pulseaudio-x11"
    <> startService "notify"  "dunst"
    <> startService "udiskie" "udiskie"
  where
    initHook = do
        spawn $ "nitrogen" :+ [ "--restore" ]
        spawn "topbar"

-- Keymap {{{1
myKeys ws browser conf = mkKeymap conf $
    [ ("M-<Return>", spawn $ terminal conf)

    , ("M-w",  spawn browser)
    , ("M-\\", tmuxPrompt tmuxSessions myXPConfig)
    , ("M-p",  shellPrompt myXPConfig)

    , ("<XF86Launch1>", spawn browser)

    -- scratchpads
    , ("M-`", namedScratchpadAction scratchpads "scratchpad")
    , ("M-v", namedScratchpadAction scratchpads "volume")

    -- quit, close or restart
    , ("M-S-q",   io exitSuccess)
    , ("M-S-c",   kill1)
    , ("M-C-c",   kill)
    , ("M-S-C-c", spawn "xkill")
    , ("M-q",     restart "xmonad" True)

    -- layout
    , ("M-<Space>",   sendMessage NextLayout)
    , ("M-S-<Space>", sendMessage FirstLayout)
    , ("M-f",         sendMessage ToggleLayout)

    -- resizing
    , ("M-[",   sendMessage Shrink)
    , ("M-]",   sendMessage Expand)
    , ("M-S-[", sendMessage MirrorExpand)
    , ("M-S-]", sendMessage MirrorShrink)
    , ("M-,",   sendMessage $ IncMasterN (-1))
    , ("M-.",   sendMessage $ IncMasterN 1)

    -- focus
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-m", windows W.focusMaster)
    , ("M-t", withFocused' $ windows . W.sink)
    , ("M-a", focusUrgent)

    -- swapping
    , ("M-S-m", windows W.shiftMaster)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-0",   sendMessage SwapWindow)

    -- cycle workspaces
    , ("M-<D>",   moveTo Next skip)
    , ("M-<U>",   moveTo Prev skip)
    , ("M-<R>",   moveToNonEmpty Next skip)
    , ("M-<L>",   moveToNonEmpty Prev skip)
    , ("M-S-<D>", shiftTo Next skip)
    , ("M-S-<U>", shiftTo Prev skip)
    , ("M-S-<R>", shiftToEmpty Next skip)
    , ("M-S-<L>", shiftToEmpty Prev skip)
    , ("M-<Tab>", toggleWS [ "NSP" ])

    -- misc keybinds against alt
    , ("M1-C-l", spawn "slock")

    -- screenshots
    , ("<Print>",   delayedSpawn 100 $ "scrot" :+ [ scrotDir ])
    , ("C-<Print>", delayedSpawn 100 $ "scrot" :+ [ "-s", scrotDir ])

    -- media keys
    , ("<XF86AudioPlay>",        withMPD MPD.toggle)
    , ("<XF86AudioStop>",        withMPD MPD.stop)
    , ("<XF86AudioPrev>",        withMPD MPD.previous)
    , ("<XF86AudioNext>",        withMPD MPD.next)
    , ("<XF86AudioMute>",        spawn $ "ponymix" :+ [ "toggle" ])
    , ("<XF86AudioLowerVolume>", spawn $ "ponymix" :+ [ "decrease", "3" ])
    , ("<XF86AudioRaiseVolume>", spawn $ "ponymix" :+ [ "increase", "3" ])

    -- for happy hacking keyboard
    , ("M-<F1>",  withMPD MPD.previous)
    , ("M-<F2>",  withMPD MPD.toggle)
    , ("M-<F3>",  withMPD MPD.next)
    , ("M-<F10>", spawn $ "ponymix" :+ [ "toggle" ])
    , ("M-<F11>", spawn $ "ponymix" :+ [ "decrease", "3" ])
    , ("M-<F12>", spawn $ "ponymix" :+ [ "increase", "3" ])
    ]
    <> wsSwitchKeys (tagSet ws)
    <> searchKeys
  where
    skip     = skipWS [ "NSP" ]
    scrotDir = "/home/simongmzlj/pictures/screenshots/%Y-%m-%d_%H:%M:%S_$wx$h.png"

wsSwitchKeys tags = namedTags <> moreTags
  where
    namedTags = [ (m <> i, f w) | (i, w) <- zip (show <$> [1..]) tags, (m, f) <- keymap "M-"   ]
    moreTags  = [ (m <> i, f i) | i      <- show <$> [1..9],           (m, f) <- keymap "M-C-" ]
    keymap p =
        [ (p,         toggleOrDoSkip [ "NSP" ] W.greedyView)
        , (p <> "S-", windows . W.shift)
        ]

searchKeys = [ ("M-s " <> k, S.promptSearch myXPConfig f) | (k, f) <- searchList ]
  where
    searchList =
        [ ("g", S.google)
        , ("w", S.wikipedia)
        , ("y", S.youtube)
        , ("h", S.hoogle)
        , ("a", S.alpha)
        , ("d", S.searchEngine "wiktionary" "http://en.wiktionary.org/w/index.php/Special:Search?search=")
        , ("t", S.searchEngine "piratebay" "http://thepiratebay.org/search/")
        ]

-- Mouse {{{1
myMouseBindings conf@(XConfig {modMask = modm}) =
    (`M.union` mouseBindings defaultConfig conf) $ M.fromList
        [ ((modm,               button2), killWindow)
        , ((modm,               button3), \w -> focus w >> Flex.mouseResizeWindow w)
        , ((modm,               button4), const $ moveTo  Prev skip)
        , ((modm,               button5), const $ moveTo  Next skip)
        , ((modm .|. shiftMask, button4), const $ shiftTo Prev skip)
        , ((modm .|. shiftMask, button5), const $ shiftTo Next skip)
        ]
  where
    skip = skipWS [ "NSP" ]

-- Themes {{{1
myTabTheme = defaultTheme
    { decoHeight          = 18
    , inactiveBorderColor = colorBlack
    , inactiveColor       = colorGray
    , inactiveTextColor   = colorGrayAlt
    , activeBorderColor   = colorBlack
    , activeColor         = colorBlue
    , activeTextColor     = colorDarkGray
    , urgentBorderColor   = colorBlack
    , urgentColor         = colorRed
    , urgentTextColor     = colorDarkGray
    }

myXPConfig = defaultXPConfig
    { font              = xftFont
    , fgColor           = colorBlue
    , bgColor           = colorBlack
    , bgHLight          = colorBlack
    , fgHLight          = colorRed
    , promptBorderWidth = 0
    , position          = Bottom
    }

myVodikConfig = VodikConfig
    { dzenFont     = "-*-envy code r-medium-r-normal-*-12-*-*-*-*-*-*-*"
    , dzenBlack    = colorBlack
    , dzenWhite    = colorWhite
    , dzenWhiteAlt = colorWhiteAlt
    , dzenGray     = colorGray
    , dzenGrayAlt  = colorGrayAlt
    , dzenBlue     = colorBlue
    , dzenRed      = colorRed
    }
-- }}}

getMachine = buildTags $ do
    host <- nodeName <$> io getSystemID

    tag "work" [ className `queryAny` [ "Aurora", "Firefox", "Chromium", "Zathura" ]
               , title     =? "MusicBrainz Picard"
               , className ~? "^[Ll]ibre[Oo]ffice" ]

    tag "term" []
    tag "code" []
    tag "chat" [ role =? "irc" ]
    unless (host == "omg") $ tag "virt" [ className =? "VirtualBox" ]
    tag "games" []

main = do
    machine <- getMachine
    screen  <- getScreen
    browser <- browser "firefox"

    -- let tweaks  = getTweaks machine
    let tweaks = defaultTweaks
        sort   = workspaceSort "work" machine
        pos    = positionRationalRect 16 screen

    xmonad . applyUrgency colorRed =<< dzenVodik myVodikConfig defaultConfig
        { manageHook         = myRules machine pos
        , handleEventHook    = docksEventHook <> fullscreenEventHook
        , layoutHook         = myLayoutRules sort tweaks
        , startupHook        = myStartupHook sort
        , modMask            = myModMask
        , keys               = myKeys machine browser
        , mouseBindings      = myMouseBindings
        , workspaces         = tagSet machine <> fmap show [1..9]
        , terminal           = myTerminal
        , borderWidth        = myBorderWidth
        , normalBorderColor  = colorGray
        , focusedBorderColor = colorBlue
        , focusFollowsMouse  = True
        }

-- vodikTweaks = defaultTweaks
--     { mainWidth  = 2/3
--     }

-- gmzljTweaks = defaultTweaks
--     { imWidth    = 1/4
--     , imGrid     = 3/2
--     , wsModifier = filterWS "virt"
--     }

-- benoTweaks = defaultTweaks
--     { masterN  = 3
--     }
