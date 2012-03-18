import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import System.Environment
import System.Exit
import System.IO
import System.Posix.Unistd (getSystemID, nodeName)
import qualified Data.Map as M

import Graphics.X11.Xinerama (getScreenInfo)

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS hiding (moveTo, shiftTo, toggleWS)
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.Master
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.TrackFloating
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S

import Machine
import BalancedTile
import CycleWS
import Dzen2
import Gaps
import GuardLayout
import GuardLayout.Instances
import SortWindows
import Workspaces
import Utils

-- spawnShell :: X ()
-- spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

-- spawnShellIn :: Dir -> X ()
-- spawnShellIn dir = asks (terminal . config) >>= \t -> spawn $ "cd " ++ dir ++ " && exec " ++ t

imClients :: Query Any
imClients = composeAs Any
    [ className =? "Empathy" <&&> role =? "contact_list"
    , className =? "Pidgin"  <&&> role =? "buddy_list"
    , className =? "Skype"   <&&> title `prefixed` "Skype"
    ]

myFloats :: Query Bool
myFloats =
    className `queryAny` [ "Xmessage", "MPlayer", "Lxappearance", "Nitrogen", "Qtconfig", "Gcolor2", "Pavucontrol"
                         , "Nvidia-settings", "Arandr", "Rbutil", "zsnes", "Dwarf_Fortress" ]

myTerminal    = "urxvtc"
myBorderWidth = 2
myModMask     = mod4Mask

xftFont         = "xft:Envy Code R:size=9"
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

myLayoutRules sort tw = avoidStruts . lessBorders OnlyFloat . mkToggle (single TNBFULL)
    . onWorkspace "work"  (mstr tabs ||| tiled)
    . onWorkspace "term"  (mtiled ||| tiled)
    . onWorkspace "chat"  (tag "IM" . sortIM $ tabs ||| grid)
    . onWorkspace "virt"  full
    . onWorkspace "games" full
    $ tiled ||| Mirror tiled
  where
    mstr l = smartBorders $ ifWider 1200 (work ||| l) l
    work   = tag "Work" $ sortQuery "work" True step (mainWidth tw) sort tabs tabs
    tabs   = trackFloating $ tabbed shrinkText myTabTheme
    tiled  = gaps 5 $ BalancedTall 2 step (11/20) []
    mtiled = gaps 5 . Mirror $ BalancedTall (masterN tw) step (1/2) []
    sortIM = sortQuery "chat" False step (imWidth tw) imClients panel
    panel  = ifTaller 1024 Grid tabs
    grid   = gaps 5 $ GridRatio (imGrid tw)
    full   = noBorders Full
    tag t  = renamed [ PrependWords t ]
    step   = 1/50

myRules ws rect = manageDocks
    <+> scratchpadManageHook rect
    -- <+> workspaceShift ws
    <+> composeAll
        [ className =? "Transmission-gtk" --> doShift "work"
        , className =? "MPlayer"          --> doCopy [ "NSP" ]
        , resource  =? "desktop_window"   --> doIgnore
        ]
    <+> composeOneCaught (insertPosition Below Newer)
        [ className =? "Wine"  -?> doFloat
        , myFloats             -?> doCenterFloat
        , isFirefoxPreferences -?> doCenterFloat
        , isDialog             -?> doCenterFloat
        , isFullscreen         -?> doFullFloat
        ]

myStartupHook = setDefaultCursor xC_left_ptr
    <+> setQuery "chat" imClients
    -- <+> setQuery "work" workSort
    <+> startServices [ "urxvtd", "udiskie", "mpd" ]
  where
    -- workSort = workspaceSort $ head myWorkspaces

myKeys browser conf = mkKeymap conf $ concat
    [ [ ("M-<Return>", spawn $ terminal conf)
      -- , ("M-S-<Return>", currentTopicAction myTopicConfig)
      , ("M-w", spawn browser)
      , ("M-`", scratchpadSpawnActionTerminal $ terminal conf)
      , ("M-p", shellPrompt myXPConfig)

      -- quit, or restart
      , ("M-S-q", io $ exitWith ExitSuccess)
      , ("M-S-c", kill1)
      , ("M-C-c", kill)
      , ("M-q",   restart "xmonad" True)

      -- layout
      , ("M-<Space>",   sendMessage NextLayout)
      , ("M-S-<Space>", sendMessage FirstLayout)
      , ("M-a",         sendMessage $ Toggle TNBFULL)

      -- resizing
      , ("M-h",   sendMessage Shrink)
      , ("M-l",   sendMessage Expand)
      , ("M-S-h", sendMessage MirrorExpand)
      , ("M-S-l", sendMessage MirrorShrink)
      , ("M-,",   sendMessage $ IncMasterN (-1))
      , ("M-.",   sendMessage $ IncMasterN 1)

      -- focus
      , ("M-j", windows W.focusDown)
      , ("M-k", windows W.focusUp)
      , ("M-m", windows W.focusMaster)
      , ("M-f", withFocused' $ windows . W.sink)
      , ("M-d", focusUrgent)

      -- swapping
      , ("M-S-m", windows W.shiftMaster)
      , ("M-S-j", windows W.swapDown)
      , ("M-S-k", windows W.swapUp)
      , ("M-0",   sendMessage SwapWindow)

      -- cycle workspaces
      , ("M-<Down>",    moveTo Next skipWS)
      , ("M-<Up>",      moveTo Prev skipWS)
      , ("M-<Right>",   moveToNonEmpty Next skipWS)
      , ("M-<Left>",    moveToNonEmpty Prev skipWS)
      , ("M-S-<Down>",  shiftTo Next skipWS)
      , ("M-S-<Up>",    shiftTo Prev skipWS)
      , ("M-S-<Right>", shiftToEmpty Next skipWS)
      , ("M-S-<Left>",  shiftToEmpty Prev skipWS)
      , ("M-<Tab>",     toggleWS skipWS)
      , ("M-C-0",       toggleCopy skipWS)

      -- misc keybinds against alt
      , ("M1-`",   goToSelected myGSConfig)
      , ("M1-C-l", spawn "slock")
      , ("M1-S-l", delayedSpawn 1500 "xset dpms force off")

      -- multimedia keys
      , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 3%-")
      , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master on 3%+")
      , ("<XF86AudioMute>",        spawn "amixer -q set Master toggle")

      -- mpd controls
      , ("<XF86AudioPlay>", spawn "mpc toggle")
      , ("<XF86AudioStop>", spawn "mpc stop")
      , ("<XF86AudioNext>", spawn "mpc next")
      , ("<XF86AudioPrev>", spawn "mpc prev")

      -- screenshot
      , ("C-<Print>", delayedSpawn 100 "scrot -s -e 'mv $f ~/pictures/screenshots/'")
      , ("<Print>",   spawn "scrot -e 'mv $f ~/pictures/screenshots/'")

      -- HACKS: backlight hack, restore screen resolution
      , ("M-x z", spawn "xrandr -s 0")
      , ("M-x x", spawn "xbacklight -set 100%")
      ]
    -- , [ (m ++ i, f w) | (i, w) <- zip (map show [1..]) $ workspaces conf
    --                   , (m, f) <- [ ("M-",   toggleOrDoSkip skipWS W.greedyView)
    --                               , ("M-S-", windows . W.shift)
    --                               , ("M-C-", windows . copy)
    --                               ]
    --   ]
    , [ ("M-C-w " ++ k, spawn $ unwords [ browser, f ]) | (k, f) <- favouritesList ]
    , [ ("M-s "   ++ k, S.promptSearch myXPConfig f)    | (k, f) <- searchList ]
    ]
  where
    skipWS = [ "NSP" ]

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

myLogHook machine output =
    dynamicLogWithPP $ (myPP machine) { ppOutput = hPutStrLn output }

myPP machine = defaultPP
    { ppCurrent         = dzenColor colorWhite    colorBlue     . dzenWSIcon machine True
    , ppUrgent          = dzenColor colorWhite    colorRed      . dzenWSIcon machine True
    , ppVisible         = dzenColor colorWhite    colorGray     . dzenWSIcon machine True
    , ppHidden          = dzenColor colorGrayAlt  colorGray     . dzenWSIcon machine True
    , ppHiddenNoWindows = dzenColor colorGray     colorBlackAlt . dzenWSIcon machine False
    , ppTitle           = dzenColor colorWhiteAlt colorBlackAlt . shorten 150
    , ppLayout          = dzenPPLayout machine colorRed colorBlue colorBlack . words
    , ppSep             = ""
    , ppWsSep           = ""
    , ppSort            = getSortByIndexWithoutNSP
    , ppOrder           = \(ws:l:t:_) -> [ ws, l, dzenColor colorBlue colorBlackAlt "» ", t ]
    }

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
    , fgColor           = "#8cedff"
    , bgColor           = "black"
    , bgHLight          = "black"
    , fgHLight          = "#f92672"
    , promptBorderWidth = 0
    , position          = Bottom
    }

myGSConfig = defaultGSConfig
    { gs_font        = xftFont
    , gs_cellheight  = 55
    , gs_cellwidth   = 164
    , gs_cellpadding = 10
    }

applyUrgency = withUrgencyHookC (BorderUrgencyHook colorRed)
    urgencyConfig { suppressWhen = Focused }

getScreen = head <$> (openDisplay "" >>= getScreenInfo)

startDzen :: MonadIO m => Rectangle -> m Handle
startDzen = spawnPipe . myDzen

main = do
    machine <- getMachine
    screen  <- getScreen
    browser <- getBrowser "firefox"
    dzenbar <- startDzen screen

    let tweaks  = getTweaks machine
    xmonad . applyProfile machine . applyUrgency $ defaultConfig
        { manageHook         = myRules tweaks (positionRationalRect screen)
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , layoutHook         = myLayoutRules (Any <?> return False) tweaks
        , logHook            = myLogHook machine dzenbar
        , startupHook        = myStartupHook
        , modMask            = myModMask
        , keys               = myKeys browser
        , borderWidth        = myBorderWidth
        , normalBorderColor  = colorGray
        , focusedBorderColor = colorBlue
        , focusFollowsMouse  = True
        }

myDzen :: Rectangle -> String
myDzen (Rectangle x y sw sh) = "dzen2 " ++ unwords
    [ "-x"   , show x
    , "-w"   , show sw
    , "-y"   , show (sh - 16)
    , "-h"   , show 16
    , "-fn"  , quote dzenFont
    , "-fg"  , quote colorWhite
    , "-bg"  , quote colorBlackAlt
    , "-ta l"
    , "-e 'onstart=lower'" ]
  where
    quote = wrap "'" "'"

positionRationalRect :: Rectangle -> W.RationalRect
positionRationalRect (Rectangle sx sy sw sh) =
    let bh = 16
        h  = (2 * fi sh / 5) - bh
        ry = (fi sh - h - bh) / fi sh
        rh = h / fi sh
    in W.RationalRect 0 ry 1 rh
  where
    fi = fromIntegral

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
