import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Graphics.X11.Xinerama (getScreenInfo)
import System.Environment
import System.Exit
import System.IO
import System.Posix.Unistd (getSystemID, nodeName)
import qualified Data.Map as M
import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD

import XMonad hiding (spawn)
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BalancedTile
import XMonad.Layout.Grid
import XMonad.Layout.GuardLayout
import XMonad.Layout.GuardLayout.Instances
import XMonad.Layout.Master
import XMonad.Layout.MinimizePlus
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.SortWindows
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowGaps
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S

import DynamicTopic
import CycleWS
import Dzen2
import MPD
import Run
import Utils
import Workspaces
import Workspaces.Instances

imClients :: Query Any
imClients = composeAs Any
    [ className =? "Empathy" <&&> role =? "contact_list"
    , className =? "Pidgin"  <&&> role =? "buddy_list"
    , className =? "Skype"   <&&> title `prefixed` "Skype"
    ]

myFloats :: Query Bool
myFloats =
    className `queryAny` [ "Xmessage", "MPlayer", "Lxappearance", "Nitrogen", "Qtconfig", "Gcolor2", "Pavucontrol"
                         , "Nvidia-settings", "Arandr", "Rbutil", "zsnes", "Dwarf_Fortress", "Display" ]

-- myTerminal    = "urxvtc"
myTerminal    = "urxvt"
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

myLayoutRules sort tw = avoidStruts . lessBorders OnlyFloat . minimize . tfull
    . onWorkspace "work"  (mstr tabs ||| tiled)
    . onWorkspace "term"  (mtiled ||| tiled)
    . onWorkspace "chat"  (tag "IM" . sortIM $ tabs ||| grid)
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
    sortIM = sortQuery "chat" False step (imWidth tw) imClients panel
    panel  = ifTaller 1024 Grid tabs
    grid   = gaps 5 $ GridRatio (imGrid tw)
    full   = noBorders Full
    tag t  = renamed [ PrependWords t ]
    step   = 1 % 50

myRules ws rect = manageDocks
    <+> scratchpadManageHook rect
    <+> workspaceShift ws
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

myStartupHook sort = setDefaultCursor xC_left_ptr
    <+> setQuery "chat" imClients
    <+> setQuery "work" sort
    <+> startCompositor "compton" [ "-cGbfI", "0.20", "-O", "0.20" ]
    -- <+> startServices [ "urxvtd", "udiskie", "mpd" ]
    <+> startServices [ "udiskie", "mpd" ]

myKeys ws browser conf = mkKeymap conf $ concat
    [ [ ("M-<Return>", spawn (terminal conf) [])
      , ("M-S-<Return>", currentAction (spawn (terminal conf) []) ws)
      , ("M-w", spawn browser [])
      , ("M-`", scratchpadSpawnActionTerminal $ terminal conf)
      , ("M-p", shellPrompt myXPConfig)

      , ("M-C-<Return>", spawnShell)
      , ("M-C-<Space>",  changeDir myXPConfig)

      -- quit, or restart
      , ("M-S-q",   io exitSuccess)
      , ("M-S-c",   kill1)
      , ("M-C-c",   kill)
      , ("M-S-C-c", spawn "xkill" [])
      , ("M-q",     restart "xmonad" True)

      -- layout
      , ("M-<Space>",   sendMessage NextLayout)
      , ("M-S-<Space>", sendMessage FirstLayout)
      , ("M-a",         sendMessage ToggleLayout)

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
      , ("M-<D>",   moveTo Next skipWS)
      , ("M-<U>",   moveTo Prev skipWS)
      , ("M-<R>",   moveToNonEmpty Next skipWS)
      , ("M-<L>",   moveToNonEmpty Prev skipWS)
      , ("M-S-<D>", shiftTo Next skipWS)
      , ("M-S-<U>", shiftTo Prev skipWS)
      , ("M-S-<R>", shiftToEmpty Next skipWS)
      , ("M-S-<L>", shiftToEmpty Prev skipWS)
      , ("M-<Tab>", toggleWS skipWS)
      , ("M-=",     toggleCopy skipWS)

      -- minimizing
      , ("M-z",   sendMessage MinimizeFloating)
      , ("M-x",   withFocused' minimizeWindow)
      , ("M-S-z", sendMessage RestoreAll)

      -- misc keybinds against alt
      , ("M1-C-l", spawn "slock" [])

      -- multimedia keys
      , ("<XF86AudioLowerVolume>", spawn "amixer" [ "-q", "set", "Master", "3%-" ])
      , ("<XF86AudioRaiseVolume>", spawn "amixer" [ "-q", "set", "Master", "on", "3%+" ])
      , ("<XF86AudioMute>",        spawn "amixer" [ "-q", "set", "Master", "toggle" ])

      -- mpd controls
      , ("M1-C-1", withMPD MPD.toggle)
      , ("M1-C-2", withMPD MPD.stop)
      , ("M1-C-3", withMPD MPD.previous)
      , ("M1-C-4", withMPD MPD.next)
      , ("<XF86AudioPlay>", withMPD MPD.toggle)
      , ("<XF86AudioStop>", withMPD MPD.stop)
      , ("<XF86AudioPrev>", withMPD MPD.previous)
      , ("<XF86AudioNext>", withMPD MPD.next)

      -- change MPD_HOST
      , ("M-<XF86AudioPlay>", changeHost myXPConfig >> logHook conf)
      , ("M1-C-S-1",          changeHost myXPConfig >> logHook conf)

      -- screenshot
      , ("C-<Print>", delayedSpawn 100 "scrot" [ "-s", "/home/simongmzlj/pictures/screenshots/%Y-%m-%d_%H:%M:%S_$wx$h.png" ])
      , ("<Print>",   spawn "scrot" [ "/home/simongmzlj/pictures/screenshots/%Y-%m-%d_%H:%M:%S_$wx$h.png" ])
      ]
    , [ (m ++ i, f w) | (i, w) <- zip (map show [1..]) $ XMonad.workspaces conf
                      , (m, f) <- [ ("M-",   toggleOrDoSkip skipWS W.greedyView)
                                  , ("M-S-", windows . W.shift)
                                  , ("M-C-", windows . copy)
                                  ]
      ]
    , [ ("M-C-w " ++ k, spawn browser [ f ])         | (k, f) <- favouritesList ]
    , [ ("M-s "   ++ k, S.promptSearch myXPConfig f) | (k, f) <- searchList ]
    ]
  where
    skipWS = [ "NSP" ]

myMouseBindings conf@(XConfig {modMask = modm}) =
    (`M.union` (mouseBindings defaultConfig) conf) $ M.fromList
        [ ((modm, button2), killWindow)
        , ((modm,               button4), const $ moveTo Prev skipWS)
        , ((modm,               button5), const $ moveTo Next skipWS)
        , ((modm .|. shiftMask, button4), const $ shiftTo Prev skipWS)
        , ((modm .|. shiftMask, button5), const $ shiftTo Next skipWS)
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

myLogHook res output =
    dynamicLogWithPP $ (myPP res) { ppOutput = hPutStrLn output }

myPP res = defaultPP
    { ppCurrent         = dzenColor colorWhite    colorBlue     . dzenWSIcon res True
    , ppUrgent          = dzenColor colorWhite    colorRed      . dzenWSIcon res True
    , ppVisible         = dzenColor colorWhite    colorGray     . dzenWSIcon res True
    , ppHidden          = dzenColor colorGrayAlt  colorGray     . dzenWSIcon res True
    , ppHiddenNoWindows = dzenColor colorGray     colorBlackAlt . dzenWSIcon res False
    , ppTitle           = dzenColor colorWhiteAlt colorBlackAlt . shorten 150
    , ppLayout          = dzenPPLayout res colorRed colorBlue colorBlack . words
    , ppSep             = ""
    , ppWsSep           = ""
    , ppSort            = getSortByIndexWithoutNSP
    , ppOrder           = \(ws:l:t:e:_) -> [ ws, e, l, dzenColor colorBlue colorBlackAlt "» ", t ]
    , ppExtras          = [ checkMPD ]
    }

checkMPD = Just . maybe "" (const icon) <$> getHost
  where icon = dzenColor colorBlack colorBlue " ^i(etc/icons/chain.xbm) "

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

applyUrgency color = withUrgencyHookC (BorderUrgencyHook color) conf
    where conf = urgencyConfig { suppressWhen = Focused }

getScreen :: IO Rectangle
getScreen = openDisplay "" >>= fmap head . getScreenInfo

getMachine = buildTags $ do
    host <- nodeName <$> liftIO getSystemID

    tag  "work" $ Workspace [ "firefox" ] :> work
    tag1 "term" $ Terminals Nothing
    tag1 "code" $ Terminals (Just "~/projects")
    tag  "chat" $ Workspace [ "pidgin", "skype" ] :> chat

    unless (host == "gmzlj") $
        tag "virt" $ Workspace [ "VirtualBox --startvm 'Windows 8'" ] :> virt

    tag "games" $ Workspace [ "sol" ] :> games
  where
    work  = [ className `queryAny` [ "Firefox", "Chromium", "Zathura", "Thunar", "Gimp" ]
            , title     =? "MusicBrainz Picard"
            , className ~? "^[Ll]ibre[Oo]ffice" ]
    chat  = [ className `queryAny` [ "Empathy", "Pidgin", "Skype" ] ]
    virt  = [ className =? "VirtualBox" ]
    games = [ className `queryAny` [ "Sol", "Pychess", "net-minecraft-LauncherFrame", "zsnes", "Wine", "Dwarf_Fortress" ] ]

ws = map fst

main = do
    machine <- getMachine
    res     <- mkResources machine
    screen  <- getScreen
    browser <- getBrowser "firefox"
    dzenbar <- startDzen

    -- let tweaks  = getTweaks machine
    let tweaks = defaultTweaks
        sort   = workspaceSort "work" machine

    xmonad . applyUrgency colorRed $ defaultConfig
        { manageHook         = myRules machine (positionRationalRect screen)
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , layoutHook         = myLayoutRules sort tweaks
        , logHook            = myLogHook res dzenbar
        , startupHook        = myStartupHook sort
        , modMask            = myModMask
        , keys               = myKeys machine browser
        , mouseBindings      = myMouseBindings
        , workspaces         = to9 $ tagSet machine
        , terminal           = myTerminal
        , borderWidth        = myBorderWidth
        , normalBorderColor  = colorGray
        , focusedBorderColor = colorBlue
        , focusFollowsMouse  = True
        }

startDzen :: MonadIO m => m Handle
startDzen = spawnPipe myDzen

myDzen :: String
myDzen = "dzen2 " ++ unwords
    [ "-y"   , "-16"
    , "-h"   ,  "16"
    , "-fn"  , wrap "'" "'" dzenFont
    , "-fg"  , wrap "'" "'" colorWhite
    , "-bg"  , wrap "'" "'" colorBlackAlt
    , "-ta l"
    , "-e 'onstart=lower'" ]

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
