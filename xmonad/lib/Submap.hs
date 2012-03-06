module Submap where

import Data.Bits
import XMonad hiding (keys)
import XMonad.Prompt (mkUnmanagedWindow)
import XMonad.Util.Font
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad.Fix (fix)

data SubKey = Key (M.Map (KeyMask, KeySym) SubKey)
            | Action (X ())

data SMConfig = SMConfig
    { smFont :: String }

defaultSubmapConfig = SMConfig
    { smFont = "xft:Envy Code R:size=9" }

-- submapP :: String -> SMConfig -> M.Map (KeyMask, KeySym) SubKey
submapP :: String -> SMConfig -> M.Map (KeyMask, KeySym) SubKey -> (String, X ())
submapP p config keys = (p, submap config p keys)

submap :: SMConfig -> String -> M.Map (KeyMask, KeySym) SubKey -> X ()
submap config msg keys = submapDefault config msg (return ()) keys

submapDefault :: SMConfig -> String -> X () -> M.Map (KeyMask, KeySym) SubKey -> X ()
submapDefault config msg def keys = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    s <- gets $ screenRect . W.screenDetail . W.current . windowset
    win <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
                    (rect_x s) (rect_y s) (rect_width s) (rect_height s)

    liftIO $ mapWindow dpy win
    liftIO $ selectInput dpy win (exposureMask .|. keyPressMask)
    status <- io $ grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime

    font <- initXMF (smFont config)

    drawWinBox win font ("red", "blue") 20 50 msg 20 20 12
    (m, s) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent dpy keyPressMask p
        KeyEvent { ev_keycode = code, ev_state = m } <- getEvent p
        keysym <- keycodeToKeysym dpy code 0
        if isModifierKey keysym
            then nextkey
            else return (m, keysym)
    -- Remove num lock mask and Xkb group state bits
    m' <- cleanMask $ m .&. ((1 `shiftL` 12) - 1)

    io $ do
        unmapWindow dpy win
        destroyWindow dpy win
        sync dpy False
    releaseXMF font

    let rst = M.lookup (m', s) keys
    case rst of
        Just (Action x) -> x
        _               -> def

borderColor :: String
borderColor = "white"

drawWinBox :: Window -> XMonadFont -> (String, String) -> Integer -> Integer -> String -> Integer -> Integer -> Integer -> X ()
drawWinBox win font (fg,bg) ch cw text x y cp =
    withDisplay $ \dpy -> do
    gc <- liftIO $ createGC dpy win
    bordergc <- liftIO $ createGC dpy win
    liftIO $ do
        Just fgcolor <- initColor dpy fg
        Just bgcolor <- initColor dpy bg
        Just bordercolor <- initColor dpy borderColor
        setForeground dpy gc fgcolor
        setBackground dpy gc bgcolor
        setForeground dpy bordergc bordercolor

    printStringXMF dpy win font gc bg fg (fromInteger x) (fromInteger y) text
    liftIO $ freeGC dpy gc >> freeGC dpy bordergc
