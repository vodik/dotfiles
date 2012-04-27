{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, FlexibleContexts, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Minimize
-- Copyright   :  (c) Jan Vornberger 2009, Alejandro Serrano 2010
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Makes it possible to minimize windows, temporarily removing them
-- from the layout until they are restored.
--
-----------------------------------------------------------------------------

module XMonad.Layout.MinimizePlus (
        -- * Usage
        -- $usage
        minimize,
        minimizeWindow,
        MinimizeMsg(..),
        Minimize,
    ) where

import Control.Applicative
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Layout.BoringWindows as BW
import XMonad.Util.WindowProperties (getProp32)
import Data.List
import Data.Monoid
import qualified Data.Map as M
import Data.Maybe
import Foreign.C.Types (CLong)

data Minimize a = Minimize [Window] (M.Map Window W.RationalRect) deriving ( Read, Show )

minimize :: LayoutClass l Window => l Window -> ModifiedLayout Minimize l Window
minimize = ModifiedLayout $ Minimize [] M.empty

data MinimizeMsg = MinimizeWin Window
                 | MinimizeFloating
                 | RestoreMinimizedWin Window
                 | RestoreNextMinimized
                 | RestoreAll
                 deriving (Typeable, Eq)

instance Message MinimizeMsg

minimizeWindow :: Window -> X ()
minimizeWindow w = sendMessage (MinimizeWin w) >> BW.focusDown

setMinimizedState :: Window -> Int -> (CLong -> [CLong] -> [CLong]) -> X ()
setMinimizedState win st f = do
    setWMState win st
    withDisplay $ \dpy -> do
        state  <- getAtom "_NET_WM_STATE"
        mini   <- getAtom "_NET_WM_STATE_HIDDEN"
        wstate <- fromMaybe [] <$> getProp32 state win
        let ptype   = 4 -- The atom property type for changeProperty
            fi_mini = fromIntegral mini
        io . changeProperty32 dpy win state ptype propModeReplace $ f fi_mini wstate

setMinimized :: Bool -> Window -> X ()
setMinimized True  win = setMinimizedState win iconicState (:)
setMinimized False win = setMinimizedState win normalState delete

instance LayoutModifier Minimize Window where
    modifierDescription _ = "Minimize"

    modifyLayout (Minimize minimized _) wksp rect = do
        let stack = W.stack wksp
            filtStack = stack >>= W.filter (`notElem` minimized)
        runLayout (wksp {W.stack = filtStack}) rect

    handleMess msg@(Minimize minimized unfloated) m
        | Just (MinimizeWin w) <- fromMessage m, w `notElem` minimized = do
            setMinimized True w
            ws <- gets windowset
            case M.lookup w (W.floating ws) of
                Nothing -> return . Just $ Minimize (w : minimized) unfloated
                Just r  -> do
                    modify (\s -> s { windowset = W.sink w ws })
                    return . Just $ Minimize (w : minimized) (M.insert w r unfloated)

        | Just MinimizeFloating <- fromMessage m = do
            floats <- gets $ W.floating . windowset
            mapM_ (setMinimized True) $ M.keys floats
            let f k v xs = if k `elem` M.keys floats then W.sink k xs else xs
            modify (\s -> s { windowset = M.foldrWithKey f (windowset s) floats })
            return . Just $ Minimize (M.keys floats `mappend` minimized) (unfloated `M.union` floats)

        | Just (RestoreMinimizedWin w) <- fromMessage m = restore w minimized unfloated

        | Just RestoreAll              <- fromMessage m = restoreAll minimized unfloated

        | Just RestoreNextMinimized <- fromMessage m =
            if null minimized
                then return Nothing
                else restore (head minimized) minimized unfloated

        | Just BW.UpdateBoring <- fromMessage m = do
            ws <- gets $ W.workspace . W.current . windowset
            flip sendMessageWithNoRefresh ws $ BW.Replace "Minimize" minimized
            return Nothing

        | otherwise = return Nothing

restore w minimized unfloated = do
    setMinimized False w
    case M.lookup w unfloated of
        Nothing -> do
            modify (\s -> s { windowset = W.focusWindow w (windowset s) })
            return . Just $ Minimize (w `delete` minimized) unfloated
        Just r  -> do
            modify (\s -> s { windowset = W.focusWindow w $ W.float w r (windowset s) })
            return . Just $ Minimize (w `delete` minimized) (M.delete w unfloated)

restoreAll minimized unfloated = do
    mapM_ (setMinimized False) minimized
    modify (\s -> s { windowset = M.foldrWithKey f (windowset s) unfloated })
    modify (\s -> s { windowset = W.focusWindow (head minimized) (windowset s) })
    return . Just $ Minimize [] (M.filterWithKey (\k v -> k `elem` minimized) unfloated)
  where
    f k v xs = if k `elem` minimized then W.float k v xs else xs
