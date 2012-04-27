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
import qualified Data.Map as M
import Data.Maybe
import Foreign.C.Types (CLong)

data Minimize a = Minimize [Window] (M.Map Window W.RationalRect) deriving ( Read, Show )

minimize :: LayoutClass l Window => l Window -> ModifiedLayout Minimize l Window
minimize = ModifiedLayout $ Minimize [] M.empty

data MinimizeMsg = MinimizeWin Window
                 | MinimizeFloating
                 | RestoreMinimizedWin Window
                 | RestoreNextMinimizedWin
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

setMinimized :: Window -> X ()
setMinimized win = setMinimizedState win iconicState (:)

setNotMinimized :: Window -> X ()
setNotMinimized win = setMinimizedState win normalState delete

instance LayoutModifier Minimize Window where
    modifierDescription _ = "Minimize"

    modifyLayout (Minimize minimized _) wksp rect = do
        let stack = W.stack wksp
            filtStack = stack >>= W.filter (`notElem` minimized)
        runLayout (wksp {W.stack = filtStack}) rect

    handleMess (Minimize minimized unfloated) m
        | Just (MinimizeWin w) <- fromMessage m, (w `notElem` minimized) = do
            setMinimized w
            ws <- gets windowset
            case M.lookup w (W.floating ws) of
                Nothing -> return $ Just $ Minimize (w:minimized) unfloated
                Just r -> do
                    modify (\s -> s { windowset = W.sink w ws})
                    return $ Just $ Minimize (w:minimized) (M.insert w r unfloated)
        | Just (RestoreMinimizedWin w) <- fromMessage m = do
            setNotMinimized w
            case M.lookup w unfloated of
                Nothing -> return $ Just $ Minimize (minimized \\ [w]) unfloated
                Just r -> do
                    ws <- gets windowset
                    modify (\s -> s { windowset = W.float w r ws})
                    return $ Just $ Minimize (minimized \\ [w]) (M.delete w unfloated)
        | Just RestoreNextMinimizedWin <- fromMessage m = do
            ws <- gets windowset
            if not (null minimized)
                then case M.lookup (head minimized) unfloated of
                    Nothing -> do
                        let w = head minimized
                        setNotMinimized w
                        modify (\s -> s { windowset = W.focusWindow w ws})
                        return $ Just $ Minimize (tail minimized) unfloated
                    Just r -> do
                        let w = head minimized
                        setNotMinimized w
                        modify (\s -> s { windowset = (W.focusWindow w . W.float w r) ws})
                        return $ Just $ Minimize (tail minimized) (M.delete w unfloated)
                else return Nothing
        | Just BW.UpdateBoring <- fromMessage m = do
            ws <- gets (W.workspace . W.current . windowset)
            flip sendMessageWithNoRefresh ws $ BW.Replace "Minimize" minimized
            return Nothing
        | otherwise = return Nothing
