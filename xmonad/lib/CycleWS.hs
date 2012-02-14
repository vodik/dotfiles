module CycleWS where

import Data.Maybe

import XMonad
import XMonad.Actions.CycleWS hiding (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

skipWS :: [String] -> X (WindowSpace -> Bool)
skipWS ws = return $ not . (`elem` ws) . W.tag

skipWSAnd :: [String] -> X (WindowSpace -> Bool) -> X (WindowSpace -> Bool)
skipWSAnd ws p = do
    a <- p
    b <- skipWS ws
    return $ \w -> a w && b w

isEmpty :: X (WindowSpace -> Bool)
isEmpty = return $ isNothing . W.stack

isNonEmpty :: X (WindowSpace -> Bool)
isNonEmpty = return $ isJust . W.stack

nextWS :: [String] -> X ()
nextWS ws = moveTo Next (WSIs $ skipWS ws)

prevWS :: [String] -> X ()
prevWS ws = moveTo Prev (WSIs $ skipWS ws)

shiftToNext :: [String] -> X ()
shiftToNext ws = do
    ws <- findWorkspace getSortByIndex Next (WSIs $ skipWS ws) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToPrev :: [String] -> X ()
shiftToPrev ws = do
    ws <- findWorkspace getSortByIndex Prev (WSIs $ skipWS ws) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

nextWSNonEmpty :: [String] -> X ()
nextWSNonEmpty ws = moveTo Next (WSIs $ skipWSAnd ws isNonEmpty)

prevWSNonEmpty :: [String] -> X ()
prevWSNonEmpty ws = moveTo Prev (WSIs $ skipWSAnd ws isNonEmpty)

shiftToNextEmpty :: [String] -> X ()
shiftToNextEmpty ws = do
    ws <- findWorkspace getSortByIndex Next (WSIs $ skipWSAnd ws isEmpty) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToPrevEmpty :: [String] -> X ()
shiftToPrevEmpty ws = do
    ws <- findWorkspace getSortByIndex Prev (WSIs $ skipWSAnd ws isEmpty) 1
    windows $ W.shift ws
    windows $ W.greedyView ws
