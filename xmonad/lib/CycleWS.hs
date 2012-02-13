module CycleWS where

import Data.Maybe

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

skipNSP :: X (WindowSpace -> Bool)
skipNSP = return $ ("NSP" /=) . W.tag

skipNSPAnd :: X (WindowSpace -> Bool) -> X (WindowSpace -> Bool)
skipNSPAnd p = do
    a <- p
    b <- skipNSP
    return $ \w -> a w && b w

isEmpty :: X (WindowSpace -> Bool)
isEmpty = return $ isNothing . W.stack

isNonEmpty :: X (WindowSpace -> Bool)
isNonEmpty = return $ isJust . W.stack

nextWS' :: X ()
nextWS' = moveTo Next (WSIs skipNSP)

prevWS' :: X ()
prevWS' = moveTo Prev (WSIs skipNSP)

shiftToNext' :: X ()
shiftToNext' = do
    ws <- findWorkspace getSortByIndex Next (WSIs skipNSP) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToPrev' :: X ()
shiftToPrev' = do
    ws <- findWorkspace getSortByIndex Prev (WSIs skipNSP) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

nextWSNonEmpty :: X ()
nextWSNonEmpty = moveTo Next (WSIs $ skipNSPAnd isNonEmpty)

prevWSNonEmpty :: X ()
prevWSNonEmpty = moveTo Prev (WSIs $ skipNSPAnd isNonEmpty)

shiftToNextEmpty :: X ()
shiftToNextEmpty = do
    ws <- findWorkspace getSortByIndex Next (WSIs $ skipNSPAnd isEmpty) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToPrevEmpty :: X ()
shiftToPrevEmpty = do
    ws <- findWorkspace getSortByIndex Prev (WSIs $ skipNSPAnd isEmpty) 1
    windows $ W.shift ws
    windows $ W.greedyView ws
