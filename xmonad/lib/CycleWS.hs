module CycleWS where

import Control.Monad
import Data.Maybe

import XMonad
import XMonad.Core
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS hiding (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

skipWS :: [String] -> WindowSpace -> Bool
skipWS ws = not . (`elem` ws) . W.tag

skipWSAnd :: [String] -> (WindowSpace -> Bool) -> WindowSpace -> Bool
skipWSAnd ws = ap $ (&&) . skipWS ws

isEmpty :: WindowSpace -> Bool
isEmpty = isNothing . W.stack

isNonEmpty :: WindowSpace -> Bool
isNonEmpty = isJust . W.stack

mkWSI :: (WindowSpace -> Bool) -> WSType
mkWSI = WSIs . return

nextWS :: [String] -> X ()
nextWS ws = moveTo Next (mkWSI $ skipWS ws)

prevWS :: [String] -> X ()
prevWS ws = moveTo Prev (mkWSI $ skipWS ws)

shiftToNext :: [String] -> X ()
shiftToNext ws = do
    ws <- findWorkspace getSortByIndex Next (mkWSI $ skipWS ws) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToPrev :: [String] -> X ()
shiftToPrev ws = do
    ws <- findWorkspace getSortByIndex Prev (mkWSI $ skipWS ws) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

nextWSNonEmpty :: [String] -> X ()
nextWSNonEmpty ws = moveTo Next (mkWSI $ skipWSAnd ws isNonEmpty)

prevWSNonEmpty :: [String] -> X ()
prevWSNonEmpty ws = moveTo Prev (mkWSI $ skipWSAnd ws isNonEmpty)

shiftToNextEmpty :: [String] -> X ()
shiftToNextEmpty ws = do
    ws <- findWorkspace getSortByIndex Next (mkWSI $ skipWSAnd ws isEmpty) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToPrevEmpty :: [String] -> X ()
shiftToPrevEmpty ws = do
    ws <- findWorkspace getSortByIndex Prev (mkWSI $ skipWSAnd ws isEmpty) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

copyOntoNonEmpty :: Eq s => [String]
                         -> W.StackSet WorkspaceId (Layout Window) Window s sd
                         -> W.StackSet WorkspaceId (Layout Window) Window s sd
copyOntoNonEmpty ws s = foldr (copy . W.tag) s . filter (skipWS ws) . filter isNonEmpty $ W.workspaces s
