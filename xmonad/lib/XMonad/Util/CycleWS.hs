module XMonad.Util.CycleWS
    ( skipWS
    , moveTo, moveToNonEmpty
    , shiftTo, shiftToEmpty
    , toggleWS, CW.toggleOrDoSkip
    , toggleCopy, doCopy
    ) where

import Control.Applicative
import Control.Arrow
import Data.Maybe

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Util.Stack
import XMonad.Util.WorkspaceCompare
import qualified XMonad.Actions.CycleWS as CW
import qualified XMonad.StackSet as W

type WSFilter = WindowSpace -> Bool

skipWS :: [String] -> WSFilter
skipWS ws = not . (`elem` ws) . W.tag

isEmpty :: WSFilter
isEmpty = isNothing . W.stack

isNonEmpty :: WSFilter
isNonEmpty = isJust . W.stack

filterWSI :: [WSFilter] -> [WindowSpace] -> [WindowSpace]
filterWSI = map filter >>> foldr1 (.)

mkWSI :: [WSFilter] -> CW.WSType
mkWSI = foldr1 (liftA2 (&&)) >>> return >>> CW.WSIs

moveTo :: CW.Direction1D -> [WSFilter] -> X ()
moveTo dir = CW.moveTo dir . mkWSI

moveToNonEmpty :: CW.Direction1D -> [WSFilter] -> X ()
moveToNonEmpty dir = CW.moveTo dir . mkWSI . (isNonEmpty :)

shiftTo :: CW.Direction1D -> [WSFilter] -> X ()
shiftTo dir f = do
    ws <- CW.findWorkspace getSortByIndex dir (mkWSI f) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToEmpty :: CW.Direction1D -> [WSFilter] -> X ()
shiftToEmpty dir f = do
    ws <- CW.findWorkspace getSortByIndex dir (mkWSI $ isEmpty : f) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

toggleWS :: [WorkspaceId] -> X ()
toggleWS = CW.toggleWS'

toggleCopy :: [WorkspaceId] -> X ()
toggleCopy ws = do
    cpys <- wsContainingCopies
    wset <- gets windowset
    let cur  = W.tag . W.workspace $ W.current wset
        set  = allNonEmpty ws wset
        tags = filter (/= cur) $ map W.tag set
    if cpys /= tags
        then windows $ ($ set) . foldr (copy . W.tag)
        else killAllOtherCopies

doCopy :: [WorkspaceId] -> ManageHook
doCopy ws = ask >>= \w -> do
    wset <- liftX $ allNonEmpty ws <$> gets windowset
    doF $ flip (foldr (copyWindow w . W.tag)) wset

allNonEmpty :: [String] -> W.StackSet WorkspaceId (Layout Window) Window s sd -> [WindowSpace]
allNonEmpty ws = W.workspaces >>> filterWSI [ isNonEmpty, skipWS ws ]
