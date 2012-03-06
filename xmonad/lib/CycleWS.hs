module CycleWS where

import Control.Applicative
import Control.Arrow
import Data.Maybe

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Util.Stack
import XMonad.Util.WorkspaceCompare
import qualified XMonad.Actions.CycleWS as CW
import qualified XMonad.StackSet as W

skipWS :: [String] -> WindowSpace -> Bool
skipWS ws = not . (`elem` ws) . W.tag

isEmpty :: WindowSpace -> Bool
isEmpty = isNothing . W.stack

isNonEmpty :: WindowSpace -> Bool
isNonEmpty = isJust . W.stack

filterWSI :: [WindowSpace -> Bool] -> [WindowSpace] -> [WindowSpace]
filterWSI = map filter >>> foldr1 (.)

mkWSI :: [WindowSpace -> Bool] -> CW.WSType
mkWSI = foldr1 (liftA2 (&&)) >>> return >>> CW.WSIs

moveTo :: CW.Direction1D -> [WorkspaceId] -> X ()
moveTo dir ws = CW.moveTo dir $ mkWSI [ skipWS ws ]

moveToNonEmpty :: CW.Direction1D -> [WorkspaceId] -> X ()
moveToNonEmpty dir ws = CW.moveTo dir $ mkWSI [ isNonEmpty, skipWS ws ]

shiftTo :: CW.Direction1D -> [WorkspaceId] -> X ()
shiftTo dir ws = do
    ws <- CW.findWorkspace getSortByIndex dir (mkWSI [ skipWS ws ]) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToEmpty :: CW.Direction1D -> [WorkspaceId] -> X ()
shiftToEmpty dir ws = do
    ws <- CW.findWorkspace getSortByIndex dir (mkWSI [ isEmpty, skipWS ws ]) 1
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
