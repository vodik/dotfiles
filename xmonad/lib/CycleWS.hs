module CycleWS where

import Control.Applicative
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

mkWSI :: (WindowSpace -> Bool) -> CW.WSType
mkWSI = CW.WSIs . return

moveTo :: CW.Direction1D -> [String] -> X ()
moveTo dir ws = CW.moveTo dir . mkWSI $ skipWS ws

moveToNonEmpty :: CW.Direction1D -> [String] -> X ()
moveToNonEmpty dir ws = CW.moveTo dir . mkWSI $ (&&) <$> isNonEmpty <*> skipWS ws

shiftTo :: CW.Direction1D -> [String] -> X ()
shiftTo dir ws = do
    ws <- CW.findWorkspace getSortByIndex dir (mkWSI $ skipWS ws) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToEmpty :: CW.Direction1D -> [String] -> X ()
shiftToEmpty dir ws = do
    ws <- CW.findWorkspace getSortByIndex dir (mkWSI $ (&&) <$> isEmpty <*> skipWS ws) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

toggleCopy :: [String] -> X () -> X ()
toggleCopy ws copier = do
    copies <- wsContainingCopies

    s <- gets windowset
    let cur = W.tag . W.workspace $ W.current s
        set = map W.tag . filter ((/= cur) . W.tag) $ allNonEmpty ws s

    if (copies /= set)
        then copier
        else killAllOtherCopies

copyOntoNonEmpty :: Eq s => [String]
                         -> W.StackSet WorkspaceId (Layout Window) Window s sd
                         -> W.StackSet WorkspaceId (Layout Window) Window s sd
copyOntoNonEmpty ws s = foldr (copy . W.tag) s $ allNonEmpty ws s

allNonEmpty :: [String]
            -> W.StackSet WorkspaceId (Layout Window) Window s sd
            -> [WindowSpace]
allNonEmpty ws = filter (skipWS ws) . filter isNonEmpty . W.workspaces
