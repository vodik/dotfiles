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

toggleWS :: [String] -> X ()
toggleWS = CW.toggleWS'

toggleCopy :: [String] -> X ()
toggleCopy ws = do
    cpys <- wsContainingCopies
    wset <- gets windowset
    let cur  = W.tag . W.workspace $ W.current wset
        set  = allNonEmpty ws wset
        tags = map W.tag . filter ((/= cur) . W.tag) $ set
    if cpys /= tags
        then windows $ ($ set) . foldr (copy . W.tag)
        else killAllOtherCopies
  where
    allNonEmpty ws = filter (skipWS ws) . filter isNonEmpty . W.workspaces
