module XMonad.Util.CycleWS
    ( skipWS
    , moveTo, moveToNonEmpty
    , shiftTo, shiftToEmpty
    , toggleWS, CW.toggleOrDoSkip
    ) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Monoid

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Util.Stack
import XMonad.Util.WorkspaceCompare
import qualified XMonad.Actions.CycleWS as CW
import qualified XMonad.StackSet as W

type WSFilter = [WindowSpace -> Bool]

skipWS :: [String] -> WSFilter
skipWS ws = return $ W.tag >>> (`notElem` ws)

isEmpty :: WSFilter
isEmpty = return $ W.stack >>> isNothing

isNonEmpty :: WSFilter
isNonEmpty = return $ W.stack >>> isJust

mkWSI :: WSFilter -> CW.WSType
mkWSI = foldr1 (liftA2 (&&)) >>> return >>> CW.WSIs

moveTo :: CW.Direction1D -> WSFilter -> X ()
moveTo dir = CW.moveTo dir . mkWSI

moveToNonEmpty :: CW.Direction1D -> WSFilter -> X ()
moveToNonEmpty dir = CW.moveTo dir . mkWSI . mappend isNonEmpty

shiftTo :: CW.Direction1D -> WSFilter -> X ()
shiftTo dir f = do
    ws <- CW.findWorkspace getSortByIndex dir (mkWSI f) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

shiftToEmpty :: CW.Direction1D -> WSFilter -> X ()
shiftToEmpty dir f = do
    ws <- CW.findWorkspace getSortByIndex dir (mkWSI $ isEmpty <> f) 1
    windows $ W.shift ws
    windows $ W.greedyView ws

toggleWS :: [WorkspaceId] -> X ()
toggleWS = CW.toggleWS'
