{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module GuardLayout ( GuardLayout
   , Condition
   , validate
   , onCondition
   , onConditions
   , modCondition
   , modConditions
   ) where


import Control.Monad
import Data.Maybe

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier

data (Show p) => GuardLayout p l1 l2 a = GuardLayout [p] Bool (l1 a) (l2 a)
    deriving (Read, Show)

class (Show p, Read p) => Condition p where
    validate :: W.Workspace WorkspaceId l a -> p -> X Bool

onCondition :: (LayoutClass l1 a, LayoutClass l2 a, Condition p) => p -> l1 a -> l2 a -> GuardLayout p l1 l2 a
onCondition p = onConditions [p]

onConditions :: (LayoutClass l1 a, LayoutClass l2 a, Condition p) => [p] -> l1 a -> l2 a -> GuardLayout p l1 l2 a
onConditions p = GuardLayout p False

modCondition :: (LayoutClass l a, Condition p) => p -> (l a -> ModifiedLayout lm l a) -> l a -> GuardLayout p (ModifiedLayout lm l) l a
modCondition p = modConditions [p]

modConditions :: (LayoutClass l a, Condition p) => [p] -> (l a -> ModifiedLayout lm l a) -> l a -> GuardLayout p (ModifiedLayout lm l) l a
modConditions p f l = GuardLayout p False (f l) l

instance (Condition p, LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (GuardLayout p l1 l2) a where
    runLayout ws@(W.Workspace i p@(GuardLayout ps _ lt lf) ms) r =
        checkCondition ws ps >>=
        (\b -> if b then do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                            return (wrs, Just $ mkNewPerScreenT p mlt')
                    else do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                            return (wrs, Just $ mkNewPerScreenF p mlt'))

    handleMessage (GuardLayout ps bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ GuardLayout ps bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ GuardLayout ps bool lt nf)

    description (GuardLayout _ True l1 _) = description l1
    description (GuardLayout _ _    _ l2) = description l2

checkCondition :: (Condition p) => W.Workspace WorkspaceId l a -> [p] -> X Bool
checkCondition ws ps = liftM (any id) $ mapM (validate ws) ps

mkNewPerScreenT :: (Condition p) => GuardLayout p l1 l2 a -> Maybe (l1 a) -> GuardLayout p l1 l2 a
mkNewPerScreenT (GuardLayout ps _ lt lf) mlt' = (\lt' -> GuardLayout ps True lt' lf) $ fromMaybe lt mlt'

mkNewPerScreenF :: (Condition p) => GuardLayout p l1 l2 a -> Maybe (l2 a) -> GuardLayout p l1 l2 a
mkNewPerScreenF (GuardLayout ps _ lt lf) mlf' = (\lf' -> GuardLayout ps False lt lf') $ fromMaybe lf mlf'
