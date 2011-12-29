{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module PerProp
    ( PerProp
    , Condition
    , getCondition
    , perProp
    , perProps ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Layout.LayoutModifier

import Control.Monad
import Data.Maybe
import Data.ByteString.Internal

data (Show p, Read p, Eq p) => PerProp p l1 l2 a = PerProp [p] Bool (l1 a) (l2 a)
    deriving (Read, Show, Eq)

class (Show p, Read p, Eq p) => Condition p where
    getCondition :: W.Workspace WorkspaceId l a -> X p

checkCondition :: (Condition p) => W.Workspace WorkspaceId l a -> [p] -> X Bool
checkCondition ws ps = liftM (`elem` ps) $ getCondition ws

perProp :: (LayoutClass l1 a, LayoutClass l2 a, Condition p) => p -> l1 a -> l2 a -> PerProp p l1 l2 a
perProp p = perProps [p]

perProps :: (LayoutClass l1 a, LayoutClass l2 a, Condition p) => [p] -> l1 a -> l2 a -> PerProp p l1 l2 a
perProps p = PerProp p False

instance (Condition p, LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (PerProp p l1 l2) a where
    runLayout ws@(W.Workspace i p@(PerProp ps _ lt lf) ms) r =
      checkCondition ws ps >>=
      (\b -> if b then do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                          return (wrs, Just $ mkNewPerScreenT p mlt')
                  else do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                          return (wrs, Just $ mkNewPerScreenF p mlt'))

    handleMessage (PerProp ps bool lt lf) m
      | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ PerProp ps bool nt lf)
      | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ PerProp ps bool lt nf)

    description (PerProp _ True l1 _) = description l1
    description (PerProp _ _    _ l2) = description l2

mkNewPerScreenT :: (Condition p) => PerProp p l1 l2 a -> Maybe (l1 a) -> PerProp p l1 l2 a
mkNewPerScreenT (PerProp ps _ lt lf) mlt' = (\lt' -> PerProp ps True lt' lf) $ fromMaybe lt mlt'

mkNewPerScreenF :: (Condition p) => PerProp p l1 l2 a -> Maybe (l2 a) -> PerProp p l1 l2 a
mkNewPerScreenF (PerProp ps _ lt lf) mlf' = (\lf' -> PerProp ps False lt lf') $ fromMaybe lf mlf'
