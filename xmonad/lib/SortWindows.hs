{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module SortWindows
    ( sortProperty
    , sortProperties
    , SortLayout
    , Property (..)
    ) where

import Control.Applicative
import Control.Monad
import Data.List (delete, intersect, (\\))
import Data.Maybe

import XMonad hiding (focus)
import XMonad.StackSet (Workspace (..), Stack (..))
import XMonad.Layout.WindowNavigation
import XMonad.Util.WindowProperties
import qualified XMonad.StackSet as W

import Debug.Trace

data SortLayout l1 l2 a = SortLayout [a] [a] [a] Bool Rational Rational [Property] (l1 a) (l2 a)
    deriving (Read, Show)

sortProperty :: (LayoutClass l1 a, LayoutClass l2 a)
                => Bool
                -> Rational
                -> Rational
                -> Property
                -> l1 a
                -> l2 a
                -> SortLayout l1 l2 a
sortProperty f d r p = sortProperties f d r [p]

sortProperties :: (LayoutClass l1 a, LayoutClass l2 a)
                  => Bool
                  -> Rational
                  -> Rational
                  -> [Property]
                  -> l1 a
                  -> l2 a
                  -> SortLayout l1 l2 a
sortProperties = SortLayout [] [] []

instance (LayoutClass l1 Window, LayoutClass l2 Window) => LayoutClass (SortLayout l1 l2) Window where
    doLayout (SortLayout f w1 w2 fill delta frac prop l1 l2) r s =
        let origws = W.integrate s              -- passed in windows
            w1c = origws `intersect` w1         -- current windows in the first pane
            w2c = origws `intersect` w2         -- current windows in the second pane
            new = origws \\ (w1c ++ w2c)        -- new windows
            f'  = focus s : delete (focus s) f  -- list of focused windows, contains 2 elements at most
        in do
            matching <- filterM pfilter new     -- new windows matching predecate
            let w1' = w1c ++ matching           -- updated first pane windows
                w2' = w2c ++ (new \\ matching)  -- updated second pane windows
                s1  = differentiate f' w1'      -- first pane stack
                s2  = differentiate f' w2'      -- second pane stack
            (wrs, ml1', ml2') <- split fill w1' l1 s1 w2' l2 s2 frac r
            return (wrs, Just $ SortLayout f' w1' w2' fill delta frac prop (fromMaybe l1 ml1') (fromMaybe l2 ml2'))
      where
        pfilter w = foldM (\a p -> (a ||) <$> hasProperty p w) False prop

    handleMessage us@(SortLayout f ws1 ws2 fill delta frac prop l1 l2) m
        | Just Shrink <- fromMessage m =
            let frac' = max 0 $ frac - delta
            in return . Just $ SortLayout f ws1 ws2 fill delta frac' prop l1 l2
        | Just Expand <- fromMessage m =
            let frac' = min 1 $ frac + delta
            in return . Just $ SortLayout f ws1 ws2 fill delta frac' prop l1 l2
        | otherwise = do
            ml1' <- handleMessage l1 m
            ml2' <- handleMessage l2 m
            if isJust ml1' || isJust ml2'
               then return . Just $ SortLayout f ws1 ws2 fill delta frac prop (fromMaybe l1 ml1') (fromMaybe l2 ml2')
               else return Nothing

split True w1 l1 s1 [] _  _  _ r = runLayout (Workspace "" l1 s1) r >>= \(wrs, ml) -> return (wrs, ml, Nothing)
split _    [] _  _  w2 l2 s2 _ r = runLayout (Workspace "" l2 s2) r >>= \(wrs, ml) -> return (wrs, Nothing, ml)
split _    w1 l1 s1 w2 l2 s2 f r = do
    (wrs1, ml1') <- runLayout (Workspace "" l1 s1) r1
    (wrs2, ml2') <- runLayout (Workspace "" l2 s2) r2
    return (wrs1 ++ wrs2, ml1', ml2')
  where
    (r1, r2) = splitHorizontallyBy f r

differentiate :: Eq q => [q] -> [q] -> Maybe (Stack q)
differentiate (z:zs) xs
    | z `elem` xs = Just Stack { focus = z
                               , up    = reverse $ takeWhile (/=z) xs
                               , down  = tail $ dropWhile (/=z) xs }
    | otherwise   = differentiate zs xs
differentiate [] xs = W.differentiate xs
