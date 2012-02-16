{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module SortWindows
    ( sortQuery
    , setQuery
    , SortLayout
    , SetSort
    ) where

import Control.Applicative
import Control.Monad
import Data.List (delete, intersect, (\\))
import Data.Monoid
import Data.Maybe

import XMonad hiding (focus)
import XMonad.Core
import XMonad.StackSet (Workspace (..), Stack (..))
import XMonad.Layout.WindowNavigation
import XMonad.Util.Invisible
import qualified XMonad.StackSet as W

type InvisibleQuery = Invisible Maybe (Query Any)

data SetSort = SetSort String (Query Any)
             | ResetSort String
    deriving (Typeable)

instance Message SetSort

data SortLayout l1 l2 a = SortLayout [a] [a] [a] String Bool Rational Rational InvisibleQuery (l1 a) (l2 a)
    deriving (Read, Show)

sortQuery :: (LayoutClass l1 a, LayoutClass l2 a)
             => String
             -> Bool
             -> Rational
             -> Rational
             -> l1 a
             -> l2 a
             -> SortLayout l1 l2 a
sortQuery n f d r = SortLayout [] [] [] n f d r (I Nothing)

setQuery :: String -> Query Any -> X ()
setQuery n q = broadcastMessage $ SetSort n q

instance (LayoutClass l1 Window, LayoutClass l2 Window) => LayoutClass (SortLayout l1 l2) Window where
    doLayout (SortLayout f w1 w2 name fill delta frac query l1 l2) r s =
        let origws = W.integrate s              -- passed in windows
            w1c = origws `intersect` w1         -- current windows in the first pane
            w2c = origws `intersect` w2         -- current windows in the second pane
            new = origws \\ (w1c ++ w2c)        -- new windows
            f'  = focus s : delete (focus s) f  -- list of focused windows, contains 2 elements at most
        in do
            matching <- queryFilter query new   -- new windows matching predecate
            let w1' = w1c ++ matching           -- updated first pane windows
                w2' = w2c ++ (new \\ matching)  -- updated second pane windows
                s1  = differentiate f' w1'      -- first pane stack
                s2  = differentiate f' w2'      -- second pane stack
            (wrs, ml1', ml2') <- split fill w1' l1 s1 w2' l2 s2 frac r
            return (wrs, Just $ SortLayout f' w1' w2' name fill delta frac query (fromMaybe l1 ml1') (fromMaybe l2 ml2'))
      where
        queryFilter (I (Just q)) ws = filterM (\w -> getAny <$> runQuery q w) ws
        queryFilter (I Nothing)  _  = return []

    handleMessage (SortLayout f ws1 ws2 name fill delta frac query l1 l2) m
        | Just Shrink <- fromMessage m =
            let frac' = max 0 $ frac - delta
            in return . Just $ SortLayout f ws1 ws2 name fill delta frac' query l1 l2
        | Just Expand <- fromMessage m =
            let frac' = min 1 $ frac + delta
            in return . Just $ SortLayout f ws1 ws2 name fill delta frac' query l1 l2
        | Just (SetSort n q) <- fromMessage m =
            if n == name
                then return . Just $ SortLayout f ws1 ws2 name fill delta frac (I (Just q)) l1 l2
                else return Nothing
        | Just (ResetSort n) <- fromMessage m =
            if n == name
                then return . Just $ SortLayout [] [] [] name fill delta frac query l1 l2
                else return Nothing
        | otherwise = do
            ml1' <- handleMessage l1 m
            ml2' <- handleMessage l2 m
            if isJust ml1' || isJust ml2'
               then return . Just $ SortLayout f ws1 ws2 name fill delta frac query (fromMaybe l1 ml1') (fromMaybe l2 ml2')
               else return Nothing

    description (SortLayout _ _ _ _ _ _ _ _ l1 l2) =
        unwords [ "SortLayout", description l1, description l2 ]

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
