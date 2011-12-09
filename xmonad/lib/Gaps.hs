{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Gaps ( gaps, gapsBorder, Gaps ) where

import Graphics.X11 (Rectangle(..))
import Control.Arrow (second)
import XMonad.Util.Font (fi)
import XMonad.Layout.LayoutModifier

gaps :: Int -> l a -> ModifiedLayout Gaps l a
gaps g = ModifiedLayout (Gaps g g)

gapsBorder :: Int -> Int -> l a -> ModifiedLayout Gaps l a
gapsBorder s g = ModifiedLayout (Gaps s g)

data Gaps a = Gaps Int Int deriving (Show, Read)

instance LayoutModifier Gaps a where
    pureModifier (Gaps s g) r _ wrs = (map (second $ shrinkRect s g r) wrs, Nothing)
    modifierDescription (Gaps _ g) = "Gaps " ++ show g

shrinkRect :: Int -> Int -> Rectangle -> Rectangle -> Rectangle
shrinkRect s g (Rectangle sx sy sw sh) (Rectangle x y w h) =
    Rectangle x' y' w' h'
    where
        x' = if x == sx then x + (fi s) else x
        y' = if y == sy then y + (fi s) else y
        w' = if x == sx then w - (fi s) - gX else w - gX
        h' = if y == sy then h - (fi s) - gY else h - gY
        gX = if x + (fi w) == fi sw then fi s else fi g
        gY = if y + (fi h) - sy == fi sh then fi s else fi g
