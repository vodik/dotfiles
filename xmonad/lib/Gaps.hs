{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Gaps ( gaps
            , Gaps ) where

import Graphics.X11 (Rectangle(..), Position, Dimension)
import Control.Arrow (second)
import XMonad.Util.Font (fi)
import XMonad.Layout.LayoutModifier

gaps :: Int -> l a -> ModifiedLayout Gaps l a
gaps g = ModifiedLayout (Gaps g)

data Gaps a = Gaps Int deriving (Show, Read)

instance LayoutModifier Gaps a where
    pureModifier gap r _ wrs = (map (second $ shrinkRect gap r) wrs, Nothing)
    modifierDescription (Gaps g) = "Gaps " ++ show g

-- | Shrink the window's rectangle to add a nice gap between windows.
--
shrinkRect :: Gaps a -> Rectangle -> Rectangle -> Rectangle
shrinkRect gap (Rectangle sx sy sw sh) (Rectangle x y w h) =
    Rectangle (x + dl) (y + dt) (w - fi dl - dr) (h - fi dt - db)
    where
        dl = xyCalcGapLeft gap x sx
        dt = xyCalcGapLeft gap y sy
        dr = xyCalcGapRight gap x w sx sw
        db = xyCalcGapRight gap y h sy sh

-- | Calculate the gap's offset from the left/top.
--
xyCalcGapLeft :: Integral a => Gaps t -> Position -> Position -> a
xyCalcGapLeft (Gaps g) x sx
    | x == sx   = fi g
    | otherwise = halfGap g

-- | Calculate the gap's offset from the right/bottom.
--
xyCalcGapRight :: Integral a => Gaps t -> Position -> Dimension -> Position -> Dimension -> a
xyCalcGapRight (Gaps g) x w sx sw
    | x + fi w - sx == fi sw = fi g
    | otherwise              = halfGap g

halfGap :: Integral a => Int -> a
halfGap = truncate . (/2) . fi
