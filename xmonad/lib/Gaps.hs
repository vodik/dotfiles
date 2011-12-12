{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Gaps ( gaps
            , gapsBorder
            , Gaps ) where

import Data.Int
import Data.Word
import Graphics.X11 (Rectangle(..), Position, Dimension)
import Control.Arrow (second)
import XMonad.Util.Font (fi)
import XMonad.Layout.LayoutModifier

gaps :: Int -> l a -> ModifiedLayout Gaps l a
gaps g = ModifiedLayout (Gaps g g)

gapsBorder :: Int -> Int -> l a -> ModifiedLayout Gaps l a
gapsBorder s g = ModifiedLayout (Gaps s g)

data Gaps a = Gaps Int Int deriving (Show, Read)

instance LayoutModifier Gaps a where
    pureModifier gap r _ wrs = (map (second $ shrinkRect gap r) wrs, Nothing)
    modifierDescription (Gaps _ g) = "Gaps " ++ show g

-- | Shrink the window's rectangle to add a nice gaps between windows.
--
shrinkRect :: Gaps a -> Rectangle -> Rectangle -> Rectangle
shrinkRect gap (Rectangle sx sy sw sh) (Rectangle x y w h) =
    Rectangle x' y' w' h'
    where
        x' = x + xyCalcGap gap x sx
        y' = y + xyCalcGap gap y sy
        w' = xyCalcWidth gap x w sx sw
        h' = xyCalcWidth gap y h sy sh

-- | Calculate the offset from either the left or from the top to add
-- a gap.
--
xyCalcGap (Gaps s g) x sx
    | x == sx   = fi s
    | otherwise = halfGap g

-- | Calculate the new width of the window to account to make the
-- gaps.
--
xyCalcWidth (Gaps s g) x w sx sw = w - xyCalcLeftGap - xyCalcRightGap
    where xyCalcLeftGap  | onLeft    = fi s
                         | otherwise = halfGap g
          xyCalcRightGap | onRight   = fi s
                         | otherwise = halfGap g
          onLeft  = x == sx
          onRight = x + fi w - sx == fi sw

halfGap :: Integral a => Int -> a
halfGap = truncate . (/2) . fi
