{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Gaps ( gaps, gapsBorder, Gaps ) where

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
    pureModifier (Gaps s g) r _ wrs = (map (second $ shrinkRect s g r) wrs, Nothing)
    modifierDescription (Gaps _ g) = "Gaps " ++ show g

shrinkRect :: Int -> Int -> Rectangle -> Rectangle -> Rectangle
shrinkRect s g (Rectangle sx sy sw sh) (Rectangle x y w h) =
    Rectangle x' y' w' h'
    where
        x' = x + xyCalcGap s g x sx
        y' = y + xyCalcGap s g y sy
        w' = xyCalcWidth s g x w sx sw
        h' = xyCalcWidth s g y h sy sh

xyCalcGap :: Integral a => Int -> Int -> a -> a -> a
xyCalcGap s g x sx
    | x == sx   = fi s
    | otherwise = halfGap g

xyCalcWidth :: Int -> Int -> Int32 -> Word32 -> Int32 -> Word32 -> Word32
xyCalcWidth s g x w sx sw = w - xyCalcLeftGap - xyCalcRightGap
    where xyCalcLeftGap  | x == sx                = fi s
                         | otherwise              = halfGap g
          xyCalcRightGap | x + fi w - sx == fi sw = fi s
                         | otherwise              = halfGap g

halfGap :: Integral a => Int -> a
halfGap = truncate . (/2) . fi
