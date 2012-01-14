{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module BalancedTile
    ( BalancedTall (..)
    ) where

import XMonad hiding (tile, splitVertically, splitHorizontallyBy, trace)
import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W
import Debug.Trace

import Control.Monad
import Data.List ((\\))
import qualified Data.Map as M

data BalancedTall a = BalancedTall
    { nmaster :: Int
    , delta   :: Rational
    , frac    :: Rational
    , slaves  :: [Rational]
    } deriving (Show, Read)

instance LayoutClass BalancedTall a where
    doLayout (BalancedTall nmaster _ frac mfrac) r =
        return . (\x -> (x, Nothing))
        . ap zip (tile frac (mfrac ++ repeat 1) r nmaster . length) . W.integrate

    handleMessage (BalancedTall nmaster delta frac mfrac) m = do
        ms <- (W.stack . W.workspace . W.current) `fmap` gets windowset
        fs <- (M.keys . W.floating) `fmap` gets windowset
        return $ ms >>= unfloat fs >>= handleMsg
      where
        handleMsg s = msum [ fmap resize (fromMessage m)
                           , fmap incmastern (fromMessage m) ]

        unfloat fs s = if W.focus s `elem` fs
                          then Nothing
                          else Just $ s { W.up   = W.up s   \\ fs
                                        , W.down = W.down s \\ fs }

        resize Shrink = BalancedTall nmaster delta (max 0 $ frac - delta) mfrac
        resize Expand = BalancedTall nmaster delta (min 1 $ frac + delta) mfrac

        modifymfrac [] _ _ = []
        modifymfrac (f:fx) d n | n == 0    = f + d : fx
                               | otherwise = f : modifymfrac fx d (n - 1)

        incmastern (IncMasterN d) = BalancedTall (max 0 $ nmaster + d) delta frac mfrac

    description _ = "ResizableTall"

tile :: Rational -> [Rational] -> Rectangle -> Int -> Int -> [Rectangle]
tile f mf r nmaster n =
    if n == 1 || nmaster == 0
       then splitVertically mf n r
       else let nmaster' = min (n `quot` 2) nmaster
            in splitVertically mf nmaster' r1 ++ splitVertically (drop nmaster' mf) (n - nmaster') r2
  where
    (r1,r2) = splitHorizontallyBy f r

splitVertically :: RealFrac r => [r] -> Int -> Rectangle -> [Rectangle]
splitVertically [] _ r         = [r]
splitVertically _  n r | n < 2 = [r]
splitVertically (f:fx) n (Rectangle sx sy sw sh) =
    Rectangle sx sy sw smallh : splitVertically fx (n - 1) (Rectangle sx (sy + fromIntegral smallh) sw (sh - smallh))
  where
    smallh = min sh (floor $ fromIntegral (sh `div` fromIntegral n) * f) --hmm, this is a fold or map.

splitHorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    (Rectangle sx sy leftw sh, Rectangle (sx + fromIntegral leftw) sy (sw - leftw) sh)
  where
    leftw = floor $ fromIntegral sw * f
