module GuardLayout.Instances
    ( ScreenSpace (..)
    , ScreenSize (..)
    , AspectRatio (..)
    , Hostname (..)
    , calculateBox
    ) where

import Control.Monad
import Data.Maybe
import Data.Ratio
import System.Posix.Unistd (getSystemID, nodeName)

import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)

import XMonad
import qualified XMonad.StackSet as W

import GuardLayout

data ScreenSpace = ScreenSpace (Maybe Dimension) (Maybe Dimension)
    deriving (Show, Read)

data ScreenSize = AtLeast ScreenSpace
                | SmallerThan ScreenSpace
    deriving (Show, Read)

data AspectRatio = AspectRatio (Ratio Dimension)
    deriving (Show, Read)

data Hostname = Hostname String
    deriving (Show, Read)

instance Condition ScreenSize where
    validate ws (SmallerThan si) =
        liftM (calculateBox si (<)) getScreenSize

    validate ws (AtLeast si) =
        liftM (calculateBox si (>=)) getScreenSize

instance Condition AspectRatio where
    validate ws (AspectRatio r) =
        getScreenSize >>= \(Rectangle _ _ sw sh) -> return $ r == (sw % sh)

instance Condition Hostname where
    validate ws (Hostname n) =
        liftM ((n ==) . nodeName) $ io getSystemID

getScreenSize :: X Rectangle
getScreenSize = io $ liftM head $ getScreenInfo =<< openDisplay ""

calculateBox :: ScreenSpace -> (Dimension -> Dimension -> Bool) -> Rectangle -> Bool
calculateBox (ScreenSpace w h) op (Rectangle _ _ sw sh) =
    let wide = liftM (op sw) w
        tall = liftM (op sh) h
    in any id $ catMaybes [wide, tall]
