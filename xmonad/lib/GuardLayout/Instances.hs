module GuardLayout.Instances
    ( ScreenInfo (..)
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

data ScreenInfo = ScreenInfo
    { width  :: Maybe Dimension
    , height :: Maybe Dimension
    }
    deriving (Show, Read)

data ScreenSize = AtLeast ScreenInfo
                | SmallerThan ScreenInfo
    deriving (Show, Read)

data AspectRatio = AspectRatio (Ratio Dimension)
    deriving (Show, Read)

data Hostname = Hostname String
    deriving (Show, Read)

instance Condition ScreenSize where
    getCondition ws (SmallerThan si) =
        liftM (calculateBox si (<)) getScreenSize

    getCondition ws (AtLeast si) =
        liftM (calculateBox si (>=)) getScreenSize

instance Condition AspectRatio where
    getCondition ws (AspectRatio r) =
        getScreenSize >>= \(Rectangle _ _ sw sh) -> return $ r == (sw % sh)

instance Condition Hostname where
    getCondition ws (Hostname n) =
        liftM ((n ==) . nodeName) $ io getSystemID

getScreenSize :: X Rectangle
getScreenSize = io $ fmap head $ getScreenInfo =<< openDisplay ""

calculateBox :: ScreenInfo -> (Dimension -> Dimension -> Bool) -> Rectangle -> Bool
calculateBox si op (Rectangle _ _ sw sh) =
    let wide = liftM (op sw) (width  si) >>= Just
        tall = liftM (op sh) (height si) >>= Just
    in any id $ catMaybes [wide, tall]
