module GuardLayout.Instances
    ( ScreenSize (..)
    , Hostname (..) ) where

import Control.Monad
import System.Posix.Unistd (getSystemID, nodeName)

import XMonad
import qualified XMonad.StackSet as W

import Graphics.X11 (Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)

import GuardLayout

newtype ScreenSize = ScreenWidth Dimension
    deriving (Show, Read)

newtype Hostname = Hostname String
    deriving (Show, Read)

instance Condition Hostname where
    getCondition ws (Hostname n) =
        liftM ((n ==) . nodeName) $ io getSystemID

instance Condition ScreenSize where
    getCondition ws (ScreenWidth w) = do
        (Rectangle _ _ sw _) <- liftM head $ io $ getScreenInfo =<< openDisplay ""
        return $ w <= sw
