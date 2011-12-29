module GuardLayout.Instances
    ( Hostname (..) ) where

import XMonad
import qualified XMonad.StackSet as W

import Control.Monad
import System.Posix.Unistd (getSystemID, nodeName)

import GuardLayout

newtype Hostname = Hostname String
    deriving (Eq, Show, Read)

instance Condition Hostname where
    getCondition ws (Hostname n) =
        liftM ((n ==) . nodeName) $ io getSystemID

-- ifHostname :: Condition
-- ifHostname _ = liftM ((n ==) . nodeName) $ io getSystemID
