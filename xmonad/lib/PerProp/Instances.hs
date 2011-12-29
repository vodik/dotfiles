module PerProp.Instances
    ( Tag (..)
    , Hostname (..) ) where

import XMonad
import qualified XMonad.StackSet as W

import Control.Monad
import System.Posix.Unistd (getSystemID, nodeName)

import PerProp

newtype Tag = Tag String
    deriving (Eq, Show, Read)

newtype Hostname = Hostname String
    deriving (Eq, Show, Read)

-- instance Condition Tag where
--     getCondition (W.Workspace i _ _) = return $ Tag i

instance Condition Hostname where
    getCondition ws (Hostname n) = do
    hostName <- fmap nodeName $ io getSystemID
    return $ hostName == n
