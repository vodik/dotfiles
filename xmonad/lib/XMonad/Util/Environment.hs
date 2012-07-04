module XMonad.Util.Environment
    ( env
    , getBrowser
    ) where

import Control.Applicative
import Data.Maybe
import System.Environment (getEnvironment)

env :: String -> IO (Maybe String)
env = (<$> getEnvironment) . lookup

getBrowser :: String -> IO String
getBrowser = (<$> env "BROWSER") . fromMaybe
