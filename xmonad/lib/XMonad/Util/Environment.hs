module XMonad.Util.Environment
    ( env
    , getBrowser
    , getHome
    ) where

import Control.Applicative
import Data.Maybe
import System.Environment (getEnvironment)

env :: String -> IO (Maybe String)
env = (<$> getEnvironment) . lookup

getBrowser :: String -> IO String
getBrowser = (<$> env "BROWSER") . fromMaybe

getHome :: IO String
getHome = fromMaybe "/home/simongmzlj" <$> env "HOME"
