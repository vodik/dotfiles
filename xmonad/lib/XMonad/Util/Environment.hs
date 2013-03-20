module XMonad.Util.Environment
    ( env
    , browser
    ) where

import System.Posix.Env (getEnv, setEnv)

env :: String -> String -> IO String
env var def = do
    v <- getEnv var
    case v of
        Nothing -> setEnv var def True >> return def
        Just x  -> return x

browser :: String -> IO String
browser = env "BROWSER"
