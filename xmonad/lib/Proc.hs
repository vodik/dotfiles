module Proc where

import Control.Applicative
import Control.Monad
import Control.Monad.List
import Data.Char
import Data.List
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath

pgrep :: String -> IO [(String, Int)]
pgrep cmd = filter ((cmd `isInfixOf`) . fst) <$> pids

pids :: IO [(String, Int)]
pids = runListT $ do
    proc <- ListT $ getDirectoryContents "/proc"

    guard $ all isDigit proc
    guard =<< lift (doesDirectoryExist $ "/proc" </> proc)

    stats <- lift . readFile $ "/proc" </> proc </> "stat"
    let (p : r : _) = words stats
        name = drop 1 . reverse . drop 1 $ reverse r
        pid  = read p
    return (name, pid)
