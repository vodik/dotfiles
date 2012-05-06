module Proc where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.List
import Data.Char
import Data.List
import Data.Set (Set)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath
import qualified Data.Set as S

findCmd :: String -> Set (String, Int) -> Set (String, Int)
findCmd cmd = S.filter (cmdFilter cmd)

findPid :: String -> Set (String, Int) -> Set (String, Int)
findPid cmd = S.filter (cmdFilter cmd)

cmdFilter :: String -> (String, Int) -> Bool
cmdFilter cmd = fst >>> (cmd `isInfixOf`)

pidSet :: IO (Set (String, Int))
pidSet = runSetT $ do
    proc <- ListT $ getDirectoryContents "/proc"

    guard $ all isDigit proc
    guard =<< lift (doesDirectoryExist $ "/proc" </> proc)

    stats <- lift . readFile $ "/proc" </> proc </> "stat"
    let (p : r : _) = words stats
        name = drop 1 . reverse . drop 1 $ reverse r
        pid  = read p
    return (name, pid)
  where
    runSetT f = S.fromList <$> runListT f
