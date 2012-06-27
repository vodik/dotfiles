module XMonad.Util.Tmux ( tmuxPrompt ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.List
import System.FilePath
import System.Directory (getDirectoryContents, getAppUserDataDirectory)
import Data.List
import Data.Maybe
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import qualified Data.Map as M

type Action = String -> String -> X ()

runningSessions :: IO (M.Map String Action)
runningSessions = M.fromList <$> do
    sessions <- runProcessWithInput "tmux" [ "list-sessions", "-F", "#{session_name}" ] ""
    return . map (flip (,) attach) $ lines sessions

tmuxSessions :: IO (M.Map String Action)
tmuxSessions = handle (\(SomeException _) -> return M.empty) $ do
    dir <- getAppUserDataDirectory "tmux-sessions"
    fmap M.fromList . runListT $ do
        file <- ListT $ filter (not . isPrefixOf ".") <$> getDirectoryContents dir
        cmd  <- fmap (create . Just) . io . readFile $ dir </> file
        return (file, cmd)

tmuxPrompt :: XPConfig -> X ()
tmuxPrompt conf = do
    commands <- io $ liftA2 M.union runningSessions tmuxSessions
    inputPromptWithCompl conf "Tmux" (mkComplFunFromList' $ M.keys commands) ?+ attachTmux commands

attachTmux :: M.Map String Action -> String -> X ()
attachTmux commands t = do
    term <- asks $ terminal . config
    fromMaybe (create Nothing) (M.lookup t commands) term t

attach :: Action
attach term t = safeSpawn term [ "-r", t, "-e", tmux ]
  where tmux = "tmux attach -t " ++ t

create :: Maybe String -> Action
create cmd term t = safeSpawn term [ "-r", t, "-e", tmux ]
  where tmux = unwords $ [ "tmux new -s", t ] ++ maybe [] return cmd
