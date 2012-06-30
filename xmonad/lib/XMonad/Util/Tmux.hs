module XMonad.Util.Tmux
    ( TmuxSessions, TS(..)
    , tmuxPrompt
    ) where

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

type TmuxSessions = [TS]
data TS = TS { name :: String, command :: String }

type Action = String -> String -> X ()

runningSessions :: IO (M.Map String Action)
runningSessions = M.fromList <$> do
    sessions <- runProcessWithInput "tmux" [ "list-sessions", "-F", "#{session_name}" ] ""
    return . map (flip (,) attach) $ lines sessions

tmuxSessions :: TmuxSessions -> M.Map String Action
tmuxSessions = M.fromList . map (liftA2 (,) name (create . Just . command))

tmuxPrompt :: TmuxSessions -> XPConfig -> X ()
tmuxPrompt ts conf = do
    commands <- io $ (`M.union` tmuxSessions ts) <$> runningSessions
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
