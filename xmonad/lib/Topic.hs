{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.TopicSpace
-- Copyright   :  (c) Nicolas Pouillard
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Turns your workspaces into a more topic oriented system.
-----------------------------------------------------------------------------

module Topic
  (
  -- * Overview
  -- $overview

  -- * Usage
  -- $usage
   Topic
  , Dir
  , TopicConfig(..)
  , defaultTopicConfig
  , getLastFocusedTopics
  , setLastFocusedTopic
  , reverseLastFocusedTopics
  , pprWindowSet
  , topicActionWithPrompt
  , topicAction
  , currentTopicAction
  , switchTopic
  , switchNthLastFocused
  , shiftNthLastFocused
  , currentTopicDir
  , (>*>)
  )
where

import XMonad

import Data.List
import Data.Maybe (fromMaybe, isNothing, listToMaybe, fromJust)
import Data.Ord
import qualified Data.Map as M
import Control.Monad (liftM2,when,unless,replicateM_)
import System.IO

import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Workspace

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog (PP(..))
import qualified XMonad.Hooks.DynamicLog as DL

import XMonad.Util.Run (spawnPipe)
import qualified XMonad.Util.ExtensibleState as XS

-- | An alias for @flip replicateM_@
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_
infix >*>

-- | 'Topic' is just an alias for 'WorkspaceId'
type Topic = WorkspaceId

-- | 'Dir' is just an alias for 'FilePath' but should points to a directory.
type Dir = FilePath

-- | Here is the topic space configuration area.
data TopicConfig a = TopicConfig
    { topicDirs          :: M.Map Topic a
    -- ^ This mapping associate a directory to each topic.
    , topicActions       :: M.Map Topic (X ())
    -- ^ This mapping associate an action to trigger when
    -- switching to a given topic which workspace is empty.
    , defaultTopicAction :: Topic -> X ()
    -- ^ This is the default topic action.
    , defaultTopic       :: Topic
    -- ^ This is the default topic.
    , maxTopicHistory    :: Int
    -- ^ This setups the maximum depth of topic history, usually
    -- 10 is a good default since we can bind all of them using
    -- numeric keypad.
    }

defaultTopicConfig :: TopicConfig a
defaultTopicConfig = TopicConfig
    { topicDirs = M.empty
    , topicActions = M.empty
    , defaultTopicAction = const (ask >>= spawn . terminal . config)
    , defaultTopic = "1"
    , maxTopicHistory = 10
    }

newtype PrevTopics = PrevTopics
    { getPrevTopics :: [String]
    } deriving (Read,Show,Typeable)

instance ExtensionClass PrevTopics where
    initialValue = PrevTopics []
    extensionType = PersistentExtension

-- | Returns the list of last focused workspaces the empty list otherwise.
getLastFocusedTopics :: X [String]
getLastFocusedTopics = XS.gets getPrevTopics

-- | Given a 'TopicConfig', the last focused topic, and a predicate that will
-- select topics that one want to keep, this function will set the property
-- of last focused topics.
setLastFocusedTopic :: Topic -> (Topic -> Bool) -> X ()
setLastFocusedTopic w predicate =
  XS.modify $ PrevTopics
    . seqList . nub . (w:) . filter predicate
    . getPrevTopics
  where seqList xs = length xs `seq` xs

-- | Reverse the list of "last focused topics"
reverseLastFocusedTopics :: X ()
reverseLastFocusedTopics =
  XS.modify $ PrevTopics . reverse . getPrevTopics

-- | This function is a variant of 'DL.pprWindowSet' which takes a topic configuration
-- and a pretty-printing record 'PP'. It will show the list of topics sorted historically
-- and highlighting topics with urgent windows.
pprWindowSet :: TopicConfig Dir -> PP -> X String
pprWindowSet tg pp = do
    winset <- gets windowset
    urgents <- readUrgents
    let empty_workspaces = map W.tag $ filter (isNothing . W.stack) $ W.workspaces winset
        maxDepth = maxTopicHistory tg
    setLastFocusedTopic (W.tag . W.workspace . W.current $ winset)
                        (`notElem` empty_workspaces)
    lastWs <- getLastFocusedTopics
    let depth topic = fromJust $ elemIndex topic (lastWs ++ [topic])
        add_depth proj topic = proj pp . (((topic++":")++) . show) . depth $ topic
        pp' = pp { ppHidden = add_depth ppHidden, ppVisible = add_depth ppVisible }
        sortWindows = take maxDepth . sortBy (comparing $ depth . W.tag)
    return $ DL.pprWindowSet sortWindows urgents pp' winset

-- | Given a prompt configuration and a topic configuration, triggers the action associated with
-- the topic given in prompt.
topicActionWithPrompt :: XPConfig -> TopicConfig a -> X ()
topicActionWithPrompt xp tg = workspacePrompt xp (liftM2 (>>) (switchTopic tg) (topicAction tg))

-- | Given a configuration and a topic, triggers the action associated with the given topic.
topicAction :: TopicConfig a -> Topic -> X ()
topicAction tg topic = fromMaybe (defaultTopicAction tg topic) $ M.lookup topic $ topicActions tg

-- | Trigger the action associated with the current topic.
currentTopicAction :: TopicConfig a -> X ()
currentTopicAction tg = topicAction tg =<< gets (W.tag . W.workspace . W.current . windowset)

-- | Switch to the given topic.
switchTopic :: TopicConfig a -> Topic -> X ()
switchTopic tg topic = do
  windows $ W.greedyView topic
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) $ topicAction tg topic

-- | Switch to the Nth last focused topic or failback to the 'defaultTopic'.
switchNthLastFocused :: TopicConfig a -> Int -> X ()
switchNthLastFocused tg depth = do
  lastWs <- getLastFocusedTopics
  switchTopic tg $ (lastWs ++ repeat (defaultTopic tg)) !! depth

-- | Shift the focused window to the Nth last focused topic, or fallback to doing nothing.
shiftNthLastFocused :: Int -> X ()
shiftNthLastFocused n = do
  ws <- fmap (listToMaybe . drop n) getLastFocusedTopics
  whenJust ws $ windows . W.shift

-- | Returns the directory associated with current topic returns the empty string otherwise.
currentTopicDir :: TopicConfig Dir -> X String
currentTopicDir tg = do
  topic <- gets (W.tag . W.workspace . W.current . windowset)
  return . fromMaybe "" . M.lookup topic $ topicDirs tg
