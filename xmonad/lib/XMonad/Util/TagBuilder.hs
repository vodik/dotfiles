module XMonad.Util.TagBuilder where

import Control.Monad.Writer
import Data.Maybe (fromMaybe)

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.SortWindows

data Tweaks = Tweaks
    { mainWidth  :: Rational
    , imWidth    :: Rational
    , imGrid     :: Double
    , masterN    :: Int
    }

defaultTweaks :: Tweaks
defaultTweaks = Tweaks
    { mainWidth = 1/2
    , imWidth   = 1/5
    , imGrid    = 2/3
    , masterN   = 2
    }

type Tag = (WorkspaceId, [Query Bool])
type WSGenT = WriterT [Tag]

tag :: MonadIO m => String -> [Query Bool] -> WSGenT m ()
tag n r = tell $ return (n, r)

tagSet :: [Tag] -> [WorkspaceId]
tagSet = fmap fst

buildTags :: MonadIO m => WSGenT m () -> m [Tag]
buildTags = execWriterT

workspaceShift :: [Tag] -> MaybeManageHook
workspaceShift = foldr (\(w, t) -> mappend $ composeAll [ r -?> doShift w | r <- t ]) (return Nothing)

workspaceSort :: WorkspaceId -> [Tag] -> Query Any
workspaceSort w = composeAs Any . fromMaybe [] . lookup w
