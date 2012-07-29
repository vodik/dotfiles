module XMonad.Util.Commands.Common where

import Data.Monoid
import XMonad hiding (spawn)
import XMonad.Util.Commands

data Select = Desktop | Window
data Scrot  = Scrot FilePath Select

scrot :: MonadIO m => FilePath -> Select -> m ()
scrot path Desktop = delayedSpawn 100 $ "scrot" :+ [ path ]
scrot path Window  = delayedSpawn 100 $ "scrot" :+ [ "-s", path ]

data Action = On | Off | Toggle
            | Increase Int | Decrease Int

data Amixer = Amixer String Action

amixer :: MonadIO m => String -> Action -> m ()
amixer ch act = spawn $ "amixer" :+ [ "-q", "set", ch ] <> actOn act

actOn :: Action -> [String]
actOn On           = return "on"
actOn Off          = return "off"
actOn Toggle       = return "toggle"
actOn (Increase v) = [ "on", show v <> "%+" ]
actOn (Decrease v) = [       show v <> "%-" ]
