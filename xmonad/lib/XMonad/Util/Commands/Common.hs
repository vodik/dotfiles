module XMonad.Util.Commands.Common where

import Data.Monoid
import XMonad hiding (spawn)
import XMonad.Util.Commands

data Select = Desktop | Window
data Scrot  = Scrot FilePath Select

instance Command Scrot where
    exec (Scrot path Desktop) = execute "scrot" []
    exec (Scrot path Window)  = execute "scrot" ["-s", path]

scrot :: MonadIO m => FilePath -> Select -> m ()
scrot path sel = delayedSpawn 100 $ Scrot path sel

data Action = On | Off | Toggle
            | Increase Int | Decrease Int

data Amixer = Amixer String Action

instance Command Amixer where
    exec (Amixer ch act) = execute "amixer" $ [ "-q", "set", ch ] <> actOn act

actOn :: Action -> [String]
actOn On           = return "on"
actOn Off          = return "off"
actOn Toggle       = return "toggle"
actOn (Increase v) = [ "on", show v <> "%+" ]
actOn (Decrease v) = [       show v <> "%-" ]

amixer :: MonadIO m => String -> Action -> m ()
amixer ch act = spawn $ Amixer ch act
