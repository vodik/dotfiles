module Dzen2
    ( dzenIcon
    , dzenAction
    ) where

import XMonad.Hooks.DynamicLog

dzenIcon :: String -> String
dzenIcon = wrap "^i(" ")"

dzenAction :: String -> String -> String
dzenAction cmd = wrap "^ca(1," ")" cmd `wrap` "^ca()"
