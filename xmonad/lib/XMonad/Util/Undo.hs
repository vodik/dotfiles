{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Util.Undo
    ( undoPop
    , undoPush
    ) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

data UndoOp = UndoOp { undo :: X () }

data UndoStack = UndoStack { stack :: [UndoOp] }
    deriving (Typeable)

instance ExtensionClass UndoStack where
    initialValue = UndoStack []

undoPop :: X ()
undoPop = do
    (f:xs) <- XS.gets stack
    XS.put $ UndoStack xs
    undo f

undoPush :: Int -> X () -> X ()
undoPush n f = do
    xs <- XS.gets stack
    XS.put . UndoStack $ UndoOp f : take (n - 1) xs
