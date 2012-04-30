{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses #-}

module MPD where

import Control.Applicative
import Control.Monad
import qualified Network.MPD as MPD

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import qualified XMonad.Util.ExtensibleState as XS

data MPDConf = MPDConf
    { host :: Maybe String
    , port :: Maybe String
    } deriving (Read, Show, Typeable)

instance ExtensionClass MPDConf where
   initialValue  = MPDConf Nothing Nothing
   extensionType = PersistentExtension

changeHost :: XPConfig -> X ()
changeHost conf = inputPromptWithCompl conf "MPD_HOST" (historyCompletionP (== "MPD_HOST: "))
    ?+ setHost . ap ((>>) . guard . not . null) return

setHost :: Maybe String -> X ()
setHost h = XS.modify $ \conf -> conf { host = h }

withMPD :: MPD.MPD a -> X ()
withMPD cmd = XS.get >>= io . void . flip (liftM2 MPD.withMPD_ host port) cmd
