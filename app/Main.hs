{-# LANGUAGE DataKinds #-}

module Main (main) where
-- TO-DO
-- Proper up-layer of the app, with config, handle creation conveer, etc..


import Colog (logTextStdout)
import System.Environment (getArgs)

import Server (runServer)

import qualified Server as S (Handle(..))
import qualified SpellChecker.YandexSpellChecker as Y (Config(..), createHandle)

defaultPort :: Int
defaultPort = 8081

main :: IO ()
main = do
   args <- getArgs
   case args of
      []
         -> runWithPort defaultPort
      (port:_)
         -> runWithPort (read port)

runWithPort :: Int -> IO ()
runWithPort port =
   runServer $ S.Handle (Y.createHandle yandexSpellCheckCfg logTextStdout) port logTextStdout

yandexSpellCheckCfg :: Y.Config IO
yandexSpellCheckCfg = Y.Config
   { cfgMaxConnectionAttempts = 2
   , cfgLogger = logTextStdout
   }