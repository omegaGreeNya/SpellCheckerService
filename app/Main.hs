{-# LANGUAGE DataKinds #-}

module Main (main) where
-- TO-DO
-- Proper up-layer of the app, with config, handle creation conveer, etc..


import Cheops.Logger
import Colog.Json.Action (logToHandle)
import System.Environment (getArgs)
import System.IO

import Server (runServer)

import qualified Server as S (Handle(..))
import qualified SpellChecker.YandexSpellChecker as Y (Config, createConfig, createHandle)

defaultPort :: Int
defaultPort = 8081

main :: IO ()
main = do
   args <- getArgs
   let 
      serverPort = case args of
         []
            -> defaultPort
         (port:_)
            -> read port
      
      spellCheckLogger = stdoutLogger
      yandexSpellHandle = Y.createHandle yandexSpellCheckCfg spellCheckLogger
      serverLogger = stdoutLogger
   
   runServer $ S.Handle yandexSpellHandle serverPort serverLogger

yandexSpellCheckCfg :: Y.Config
yandexSpellCheckCfg =
   Y.createConfig
      connectionAttempts
      stdoutLogger
   where
      connectionAttempts = 2 :: Int

stdoutLogger :: LoggerEnv
stdoutLogger = mkLogger $ logToHandle stdout