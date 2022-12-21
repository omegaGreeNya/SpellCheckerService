{-# LANGUAGE DataKinds #-}

module Main (main) where
-- TO-DO
-- Proper up-layer of the app, with config, handle creation conveer, etc..


import Cheops.Logger
import Colog.Core (LogAction)
import Colog.Json.Action (logToHandle)
import Colog.Json.Internal.Structured (Message)
import Data.Time.Clock (getCurrentTime)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.IO

import Server (runServer)
import Time.Extra (formatUTCTimeForFilePath)

import qualified Server as S (Handle(..))
import qualified SpellChecker.YandexSpellChecker as Y (Config, createConfig, createHandle)

defaultPort :: Int
defaultPort = 8081

main :: IO ()
main = do
   logFilePath <- getLogFilePath
   withMainLogger logFilePath $ \logger -> do
      args <- getArgs
      let 
         serverPort = case args of
            []
               -> defaultPort
            (port:_)
               -> read port
         
         spellCheckLogger = logger
         yandexSpellConfig = yandexSpellCheckCfg logger
         yandexSpellHandle = Y.createHandle yandexSpellConfig spellCheckLogger
         serverLogger = logger
      
      runServer $ S.Handle yandexSpellHandle serverPort serverLogger

yandexSpellCheckCfg :: LoggerEnv -> Y.Config
yandexSpellCheckCfg logger =
   Y.createConfig
      connectionAttempts
      logger
   where
      connectionAttempts = 2 :: Int

-- stdoutLogger :: LoggerEnv
-- stdoutLogger = mkLogger stdoutLogAct

withMainLogger :: FilePath -> (LoggerEnv -> IO a) -> IO a
withMainLogger logFilePath f = 
   withMainLogAct logFilePath (f . mkLogger)

stdoutLogAct :: LogAction IO Message
stdoutLogAct = logToHandle stdout

withFileLogAct :: FilePath -> (LogAction IO Message -> IO a) -> IO a
withFileLogAct logFilePath f =
   withFile logFilePath AppendMode $ \logFileHandle ->
      f $ logToHandle logFileHandle

withMainLogAct :: FilePath -> (LogAction IO Message -> IO a) -> IO a
withMainLogAct logFilePath f =
   withFileLogAct logFilePath $ \fileLogAct ->
      f $ stdoutLogAct <> fileLogAct

getLogFilePath :: IO FilePath
getLogFilePath = do
   currentTime <- getCurrentTime
   currentPath <- getCurrentDirectory
   let
      path = currentPath
         <> "\\logs\\log-"
         <> (formatUTCTimeForFilePath currentTime)
         <> ".log"
   createDirectoryIfMissing True $ currentPath <> "\\logs"
   return path