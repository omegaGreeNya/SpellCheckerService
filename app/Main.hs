{-# LANGUAGE DataKinds #-}

module Main (main) where

import System.Environment (getArgs)

import Server (runServer)

import qualified Server as S (Handle(..))
import qualified SpellChecker.YandexSpellChecker as Y (createHandle)

main :: IO ()
main = do
   args <- getArgs
   case args of
      []
         -> runWithPort 8081
      (port:_)
         -> runWithPort (read port)

runWithPort :: Int -> IO ()
runWithPort port =
   runServer $ S.Handle (Y.createHandle) port
