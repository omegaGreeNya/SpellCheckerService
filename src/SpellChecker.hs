{-# LANGUAGE RecordWildCards #-}

module SpellChecker
    ( Handle
    , TextError(..)
    , SpellCheckResult(..)
    , checkText
    ) where

import Control.Monad.IO.Class (MonadIO)
import Cheops.Logger
import Data.Text (Text)

import SpellChecker.Handle (Handle(..), TextError(..))

data SpellCheckResult = SpellCheckResult
   { checkMark :: Int
   , checkErrors :: [TextError]
   } deriving (Show)

checkText :: MonadIO m => Handle m -> Text -> m (Maybe SpellCheckResult)
checkText Handle{..} text = do
   mErrorList <- hSpellCheck text
   let result = do
         errorList <- mErrorList
         let mark = 5 - (min 5 (length errorList))
         return $ SpellCheckResult mark errorList
   logResult result
   return result
   
   where
      logResult (Just _) =
         logInfo hLogger "Succesfully preformed spell check"
      logResult Nothing =
         logErr hLogger "Errors occured during spell check. Given text wasn't checked"