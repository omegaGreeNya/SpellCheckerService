{-# LANGUAGE RecordWildCards #-}

module SpellChecker
    ( Handle
    , TextError(..)
    , SpellCheckResult(..)
    , checkText
    ) where

import Control.Monad (when)
import Colog ((<&))
import Data.Text (Text)
import Data.Maybe (isJust, isNothing)

import SpellChecker.Handle (Handle(..), TextError(..))

data SpellCheckResult = SpellCheckResult
   { checkMark :: Int
   , checkErrors :: [TextError]
   } deriving (Show)

checkText :: Monad m => Handle m -> Text -> m (Maybe SpellCheckResult)
checkText Handle{..} text = do
   mErrorList <- hSpellCheck text
   let result = do
         errorList <- mErrorList
         let mark = 5 - (min 5 (length errorList))
         return $ SpellCheckResult mark errorList
   when (isJust result)
      $ hLogger <& "Succesfully preformed spell check"
   when(isNothing result)
      $ hLogger <& "Errors occured during spell check. Given text wasn't checked"
   return result