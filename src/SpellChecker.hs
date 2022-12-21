{-# LANGUAGE RecordWildCards #-}
-- | This is interface module for SpellChecker part.
-- It should be used as import for mid-layer of the app.
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

-- | Result of text checking
data SpellCheckResult = SpellCheckResult
   { checkMark :: Int
   -- ^ Text mark in range of 0..5
   , checkErrors :: [TextError]
   -- ^ Founded errors in text
   } deriving (Show)

-- | Performs text check. In case of inner errors may return Nothing.
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