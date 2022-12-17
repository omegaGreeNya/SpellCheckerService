{-# LANGUAGE RecordWildCards #-}

module SpellChecker
    ( Handle
    , TextError(..)
    , SpellCheckResult(..)
    , checkText
    ) where

import Data.Text (Text)

import SpellChecker.Handle (Handle(..), TextError(..))

data SpellCheckResult = SpellCheckResult
   { checkMark :: Int
   , checkErrors :: [TextError]
   } deriving (Show)

checkText :: Monad m => Handle m -> Text -> m SpellCheckResult
checkText Handle{..} text = do
   errorList <- hSpellCheck text
   let mark = 5 - (min 5 (length errorList))
   return $ SpellCheckResult mark errorList