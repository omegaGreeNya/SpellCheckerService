module SpellChecker.Handle
    ( Handle(..)
    , TextError(..)
    ) where

import Data.Text

data Handle m = Handle
   { hSpellCheck :: Text -> m [TextError]
   }

data TextError = TextError 
   { errorCode :: Int
   , errorWord :: Text
   } deriving (Show)