module SpellChecker.Handle
    ( Handle(..)
    , TextError(..)
    ) where

import Data.Text

data Handle m = Handle
   { hSpellCheck :: Text -> m (Maybe [TextError])
   }

data TextError = TextError 
   { errorWord :: Text
   } deriving (Show)