module SpellChecker.Handle
    ( Handle(..)
    , TextError(..)
    ) where

import Colog (LogAction)
import Data.Text

data Handle m = Handle
   { hSpellCheck :: Text -> m (Maybe [TextError])
   , hLogger     :: LogAction m Text
   }

data TextError = TextError 
   { errorWord :: Text
   } deriving (Show)