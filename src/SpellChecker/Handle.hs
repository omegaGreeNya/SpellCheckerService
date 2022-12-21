module SpellChecker.Handle
    ( Handle(..)
    , TextError(..)
    , adaptHandle
    ) where

import Cheops.Logger
import Data.Text (Text)

data Handle m = Handle
   { hSpellCheck :: Text -> m (Maybe [TextError])
   , hLogger     :: LoggerEnv
   }

data TextError = TextError 
   { errorWord :: Text
   } deriving (Show)

adaptHandle :: Handle m -> Handle m
adaptHandle Handle{..} = Handle{hLogger = addNamespace "SpellChecker" hLogger, ..}