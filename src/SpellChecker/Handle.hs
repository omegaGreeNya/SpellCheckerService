-- | Inner module, should be used only for implementation,
-- or for functions that utilize this Handle.
-- In any case, Handle data constructor should not be re-exported in general.
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

-- | Adds proper namespace for SpellChecker.Handle.
-- Use this function for Handle constraction.
adaptHandle :: Handle m -> Handle m
adaptHandle Handle{..} = Handle{hLogger = addNamespace "SpellChecker" hLogger, ..}