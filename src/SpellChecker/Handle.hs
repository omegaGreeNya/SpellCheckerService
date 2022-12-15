module SpellChecker.Handle
    ( Handle(..)
    , TextError(..)
    ) where

import Data.Aeson
import Data.Text
import GHC.Generics

data Handle m = Handle
   { hSpellCheck :: Text -> m [TextError]
   }

data TextError = TextError 
   { errorCode :: Int
   , errorWord :: Text
   } deriving (Show, Generic)

instance ToJSON TextError
instance FromJSON TextError