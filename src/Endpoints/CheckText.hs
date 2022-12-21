{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
-- | Single module endpoint. Performs spell checking.
module Endpoints.CheckText
    ( Route
    , createHandle
    , checkText
    ) where
-- To do
-- Unclip IO from SpellChecker.Handle IO, if it actually
-- may deacrease boilerplate.
--    Motivation: Spell check may be performed pure.
--       For now, in case of pure monad, like Identety,
--       you should unwrap monad and return result to IO context.

import Control.Monad.IO.Class (liftIO)
import Cheops.Logger
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant

import SpellChecker (SpellCheckResult(..), TextError(..))

import qualified SpellChecker (Handle, checkText)

-- << Routing & DTOs
-- | POST ../CeckText
type Route =
   "CheckText" 
         :> ReqBody '[JSON] TextToCheck
         :> Post '[JSON] SpellCheckResultDTO

-- | Data model to capture incoming text to check.
data TextToCheck = TextToCheck
   { text :: Text
   } deriving Generic

instance ToJSON TextToCheck
instance FromJSON TextToCheck

-- | Wrapper around 'SpellCheckResult'. Sended back as check result.
data SpellCheckResultDTO = SpellCheckResultDTO
   { mark :: Int
   , errors :: [Text]
   } deriving (Show, Generic)

instance ToJSON SpellCheckResultDTO

spellCheckResultToDTO :: SpellCheckResult -> SpellCheckResultDTO
spellCheckResultToDTO SpellCheckResult{..} = 
   SpellCheckResultDTO
      checkMark
      (map errorWord checkErrors)
-- >>

data Handle = Handle
   { hSpellChecker :: SpellChecker.Handle IO
   -- ^ Spell checker
   , hLogger :: LoggerEnv
   }

-- | Creates check text endpoint handle. Caller shouldn't bother himself
-- with adapting LoggerEnv.
createHandle :: SpellChecker.Handle IO -> LoggerEnv -> Handle
createHandle hSpellChecker logger =
   Handle
      { hLogger = addNamespace "CheckText"
         $ addContext (sl "endpoint" ("CheckText" :: String)) logger
      , ..}

-- | Manages spell checking
checkText :: Handle -> TextToCheck -> Handler SpellCheckResultDTO
checkText Handle{..} TextToCheck{..} = do
   logInfo hLogger "Gotted POST CheckText request"
   mResult <- liftIO $ SpellChecker.checkText hSpellChecker text
   maybe
      (throwError err500)
      (return . spellCheckResultToDTO)
      mResult