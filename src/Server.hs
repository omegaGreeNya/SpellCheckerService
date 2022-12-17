{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Server 
   ( Handle(..)
   , runServer
   ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

import SpellChecker (checkText, SpellCheckResult(..), TextError(..))

import qualified SpellChecker (Handle)

-- | Server Handle
data Handle = Handle
   { hSpellChecker :: SpellChecker.Handle IO
   , hServerPort   :: Int
   }

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

type API = 
   "api" 
      :> "CheckText" 
         :> ReqBody '[JSON] TextToCheck :> Post '[JSON] SpellCheckResultDTO

spellCheckAPI :: Proxy API
spellCheckAPI = Proxy

runServer :: Handle -> IO ()
runServer h@Handle{..} = run hServerPort (app h)

app :: Handle -> Application
app h = serve spellCheckAPI (server h)

server :: Handle -> Server API
server Handle{..} = checkText'
   where
      checkText' :: TextToCheck -> Handler SpellCheckResultDTO
      checkText' TextToCheck{..} = do
         mResult <- liftIO $ checkText hSpellChecker text
         maybe 
            (throwError err500)
            (return . spellCheckResultToDTO)
            mResult
