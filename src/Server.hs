-- Powershell
-- curl -Method Post -ContentType "application/json" -InFile ./data.json -Uri "http://localhost:8081/check"
{-# LANGUAGE DataKinds #-}

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

data Handle = Handle
   { hSpellChecker :: SpellChecker.Handle IO
   , hServerPort   :: Int
   }

data TextToCheck = TextToCheck
   { text :: Text
   } deriving Generic

instance ToJSON TextToCheck
instance FromJSON TextToCheck

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

type API = 
   "api" 
      :> "CheckText" 
         :> ReqBody '[JSON] TextToCheck :> Post '[JSON] SpellCheckResultDTO

runServer :: Handle -> IO ()
runServer h@Handle{..} = run hServerPort (app h)

spellCheckAPI :: Proxy API
spellCheckAPI = Proxy

server :: Handle -> Server API
server Handle{..} = checkText'
   where
      checkText' :: TextToCheck -> Handler SpellCheckResultDTO
      checkText' TextToCheck{..} = do
         result <- liftIO $ checkText hSpellChecker text
         return $ spellCheckResultToDTO result

app :: Handle -> Application
app h = serve spellCheckAPI (server h)