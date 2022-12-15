-- Powershell
-- curl -Method Post -ContentType "application/json" -InFile ./data.json -Uri "http://localhost:8081/check"
{-# LANGUAGE DataKinds #-}

module Server 
   ( Handle(..)
   , runServer
   ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

import SpellChecker (checkText, SpellCheckResult)

import qualified SpellChecker (Handle)

data Handle = Handle
   { hSpellChecker :: SpellChecker.Handle IO
   , hServerPort   :: Int
   }

data TextToCheck = TextToCheck
   { _text :: Text
   } deriving Generic

instance ToJSON TextToCheck
instance FromJSON TextToCheck

type API = "check" :> ReqBody '[JSON] TextToCheck :> Post '[JSON] SpellCheckResult

runServer :: Handle -> IO ()
runServer h@Handle{..} = run hServerPort (app h)

spellCheckAPI :: Proxy API
spellCheckAPI = Proxy

server :: Handle -> Server API
server Handle{..} = checkText'
   where
      checkText' :: TextToCheck -> Handler SpellCheckResult
      checkText' TextToCheck{..} = 
         liftIO $ checkText hSpellChecker _text

app :: Handle -> Application
app h = serve spellCheckAPI (server h)