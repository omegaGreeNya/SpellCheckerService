{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server 
   ( Handle(..)
   , runServer
   ) where

import Cheops.Logger
import Network.Wai.Handler.Warp
import Servant

import qualified Endpoints.CheckText as CheckText
import qualified SpellChecker (Handle)

-- | Server Handle
data Handle = Handle
   { hSpellChecker :: SpellChecker.Handle IO
   , hServerPort   :: Int
   , hLogger       :: LoggerEnv
   }

-- >>

type API = 
   "api" 
      :> CheckText.Route

spellCheckAPI :: Proxy API
spellCheckAPI = Proxy

runServer :: Handle -> IO ()
runServer h@Handle{..} = run hServerPort (app h)

app :: Handle -> Application
app h = serve spellCheckAPI (server h)

server :: Handle -> Server API
server Handle{..} =
   CheckText.checkText (CheckText.createHandle hSpellChecker hLogger)