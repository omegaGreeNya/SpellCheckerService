{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- | This module is the hearth of app, that collects
-- all endpoints and runs them as needed.
--
-- New endpoint should provide it's route
-- and implementation (example: Endpoints.ChekText).
module Server 
   ( Handle(..)
   , runServer
   ) where
-- To Do
-- Extract Hanlde to separate module, something like App.Handler
-- and provide lins-like getters.
--    Motivation: In case of addig app configuration files, it would
--       be wise to separate constructing app handle from app config.
--       In that case, this Server module whould use provided app handle.
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