{-# LANGUAGE DataKinds #-}

module Main (main) where

import GHC.Generics
import Data.Aeson
import Data.Text
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = run 8081 app

type API = ReqBody '[JSON] TextToCheck :> Get '[JSON] TextToCheck

appAPI :: Proxy API
appAPI = Proxy

server :: Server API
server = return

app :: Application
app = serve appAPI server


-------------------

data TextToCheck = TextToCheck { _text :: Text } deriving Generic

instance FromJSON TextToCheck
instance ToJSON TextToCheck