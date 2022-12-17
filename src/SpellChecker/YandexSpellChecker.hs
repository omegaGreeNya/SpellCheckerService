{-# LANGUAGE DeriveGeneric #-}
module SpellChecker.YandexSpellChecker
    ( createHandle
    ) where
-- TO DO: Splitting text on 10k symbol may result in splitted word!
-- Handle request errors.

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (RequestBody(..))
import Network.HTTP.Simple
   ( Request, httpLBS, getResponseBody, setRequestHeaders
   , setRequestBody, setRequestMethod, setRequestPath
   , setRequestSecure, setRequestHost, defaultRequest)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import SpellChecker.Handle (Handle(..), TextError(..))

-- << Data Transfer Object, our wrapper for parsing responses.
data TextErrorDTO = TextErrorDTO
   { code :: Int
   , word :: Text
   , pos  :: Int
   } deriving (Eq, Show, Generic)

instance ToJSON TextErrorDTO
instance FromJSON TextErrorDTO

textErrorFromDTO :: TextErrorDTO -> TextError
textErrorFromDTO TextErrorDTO{..} = TextError word
-- >>

-- << API related constants
yandexMaxTextLength :: Int
yandexMaxTextLength = 10000

errorTooManyErrorsCode :: Int
errorTooManyErrorsCode = 4
-- >>

-- << SpellChecker Handle construction
createHandle :: MonadIO m => Handle m
createHandle = Handle processText

processText :: MonadIO m => Text -> m [TextError]
processText ""   = return mempty
processText text = do
   (result, rest) <- checkText text
   restResult <- processText rest
   return $ result <> restResult

-- >>

-- << Implementation

-- | Creates a single API call, tries to send as much text as possible,
-- returns call result and unprocessed text.
checkText :: MonadIO m => Text -> m ([TextError], Text)
checkText text = do
   let (request, rest) = constructSpellCheckRequest text
   response <- liftIO $ httpLBS request
   
   let resultDTO = parseResponse $ getResponseBody response
   if resultDTO == []
      then
         return ([], rest)
      else do
         let (resultDTO', unprocessedText) = splitOnUnchecked resultDTO text
         return (map textErrorFromDTO resultDTO', unprocessedText <> rest)
   
   where 
      splitOnUnchecked errorDTO sendedText = 
         let lastErr = last errorDTO
         in if code lastErr == errorTooManyErrorsCode
            then (init errorDTO, T.drop (pos lastErr) sendedText)
            else (errorDTO, "")

-- | Parses raw json bytesting.
parseResponse :: ByteString -> [TextErrorDTO]
parseResponse rawJson =
   maybe [] id $ decode rawJson 

-- | Creates request, returns request and unfitted part of text.
constructSpellCheckRequest :: Text -> (Request, Text)
constructSpellCheckRequest text =
   (constructSpellCheckRequestUnsafe text', rest)
   where (text', rest) = T.splitAt yandexMaxTextLength text

-- | Unsafe, doesn't handles max size of text in single request.
constructSpellCheckRequestUnsafe :: Text -> Request
constructSpellCheckRequestUnsafe text = 
   setRequestMethod "POST"
   $ setRequestSecure False
   $ setRequestHost "speller.yandex.net"
   $ setRequestPath "/services/spellservice.json/checkText"
   $ setRequestHeaders [("Content-Type", "application/x-www-form-urlencoded")]
   $ setRequestBody (RequestBodyBS $ "text=" <> (T.encodeUtf8 text))
   $ defaultRequest

-- >>