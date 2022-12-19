{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SpellChecker.YandexSpellChecker
    ( Config(..)
    , createHandle
    ) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Colog (LogAction, (<&))
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (RequestBody(..))
import Network.HTTP.Simple
   ( Response, Request, HttpException, httpLBS, getResponseBody, setRequestHeaders
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

data Config m = Config
   { cfgMaxConnectionAttempts :: Int
   , cfgLogger                :: LogAction m Text
   }

-- << SpellChecker Handle construction
createHandle :: MonadIO m => Config m -> LogAction m Text -> Handle m
createHandle cfg spellCheckLogger = Handle (processText cfg) spellCheckLogger

processText :: MonadIO m => Config m -> Text -> m (Maybe [TextError])
processText _ ""   = return $ Just mempty
processText cfg text = do
   results <- mapM (checkText cfg) $ splitTextByLimit yandexMaxTextLength text
   return . fmap concat $ sequence results

-- >>

-- << Implementation

-- | Creates a single API call, tries to send as much text as possible,
-- returns call result and unprocessed text.
checkText :: MonadIO m => Config m -> Text -> m (Maybe [TextError])
checkText cfg text = do
   -- Step one, check as much text as we can.
   mResponse <- (makeHttpRequestLBS cfg) $ constructSpellCheckRequest text
   let
      mResult = do
         response <- mResponse
         errorDTOs <- decode $ getResponseBody response
         if null errorDTOs
            then
               return ([], "")
            else do
               let (checkedErrorDTOs, unprocessedText) = splitOnUnchecked errorDTOs text
               return (map textErrorFromDTO checkedErrorDTOs, unprocessedText)
   -- Step two, in case of check error, return Nothing, otherwise process rest of text.
   case mResult of
      Nothing -> return Nothing
      Just (errors, "") -> return $ Just errors
      Just (errors, unprocessedText) -> do
         mRestErrors <- checkText cfg unprocessedText
         return $ do
            restErrors <- mRestErrors
            return $ errors <> restErrors
   
   where
      -- This function handles errorTooManyErrorsCode
      splitOnUnchecked errorDTOs sendedText = 
         let lastErr = last errorDTOs
         in if code lastErr == errorTooManyErrorsCode
            then (init errorDTOs, T.drop (pos lastErr) sendedText)
            else (errorDTOs, "")


-- | Tries to make http request until max attempt count isn't exhausted.
makeHttpRequestLBS :: MonadIO m => Config m -> Request -> m (Maybe (Response ByteString))
makeHttpRequestLBS cfg@Config{..} request = do
   let runner 0 = do
         cfgLogger <& "Connecting attempts exhausted. Yandex API is not avaible"
         return Nothing
       runner remainingAttemptsCount = do
         callResult <- makeHttpRequestLBS' cfg request
         case callResult of
            Nothing -> runner (remainingAttemptsCount - 1)
            result -> return result
   runner cfgMaxConnectionAttempts

-- | Tries to make http request, returns Nothing on network fail.
makeHttpRequestLBS' :: MonadIO m => Config m -> Request -> m (Maybe (Response ByteString))
makeHttpRequestLBS' Config{..} request = do
   cfgLogger <& "Making Yandex API call"
   eResult <- liftIO . try $ httpLBS request
   case eResult of
      Left err -> do
         cfgLogger <& "Couldn't get answer from Yandex API"
         handleHttpException cfgLogger err
         return Nothing
      Right result -> do
         cfgLogger <& "Succesfully made Yandex API call"
         return $ Just result

-- | Splits text on parts of specified size.
-- Also, all words remains unsplitted.
-- Due unsplit requerment, some parts may be more than specified size,
-- but contain exactly one word (word with size bigger than limit).
--
-- Warning: This is distructive function, extra withespaces would be ommited.
splitTextByLimit :: Int -> Text -> [Text]
splitTextByLimit charLimit textToSplit =
   process "" (T.words textToSplit)
   where 
      process "" [] = []
      process "" (w:ws) = process w ws
      process acc [] = [acc]
      process acc (w:ws) =
         if (T.length acc) + (T.length w) >= charLimit
            then acc : (process "" ws)
            else process (acc <> " " <> w) ws

-- | Doesn't handles max size of text in single request.
constructSpellCheckRequest :: Text -> Request
constructSpellCheckRequest text = 
   setRequestMethod "POST"
   $ setRequestSecure False
   $ setRequestHost "speller.yandex.net"
   $ setRequestPath "/services/spellservice.json/checkText"
   $ setRequestHeaders [("Content-Type", "application/x-www-form-urlencoded")]
   $ setRequestBody (RequestBodyBS $ "text=" <> (T.encodeUtf8 text))
   $ defaultRequest
-- >>

handleHttpException :: LogAction m Text -> HttpException -> m ()
handleHttpException logger err = logger <& (T.pack . show $ err)