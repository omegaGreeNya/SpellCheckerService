{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SpellChecker.YandexSpellChecker
    (createHandle
    ) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
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

-- << SpellChecker Handle construction
createHandle :: MonadIO m => Handle m
createHandle = Handle processText

processText :: MonadIO m => Text -> m (Maybe [TextError])
processText ""   = return $ Just mempty
processText text = do
   results <- mapM checkText $ splitTextByLimit yandexMaxTextLength text
   return . fmap concat $ sequence results

-- >>

-- << Implementation

-- | Creates a single API call, tries to send as much text as possible,
-- returns call result and unprocessed text.
checkText :: MonadIO m => Text -> m (Maybe [TextError])
checkText text = do
   mResponse <- makeHttpRequestLBS $ constructSpellCheckRequest text
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
   
   case mResult of
      Nothing -> return Nothing
      Just (errors, "") -> return $ Just errors
      Just (errors, unprocessedText) -> do
         mRestErrors <- checkText unprocessedText
         return $ do
            restErrors <- mRestErrors
            return $ errors <> restErrors
   
   where
      splitOnUnchecked errorDTOs sendedText = 
         let lastErr = last errorDTOs
         in if code lastErr == errorTooManyErrorsCode
            then (init errorDTOs, T.drop (pos lastErr) sendedText)
            else (errorDTOs, "")

-- | Tries to make http request, returns Nothing on network fail.
makeHttpRequestLBS :: MonadIO m => Request -> m (Maybe (Response ByteString))
makeHttpRequestLBS request = do
   eResult <- liftIO . try $ httpLBS request
   return $ case eResult of
      Left (_ :: HttpException) -> Nothing
      Right result -> Just result

-- | Splits text on parts of specified size. Without ommited symbols.
-- Also, every word remains unsplitted.
-- Due unsplit requerment, some parts may be more than specified size,
-- but contain exactly one word (word with size bigger than limit).
-- Warning: This is distructive function, in terms of that concatination of result
-- may contain less spaces than original text between words.
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