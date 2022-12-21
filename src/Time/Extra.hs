module Time.Extra
   ( formatUTCTimeForFilePath
   ) where

import Data.Time.Clock

formatUTCTimeForFilePath :: UTCTime -> String
formatUTCTimeForFilePath utcTime = formatTimeString $ show utcTime

formatTimeString :: String -> String
formatTimeString = let
   replacer []       = []
   replacer (' ':xs) = '_' : replacer xs
   replacer (':':xs) = '-' : replacer xs
   replacer ('.':xs) = '-' : replacer xs
   replacer (x  :xs) = x   : replacer xs
   in replacer