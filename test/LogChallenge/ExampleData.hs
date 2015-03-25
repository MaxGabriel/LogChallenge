module LogChallenge.ExampleData (parsedLog1, frontPageLog) where

import ClassyPrelude

import LogChallenge.Parsing (LogSuccess(..))
import Data.Maybe (fromJust)

import Data.IP (toIPv4)
import Network.URI (parseURI)
-- import Data.Either (isRight)

parsedLog1 :: LogSuccess
parsedLog1 = LogSuccess
              { controller = "HeyzapController"
              , method = "index"
              , ipAddress = toIPv4 [194, 88, 236, 84]
              , time = fromJust $ ((parseTime defaultTimeLocale "%F %T" "2009-07-10 06:53:28") :: Maybe UTCTime)
              , httpMethod = "GET"
              , completionTime = 31
              , statusCode = 200
              , uri = fromJust $ parseURI "http://www.heyzap.com/embed?ajax=1&back=true&embed_key=12affbbace"
              }

frontPageLog :: LogSuccess
frontPageLog = parsedLog1 { controller = "HomeController" }