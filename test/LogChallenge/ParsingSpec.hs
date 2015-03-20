module LogChallenge.ParsingSpec (spec) where

import ClassyPrelude
import Test.Hspec
import LogChallenge.Parsing
import Text.RawString.QQ
import Data.Maybe (fromJust)

import Data.IP (IPv4, toIPv4)
import Network.URI (URI, parseURI)
import Data.Attoparsec.Text
import Data.Either (isRight)

validLog1 :: Text
validLog1 = [r|Processing HeyzapController#index (for 194.88.236.84 at 2009-07-10 06:53:28) [GET]
  Parameters: {"back"=>"true", "embed_key"=>"12affbbace", "action"=>"index", "ajax"=>"1", "controller"=>"heyzap", "embed"=>"1"}
Rendering template within layouts/widget_only
Rendering heyzap/index
Completed in 31ms (View: 22, DB: 1) | 200 OK [http://www.heyzap.com/embed?ajax=1&back=true&embed_key=12affbbace]|]

parsedLog1 :: Log
parsedLog1 = Log
              { controller = "HeyzapController"
              , method = "index"
              , ipAddress = toIPv4 [194, 88, 236, 84]
              , time = fromJust $ ((parseTime defaultTimeLocale "%F %T" "2009-07-10 06:53:28") :: Maybe UTCTime)
              , httpMethod = "GET"
              , completionTime = 31
              , statusCode = 200
              , uri = fromJust $ parseURI "http://www.heyzap.com/embed?ajax=1&back=true&embed_key=12affbbace"
              }

-- someLog :: Text
-- someLog = [r|Processing AnalyticsController#game_play_time (for 194.88.236.84 at 2009-07-10 06:55:18) [GET]
--   Parameters: {"permalink"=>"bloons", "time"=>"1", "embed_key"=>"12affbbace", "action"=>"game_play_time", "controller"=>"analytics", "embed"=>"1"}
-- Completed in 4ms (View: 0, DB: 0) | 200 OK [http://www.heyzap.com/analytics/game_play_time?embed=1&embed_key=12affbbace&permalink=bloons&time=1]|]

xmlLog :: Text
xmlLog = [r|Processing HeyzapController#service to xml (for 174.37.109.180 at 2009-07-10 06:55:19) [GET]
  Parameters: {"thumb_width"=>"120", "max_width"=>"600", "format"=>"xml", "category"=>"popular", "max_height"=>"600", "embed_key"=>"5323333cac", "action"=>"service", "secret_key"=>"365aa4fc67", "method"=>"get_game", "controller"=>"heyzap", "length"=>"4", "offset"=>"0", "thumb_height"=>"90"}
Rendering heyzap/service
Completed in 13ms (View: 10, DB: 2) | 200 OK [http://www.heyzap.com/heyzap/service?embed_key=5323333cac&format=xml&secret_key=365aa4fc67&method=get_game&length=4&offset=0&category=popular&thumb_width=120&thumb_height=90&max_width=600&max_height=600]|]

someLog :: Text
someLog = [r|Processing AnalyticsController#game_play_time (for 216.15.119.172 at 2009-07-10 07:00:03) [GET]
  Parameters: {"permalink"=>"stunt-dirt-bike", "time"=>"5", "embed_key"=>"12affbbace", "action"=>"game_play_time", "controller"=>"analytics", "embed"=>"1"}
Completed in 12ms (View: 1, DB: 2) | 200 OK [http://www.heyzap.com/analytics/game_play_time?embed=1&embed_key=12affbbace&permalink=stunt-dirt-bike&time=5]
Running hourly tasks at 2009-07-10 05:00:03 -0700
Fri Jul 10 07:00:03 -0500 2009 GAME_PLAY.rb Game play worker run
Fri Jul 10 07:00:03 -0500 2009 GAME_PLAY.rb processing 1|]

spec :: Spec
spec = do
    describe "Parsing" $ do
        it "should be true" $ do
            let (Right log) = parseOnly parseLog validLog1
            log `shouldBe` parsedLog1
        it "should parse" $ do
            let parsed = parseOnly parseLog xmlLog
            (isRight parsed) `shouldBe` True
        it "should parse" $ do
            let parsed = parseOnly parseLog someLog
            (isRight (traceShowId parsed)) `shouldBe` True