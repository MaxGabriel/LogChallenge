module LogChallenge.ParsingSpec (spec) where

import LogChallenge.ExampleData

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

-- parsedLog1 :: LogSuccess
-- parsedLog1 = LogSuccess
--               { controller = "HeyzapController"
--               , method = "index"
--               , ipAddress = toIPv4 [194, 88, 236, 84]
--               , time = fromJust $ ((parseTime defaultTimeLocale "%F %T" "2009-07-10 06:53:28") :: Maybe UTCTime)
--               , httpMethod = "GET"
--               , completionTime = 31
--               , statusCode = 200
--               , uri = fromJust $ parseURI "http://www.heyzap.com/embed?ajax=1&back=true&embed_key=12affbbace"
--               }

-- someLog :: Text
-- someLog = [r|Processing AnalyticsController#game_play_time (for 194.88.236.84 at 2009-07-10 06:55:18) [GET]
--   Parameters: {"permalink"=>"bloons", "time"=>"1", "embed_key"=>"12affbbace", "action"=>"game_play_time", "controller"=>"analytics", "embed"=>"1"}
-- Completed in 4ms (View: 0, DB: 0) | 200 OK [http://www.heyzap.com/analytics/game_play_time?embed=1&embed_key=12affbbace&permalink=bloons&time=1]|]

-- | Log with "to xml" after the method name. Logs can also have "to json", and theoretically other content types.
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

leadingNewlineLog :: Text
leadingNewlineLog = [r|

Processing HeyzapController#index (for 194.88.236.84 at 2009-07-10 06:53:28) [GET]
  Parameters: {"back"=>"true", "embed_key"=>"12affbbace", "action"=>"index", "ajax"=>"1", "controller"=>"heyzap", "embed"=>"1"}
Rendering template within layouts/widget_only
Rendering heyzap/index
Completed in 31ms (View: 22, DB: 1) | 200 OK [http://www.heyzap.com/embed?ajax=1&back=true&embed_key=12affbbace]|]

isSuccessful :: Either String Log -> Bool
isSuccessful (Right (Success _)) = True
isSuccessful _ = False

isException :: Either String Log -> Bool
isException (Right (Exception LogException)) = True
isException _ = False


exceptionLog :: Text
exceptionLog = [r|Processing ApplicationController#index (for 84.185.219.205 at 2009-07-10 06:53:59) [GET]

ActionController::RoutingError (No route matches "/heyzap/pop.mp3" with {:method=>:get, :canvas=>false}):
  passenger (2.2.2) lib/phusion_passenger/rack/request_handler.rb:81:in `process_request'
  passenger (2.2.2) lib/phusion_passenger/abstract_request_handler.rb:203:in `main_loop'
  passenger (2.2.2) lib/phusion_passenger/railz/application_spawner.rb:340:in `start_request_handler'
  passenger (2.2.2) lib/phusion_passenger/railz/application_spawner.rb:298:in `handle_spawn_application'
  passenger (2.2.2) lib/phusion_passenger/utils.rb:181:in `safe_fork'
  passenger (2.2.2) lib/phusion_passenger/railz/application_spawner.rb:296:in `handle_spawn_application'
  passenger (2.2.2) lib/phusion_passenger/abstract_server.rb:337:in `__send__'
  passenger (2.2.2) lib/phusion_passenger/abstract_server.rb:337:in `main_loop'
  passenger (2.2.2) lib/phusion_passenger/abstract_server.rb:187:in `start_synchronously'
  passenger (2.2.2) lib/phusion_passenger/abstract_server.rb:154:in `start'
  passenger (2.2.2) lib/phusion_passenger/railz/application_spawner.rb:192:in `start'
  passenger (2.2.2) lib/phusion_passenger/spawn_manager.rb:257:in `spawn_rails_application'
  passenger (2.2.2) lib/phusion_passenger/abstract_server_collection.rb:126:in `lookup_or_add'
  passenger (2.2.2) lib/phusion_passenger/spawn_manager.rb:251:in `spawn_rails_application'
  passenger (2.2.2) lib/phusion_passenger/abstract_server_collection.rb:80:in `synchronize'
  passenger (2.2.2) lib/phusion_passenger/abstract_server_collection.rb:79:in `synchronize'
  passenger (2.2.2) lib/phusion_passenger/spawn_manager.rb:250:in `spawn_rails_application'
  passenger (2.2.2) lib/phusion_passenger/spawn_manager.rb:153:in `spawn_application'
  passenger (2.2.2) lib/phusion_passenger/spawn_manager.rb:282:in `handle_spawn_application'
  passenger (2.2.2) lib/phusion_passenger/abstract_server.rb:337:in `__send__'
  passenger (2.2.2) lib/phusion_passenger/abstract_server.rb:337:in `main_loop'
  passenger (2.2.2) lib/phusion_passenger/abstract_server.rb:187:in `start_synchronously'

Rendering /var/www/heyzap.com/releases/20090710031526/public/404.html (404 Not Found)|]




spec :: Spec
spec = do
    describe "Parsing" $ do
        it "should be true" $ do
            let (Right log) = parseOnly parseLog validLog1
            log `shouldBe` (Success parsedLog1)
        it "should parse" $ do
            let parsed = parseOnly parseLog xmlLog
            isSuccessful parsed `shouldBe` True
        it "should parse" $ do
            let parsed = parseOnly parseLog someLog
            isSuccessful parsed `shouldBe` True
        it "should parse quoteLog" $ do
            let parsed = parseOnly parseLog quoteLog
            isSuccessful parsed `shouldBe` True
        it "should handle logs with leading newlines" $ do
            let parsed = parseOnly parseLog leadingNewlineLog
            isSuccessful parsed `shouldBe` True
        it "should parse exceptions as such" $ do
            let parsed = parseOnly parseException exceptionLog
            isException parsed `shouldBe` True



quoteLog :: Text
quoteLog = [r|Processing HeyzapController#index (for 24.132.117.221 at 2009-07-10 08:37:29) [GET]
  Parameters: {"category"=>"\"", "action"=>"index", "controller"=>"heyzap"}
Rendering template within layouts/heyzap
Rendering heyzap/index
Completed in 32ms (View: 24, DB: 1) | 200 OK [http://www.heyzap.com/"]|]