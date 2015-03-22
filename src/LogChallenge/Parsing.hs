{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module LogChallenge.Parsing (Log(..), LogException(..), LogSuccess(..), parseLog, parseException, parseLog2, uriParameters) where

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AttoT
import ClassyPrelude
import Data.IP (IPv4, toIPv4)
-- import Data.Time.Format (parseTime)
-- import Data.Time.Clock (UTCTime)

import Text.RawString.QQ
import Data.Text (splitOn)
import Network.URI (URI, parseURI, uriQuery)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.URI (parseSimpleQuery)

-- I should start a test suite for this, and start putting example data in there. 
-- This is way too fragile right now. 

-- data Log =
-- Change name to LogParseResult maybe? 
-- and just make LogException 1 thing ?
data Log = Success LogSuccess | Exception LogException deriving (Show, Eq)

-- And this could be 'Log' again
data LogSuccess = LogSuccess
        { controller :: Text -- ^ The Rails controller that handled the request (e.g. HomeController)
        , method :: Text -- ^ The ruby method handling the request (e.g. index)
        , ipAddress :: IPv4 -- ^ The user's IP address
        , time :: UTCTime -- ^ The time when the request occurred
        , httpMethod :: Text -- ^ The HTTP method used (e.g. GET, POST, etc.)
        , completionTime :: Integer -- ^ Time to complete request in milliseconds. TODO: Get a type for milliseconds
        , statusCode :: Integer -- ^ HTTP code returned by the server (e.g. 200)
        , uri :: URI -- ^ URI the request was made for.
        } deriving (Show, Eq)

uriParameters :: URI -> [(Text, Text)]
uriParameters uri = let bsParams = parseSimpleQuery (BS.pack $ uriQuery uri)
                    in map (\(k, v) -> (decodeUtf8 k, decodeUtf8 v)) bsParams

data LogException = LogException deriving (Show, Eq)

parseLog2 :: Parser Log
parseLog2 = parseLog <|> parseException


exampleLog :: Text
exampleLog = [r|Processing HeyzapController#index (for 194.88.236.84 at 2009-07-10 06:53:28) [GET]
  Parameters: {"back"=>"true", "embed_key"=>"12affbbace", "action"=>"index", "ajax"=>"1", "controller"=>"heyzap", "embed"=>"1"}
Rendering template within layouts/widget_only
Rendering heyzap/index
Completed in 31ms (View: 22, DB: 1) | 200 OK [http://www.heyzap.com/embed?ajax=1&back=true&embed_key=12affbbace]|]

parseLog :: Parser Log
parseLog = do
    -- Allow optional leading newlines, to handle the beginning of the log,
    -- plus one random case where there's an extra newline separator.
    _ <- AttoT.takeWhile (== '\n')
    -- skipMany skipSpace
    -- Example logfile line: 
    -- Processing HomeController#index (for 194.88.236.84 at 2009-07-10 06:53:28) [GET]
    controller <- "Processing " *> takeTill' (== '#')
    method <- takeTill' (== ' ')
    ipAddress <- takeTill' (== '(') >> "for " >> parseIp

    timeString <- " at " *> takeTill' (== ')')
    let mTime = (parseTime defaultTimeLocale "%F %T" (unpack timeString)) :: Maybe UTCTime
    time <- case mTime of
                Nothing -> fail "invalid date"
                Just d -> return d

    httpMethod <- " [" *> takeTill' (== ']')

    skipRestOfLine

    -- Next line: optional "Parameters" section
    -- This section escapes " as \" and newlines as \n.
    -- Thinking I should skip this part because the parameters information can be gotten from the URL (which can be parsed much easier)
    -- Sometimes rails adds "parameters" not from the URL to this section (e.g. controller and method name), so it may end up being necessary

    -- Find final line
    _ <- manyTill anyChar (string "\nCompleted in ")
    completionTime <- decimal
    statusCode <- takeTill' (== '|') >> " " *> decimal

    uriString <- takeTill' (== '[') >> takeTill' (== ']')

    -- Every URL in the logfile appears to be correctly escaped, *except* these: http://www.heyzap.com/"
    -- Ideally I'd figure out if there's some rhyme or reason to the escaping, but Rails no longer formats log files with the URL at the end, so I'm just URL encoding these manually.
    let uriStringEscaped = concatMap (\y -> if y == '"' then "%22" else [y]) uriString


    uri <- case parseURI (unpack uriStringEscaped) of
        Nothing -> fail "Invalid URL"
        Just uri -> return uri

    return $ Success LogSuccess{..}


parseIp :: Parser IPv4
parseIp = do
    d1 <- decimal
    _ <- char '.'
    d2 <- decimal
    _ <- char '.'
    d3 <- decimal
    _ <- char '.'
    d4 <- decimal
    return $ toIPv4 [d1, d2, d3, d4]

-------

parseException :: Parser Log
parseException = do
    _ <- manyTill anyChar (string "\n  passenger (") -- ridiculous, but I'm not sure there's a more reliable way.
    return $ Exception LogException


takeTill' :: (Char -> Bool) -> Parser Text
takeTill' func = takeTill func <* skip func

skipRestOfLine :: Parser ()
skipRestOfLine = skipWhile (not . isEndOfLine) >> endOfLine

-- parseParams :: Parser [(Text, Text)]
-- parseParams = do
--     params <- "  Parameters: {" *> takeTill' (== '}')
--     (parseParamPair `sepBy` (string ", "))

traceShowM1 :: (Show a, Monad m) => String -> a -> m ()
traceShowM1 s a = traceM $ s ++ show a

parseParams :: Parser [(Text, Text)]
parseParams = do

    params <- "  Parameters: {" *> takeTill' (== '}')
    traceShowM1 "params are " params
    let pairs = splitOn ", " params
    traceShowM1 "pairs are " pairs
    mapM (pure parseParamPair) pairs
    -- (parseParamPair `sepBy` (string ", "))
    

parseParamPair :: Parser (Text, Text)
parseParamPair = do
    key <- parseKeyOrValue
    value <- string "=>" >> parseKeyOrValue
    return (key, value)
    where
        parseKeyOrValue :: Parser Text
        parseKeyOrValue = char '"' >> takeTill' (== '"')



-- restOfLine :: Parser Text
-- restOfLine = takeTill' (== '\n')

-- | Consumes until a newline character, if one exists.
-- skipToEndOfLine :: Parser ()
-- skipToEndOfLine = (skipWhile (/= '\n') >> skip (== '\n')) <|> return ()

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
    
    
 

-- parseIp :: Parser IPv4
-- parseIp = toIPv4 <$> sepByN 3 decimal (char '.')

-- sepByN :: Applicative f => Int -> f a -> f b -> f [a]
-- sepByN 0 fa _  = (:[]) <$> fa
-- sepByN n fa fb = (:) <$> fa <* fb <*> sepByN (n-1) fa fb
