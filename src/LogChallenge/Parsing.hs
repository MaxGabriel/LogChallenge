module LogChallenge.Parsing (Log(..), LogException(..), LogSuccess(..), parseLog, parseException, parseLog2, uriParameters) where

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AttoT
import ClassyPrelude
import Data.IP (IPv4, toIPv4)
-- import Data.Time.Format (parseTime)
-- import Data.Time.Clock (UTCTime)

import Data.Text (splitOn)
import Network.URI (URI, parseURI, uriQuery)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.URI (parseSimpleQuery)

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
    let mTime = parseTime defaultTimeLocale "%F %T" (unpack timeString) :: Maybe UTCTime
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
 

-- parseIp :: Parser IPv4
-- parseIp = toIPv4 <$> sepByN 3 decimal (char '.')

-- sepByN :: Applicative f => Int -> f a -> f b -> f [a]
-- sepByN 0 fa _  = (:[]) <$> fa
-- sepByN n fa fb = (:) <$> fa <* fb <*> sepByN (n-1) fa fb
