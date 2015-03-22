module LogChallenge.ChallengeMain (runChallenge) where

import ClassyPrelude

import qualified Data.Text as T
import Data.Either (isRight)

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Attoparsec
import Control.Monad.Trans.Resource
import Data.Attoparsec.Text

import LogChallenge.Parsing
import LogChallenge.Counters

testParser :: Parser Text
testParser = do
    s <- manyTill' anyChar (eitherP (string "\n\n\n") endOfInput)
    return $ T.pack s


-- Using fromRight is ugly; find a better way.
-- Maybe look into conduit functions that await a value matching a certain function.
-- or just look into how to not yield a value. 
-- filterValidParses :: MonadResource m => Conduit (Either ParseError (PositionRange, b)) m b
-- filterValidParses = CC.filter isRight
--                     =$ CC.mapM (return . snd . fromRight)

filterValidParses :: MonadResource m => Conduit (Either ParseError (PositionRange, b)) m b
filterValidParses = rightsC $= CC.mapM (return . snd)

-- filterInvalidParses :: MonadResource m => Conduit (Either ParseError (PositionRange, b)) m ParseError
-- filterInvalidParses = leftsC

-- fromRight :: Either a b -> b
-- fromRight (Right b) = b
-- fromRight (Left _) = error "fromRight found a Left value"

-- rights :: MonadResource m => Conduit (Either a b) m b
-- rights = CC.mapM go
--         where
--             go (Left _) = return ()
--             go (Right x) = yield x

leftsC :: MonadResource m => Conduit (Either a b) m a
leftsC = go
    where go = do
            val <- await
            case val of
                Nothing -> return ()
                Just e -> case e of
                    Left x -> yield x
                    Right x -> go

rightsC :: MonadResource m => Conduit (Either a b) m b
rightsC = go
    where go = do
            val <- await
            case val of
                Nothing -> return ()
                Just e -> case e of
                    Left _ -> go
                    Right x -> yield x >> rightsC

printOutput :: (MonadResource m, MonadIO m) => Conduit Text m Text
printOutput = do
        val <- await
        case val of
            Nothing -> return ()
            Just e -> do
                liftIO $ print e
                yield e >> printOutput

parseLogs :: MonadResource m => Conduit Text m (Either String Log)
parseLogs = CC.map (\t -> case parseOnly parseLog2 t of
                            left@(Left _) -> (traceShow (t ++ "\n\n") left)
                            Right l -> Right l
                    )


filterLogSuccess :: MonadResource m => Conduit (Either String Log) m LogSuccess
filterLogSuccess = awaitForever $ handleLog
                   where handleLog (Right (Success x)) = yield x
                         handleLog _ = filterLogSuccess

runCounters :: (MonadResource m, MonadIO m) => IORef [CounterData] -> (Conduit LogSuccess m LogSuccess) 
runCounters counterDataRef = do
            awaitForever $ handleLog
              where handleLog theLog = do
                        counterData <- liftIO $ readIORef counterDataRef
                        let newCounters = map (\cd -> updateWithLog theLog cd) counterData 
                        liftIO $ writeIORef counterDataRef newCounters
                        return theLog

runChallenge :: IO ()
runChallenge = do
    let countersData = map initCounterData allCounters
    countersRef <- newIORef countersData
    runResourceT
         $ CC.sourceFile "heyzap-example-log"
        $$ conduitParserEither testParser
        =$ filterValidParses
        -- =$ printOutput
        -- =$ conduitParserEither parseLog
        =$ parseLogs
        =$ filterLogSuccess
        =$ runCounters countersRef
        -- =$ CC.sinkNull
        =$ CC.map (\x -> (T.pack $ show x) ++ "\n")
        =$ CC.sinkFile "output.txt"
    updatedCounters <- readIORef countersRef
    print (map showCounterData updatedCounters)