module LogChallenge.ChallengeMain (runChallenge) where

import ClassyPrelude

import qualified Data.Text as T

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Attoparsec
import Control.Monad.Trans.Resource
import Data.Attoparsec.Text

import LogChallenge.Parsing
import LogChallenge.Counters
import LogChallenge.Funnels

splitOnTripleNewline :: Parser Text
splitOnTripleNewline = do
    s <- manyTill' anyChar (eitherP (string "\n\n\n") endOfInput)
    return $ T.pack s

filterValidParses :: MonadResource m => Conduit (Either ParseError (PositionRange, b)) m b
filterValidParses = rightsC $= CC.mapM (return . snd)

rightsC :: MonadResource m => Conduit (Either a b) m b
rightsC = awaitForever filterRight
    where
        filterRight (Left _) = rightsC
        filterRight (Right x) = yield x >> rightsC

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
                        yield theLog

runFunnels :: (MonadResource m, MonadIO m) => IORef [FunnelData] -> (Conduit LogSuccess m LogSuccess)
runFunnels funnelDataRef = do
            awaitForever $ handleLog
                where handleLog theLog = do
                        funnelData <- liftIO $ readIORef funnelDataRef
                        let newFunnels = map (\fd -> updateFunnelWithLog theLog fd) funnelData
                        liftIO $ writeIORef funnelDataRef newFunnels
                        yield theLog

runChallenge :: IO ()
runChallenge = do
    let countersData = map initCounterData allCounters
        funnelsData  = map initFunnelData allFunnels
    countersRef <- newIORef countersData
    funnelsRef  <- newIORef funnelsData
    runResourceT
         $ CC.sourceFile "heyzap-example-log"
        $$ conduitParserEither splitOnTripleNewline
        =$ filterValidParses
        -- Parse text into records
        =$ parseLogs
        =$ filterLogSuccess
        -- Analyze parsed logs
        =$ runCounters countersRef
        =$ runFunnels funnelsRef
        =$ CC.sinkNull
        -- =$ CC.map (\x -> (T.pack $ show x) ++ "\n")
        -- =$ CC.sinkFile "output.txt"
    updatedCounters <- readIORef countersRef
    updatedFunnels  <- readIORef funnelsRef
    mapM_ print (map showCounterData updatedCounters)
    mapM_ print (map showFunnelData updatedFunnels)