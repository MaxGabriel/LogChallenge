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
    mapM_ (print . showCounterData) updatedCounters
    mapM_ (print . showFunnelData) updatedFunnels



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

parseLogs :: MonadResource m => Conduit Text m (Either String Log)
parseLogs = CC.map (parseOnly parseLog2)


filterLogSuccess :: MonadResource m => Conduit (Either String Log) m LogSuccess
filterLogSuccess = awaitForever handleLog
                   where handleLog (Right (Success x)) = yield x
                         handleLog _ = filterLogSuccess

runCounters :: (MonadResource m, MonadIO m) => IORef [CounterData] -> Conduit LogSuccess m LogSuccess
runCounters counterDataRef = awaitForever $ \theLog -> do
                                counterData <- liftIO $ readIORef counterDataRef
                                let newCounters = map (updateWithLog theLog) counterData 
                                liftIO $ writeIORef counterDataRef newCounters
                                yield theLog

runFunnels :: (MonadResource m, MonadIO m) => IORef [FunnelData] -> Conduit LogSuccess m LogSuccess
runFunnels funnelDataRef = awaitForever $ \theLog -> do
                                funnelData <- liftIO $ readIORef funnelDataRef
                                let newFunnels = map (updateFunnelWithLog theLog) funnelData
                                liftIO $ writeIORef funnelDataRef newFunnels
                                yield theLog

