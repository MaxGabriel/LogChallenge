{-# LANGUAGE ViewPatterns #-}

module LogChallenge.ChallengeMain (runChallenge) where

import ClassyPrelude

import qualified Data.Text as T

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Data.Attoparsec.Text

import LogChallenge.Parsing
import LogChallenge.Counters
import LogChallenge.Funnels

import Safe
import Control.DeepSeq

runChallenge :: IO ()
runChallenge = do
    let countersData = map initCounterData allCounters
        funnelsData  = map initFunnelData allFunnels

    countersRef <- newIORef countersData
    funnelsRef  <- newIORef funnelsData

    runResourceT
         $ CC.sourceFile "heyzap-example-log"
        $$ splitUnbounded "\n\n\n"
        -- Parse text into records
        =$ parseLogs
        =$ filterLogSuccess
        -- Analyze parsed logs
        =$ runCounters countersRef
        =$ runFunnels funnelsRef
        =$ CC.sinkNull
        
    updatedCounters <- readIORef countersRef
    updatedFunnels  <- readIORef funnelsRef
    mapM_ (print . showCounterData) updatedCounters
    mapM_ (print . showFunnelData) updatedFunnels

splitUnbounded :: Monad m => Text -> Conduit Text m Text
splitUnbounded splitText = awaitText T.empty
    where
      awaitText buf = await >>= maybe (finish buf) (process buf)

      finish buf = unless (T.null buf) (yield buf)

      process buf text = yieldSplits $ buf `T.append` text

      yieldSplits text = do
        let splits = T.splitOn splitText text
            lastSplit = lastDef T.empty splits
        mapM_ yield (initSafe splits)
        awaitText lastSplit

parseLogs :: MonadResource m => Conduit Text m (Either String Log)
parseLogs = CC.map (parseOnly parseLog2)


filterLogSuccess :: MonadResource m => Conduit (Either String Log) m LogSuccess
filterLogSuccess = awaitForever handleLog
                   where handleLog (Right (Success x)) = yield x
                         handleLog _ = filterLogSuccess

runCounters :: (MonadResource m, MonadIO m) => IORef [CounterData] -> Conduit LogSuccess m LogSuccess
runCounters counterDataRef = awaitForever $ \theLog -> do
                                liftIO $ modifyIORef' counterDataRef (map (updateWithLog theLog))
                                unused <- liftIO $ readIORef counterDataRef
                                unused `deepseq` yield theLog

runFunnels :: (MonadResource m, MonadIO m) => IORef [FunnelData] -> Conduit LogSuccess m LogSuccess
runFunnels funnelDataRef = awaitForever $ \theLog -> do
                                liftIO $ modifyIORef' funnelDataRef (map (updateFunnelWithLog theLog))
                                unused <- liftIO $ readIORef funnelDataRef
                                unused `deepseq` yield theLog

