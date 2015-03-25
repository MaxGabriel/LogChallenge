module LogChallenge.FunnelsSpec (spec) where

import ClassyPrelude
import Test.Hspec

import LogChallenge.ExampleData

import LogChallenge.Funnels
import LogChallenge.Parsing

import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Funnels" $ do
        it "should match for the front page funnel step" $ do
            (matchesLog frontPageFunnelStep $ frontPageLog) `shouldBe` True
        it "should update the FunnelData for the publisher front page funnel" $ do
            let funnelData  = initFunnelData publisherFrontPageFunnel
                funnelData' = updateFunnelWithLog frontPageLog funnelData
            userToStep funnelData' `shouldBe` Map.singleton (ipAddress frontPageLog) 1