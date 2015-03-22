module LogChallenge.CountersSpec (spec) where

import ClassyPrelude
import Test.Hspec

import LogChallenge.ExampleData

import LogChallenge.Counters
import LogChallenge.Parsing
-- import Data.Attoparsec.Text


frontPageLog :: LogSuccess
frontPageLog = parsedLog1 { controller = "HomeController" }

spec :: Spec
spec = do
    describe "Counters" $ do
        it "should match for the front page counter" $ do
            (matchesLog frontPageCounter $ frontPageLog) `shouldBe` True
        it "shouldn't match for the payments counter" $ do
            (matchesLog paymentsPageCounter $ frontPageLog) `shouldBe` False