module Main where

import Core qualified

import Test.Hspec (describe, hspec, it, runIO, shouldBe, beforeAll)
main :: IO ()
main = hspec $ do
  describe "Spook" $ do
    it "should work as expected" $ do
      "1" `shouldBe` "1"
