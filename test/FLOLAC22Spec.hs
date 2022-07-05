module FLOLAC22Spec where

import FLOLAC22
import Test.Hspec
import Control.Exception (evaluate)

penultimateSpec :: Spec
penultimateSpec =
  describe "penultimate" $ do
    context "penultimate [1,2,3,4]" $
      it "should be 3" $
        penultimate [1,2,3,4] `shouldBe` 3
    context "penultimate ['a'..'z']" $
      it "should be 'y'" $
        penultimate ['a'..'z'] `shouldBe` 'y'
    context "penultimate [1]" $
      it "throw an exception if list not long enough" $
        evaluate (penultimate [1]) `shouldThrow` errorCall "Prelude.last: empty list"

antepenultimateSpec :: Spec
antepenultimateSpec =
  describe "antepenultimate" $ do
    context "antepenultimate [1,2,3,4]" $
      it "should be 2" $
        antepenultimate [1,2,3,4] `shouldBe` 2
    context "antepenultimate ['a'..'z']" $
      it "should be 'x'" $
        antepenultimate ['a'..'z'] `shouldBe` 'x'
    context "antepenultimate [1]" $
      it "throw an exception if list not long enough" $
        evaluate (antepenultimate [1]) `shouldThrow` errorCall "Prelude.init: empty list"
    context "antepenultimate [1,2]" $
      it "throw an exception if list not long enough" $
        evaluate (antepenultimate [1,2]) `shouldThrow` errorCall "Prelude.last: empty list"

spec :: Spec
spec = do
  describe "penultimate" penultimateSpec
  describe "antepenultimate" antepenultimateSpec

main :: IO()
main = hspec spec
