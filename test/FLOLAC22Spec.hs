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

shiftLeftSpec :: Spec
shiftLeftSpec =
  describe "shiftLeft" $ do
    context "shiftLeft []" $
      it "should return empty list" $
        (shiftLeft [] :: [Int]) `shouldBe` []
    context "shiftLeft [1]" $
      it "should be [1]" $
        (shiftLeft [1]) `shouldBe` [1]
    context "shiftLeft [1,2,3]" $
      it "should be [2,3,1]" $
        shiftLeft [1,2,3] `shouldBe` [2,3,1]

rotateLeftSpec :: Spec
rotateLeftSpec =
  describe "rotateLeft" $ do
    context "rotateLeft -1 [1,2,3]" $
      it "throw an exception if n < 0" $
        evaluate (rotateLeft (-1) [1,2,3]) `shouldThrow` errorCall "Negative shift value"
    context "rotateLeft 0 [1,2,3]" $
      it "should be [1,2,3]" $
        rotateLeft 0 [1,2,3] `shouldBe` [1,2,3]
    context "rotateLeft 2 [1,2,3]" $
      it "should be [3,1,2]" $
        rotateLeft 2 [1,2,3] `shouldBe` [3,1,2]

insertElemSpec :: Spec
insertElemSpec =
  describe "insertElem" $ do
    context "insertElem 100 -1 []" $
      it "throw an exception if n < 0" $
        evaluate (insertElem 100 (-1) [1,2,3]) `shouldThrow` errorCall "Negative insert index"
    context "insertElem 100 0 []" $
      it "should be [100]" $
        insertElem 100 0 [] `shouldBe` [100]
    context "insertElem 100 3 [0,1,2,3,4]" $
      it "should be [0,1,2,100,3,4]" $
        insertElem 100 3 [0,1,2,3,4] `shouldBe` [0,1,2,100,3,4]
    context "insertElem 100 3 [0,1,2,3,4]" $
      it "should be [0,1,2,100,3,4]" $
        insertElem 100 3 [0,1,2,3,4] `shouldBe` [0,1,2,100,3,4]
    context "insertElem 100 10 [0,1,2,3,4]" $
      it "should be [0,1,2,3,4,100]" $
        insertElem 100 10 [0,1,2,3,4] `shouldBe` [0,1,2,3,4,100]

spec :: Spec
spec = do
  describe "penultimate" penultimateSpec
  describe "antepenultimate" antepenultimateSpec
  describe "shiftLeft" shiftLeftSpec
  describe "rotateLeft" rotateLeftSpec
  describe "insertElem" insertElemSpec

main :: IO()
main = hspec spec
