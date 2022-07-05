module FLOLAC22Spec where

import FLOLAC22
import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec
spec = describe "penultimate" $ do
        context "penultimate [1,2,3,4]" $
          it "should be 3" $
            penultimate [1,2,3,4] `shouldBe` 3
        context "penultimate ['a'..'z']" $
          it "should be 'y'" $
            penultimate ['a'..'z'] `shouldBe` 'y'
        context "penultimate [1]" $
          it "throw an exception if list not long enough" $
            evaluate (penultimate [1]) `shouldThrow` errorCall "Prelude.last: empty list"

main :: IO()
main = hspec spec
