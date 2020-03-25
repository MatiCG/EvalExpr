module BasicTestsSpec where

import EvalExpr
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Tests for basic functions like isOperator" $ do

    describe "isOperator tests" $ do
        it "check + operator" $ do
            isOperator('+') `shouldBe` True
        it "check - operator" $ do
            isOperator('-') `shouldBe` True
        it "check / operator" $ do
            isOperator('/') `shouldBe` True
        it "check * operator" $ do
            isOperator('*') `shouldBe` True
        it "check wrong operator" $ do
            isOperator('7') `shouldBe` False
        it "check if parenthesis is operator" $ do
            isOperator('(') `shouldBe` False

    describe "getNumber" $ do
        it "getNumber 12*12" $ do
            getNumber("12*12") `shouldBe` Nothing
        it "getNumber 240" $ do
            getNumber("240") `shouldBe` Just 240
        it "getNumber 12.14" $ do
            getNumber("12.14") `shouldBe` Just 12.14
        it "getNumber 240a" $ do
            getNumber("240a") `shouldBe` Nothing