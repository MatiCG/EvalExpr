module BasicTestsSpec where

import EvalExpr
import Operations
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
            isOperator('(') `shouldBe` True

    describe "getNumber Tests" $ do
        it "getNumber 12*12" $ do
            getNumber("12*12") `shouldBe` Nothing
        it "getNumber 240" $ do
            getNumber("240") `shouldBe` Just 240
        it "getNumber 12.14" $ do
            getNumber("12.14") `shouldBe` Just 12.14
        it "getNumber 240a" $ do
            getNumber("240a") `shouldBe` Nothing

    describe "evaluateToken Tests" $ do
        it "evaluate 12 * 12" $ do
            evaluateToken '*' "12" "12" `shouldBe` "144.0"
        it "evaluate 12 / 0" $ do
            evaluateToken '/' "12" "0" `shouldBe` "Error"
        it "evaluate 12 / 12" $ do
            evaluateToken '/' "12" "12" `shouldBe` "1.0"
        it "evaluate 1 + 5.35" $ do
            evaluateToken '+' "1" "5.35" `shouldBe` "6.35"
        it "evaluate 1 - 5.35" $ do
            evaluateToken '-' "1" "5.35" `shouldBe` "-4.35"    