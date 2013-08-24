module SchemeSpec where

import Test.Hspec
import Scheme

evalWithPrimitives :: String -> IO String
evalWithPrimitives expr = do
  env <- primitiveBindings
  evalString env expr

shouldEval :: String -> String -> Spec
shouldEval expr expected = do
  it expr $
    evalWithPrimitives expr `shouldReturn` expected

spec :: Spec
spec = do
  describe "True" $ do
    it "is truthy" $
      True `shouldBe` True

  describe "evalString" $ do
    describe "arithmetic operations" $ do
      "(+ 1 2)" `shouldEval` "3"
      "(- 5 3)" `shouldEval` "2"
      "(* 3 4)" `shouldEval` "12"
      "(/ 12 3)" `shouldEval` "4"

      "(+ 2 2)" `shouldEval` "4"
      "(+ 2 (- 4 1))" `shouldEval` "5"
      "(- (+ 4 6 3) 3 5 2)" `shouldEval` "3"

    describe "error checking" $ do
      "(+ 2 \"two\")" `shouldEval` "Invalid type: expected number, found \"two\""
