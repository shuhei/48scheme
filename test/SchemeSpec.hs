module SchemeSpec where

import Test.Hspec
import Scheme

evalWithPrimitives :: String -> IO String
evalWithPrimitives expr = do
  env <- primitiveBindings
  evalString env expr

shouldEval :: String -> String -> Spec
shouldEval expr result = do
  it expr $
    evalWithPrimitives expr `shouldReturn` result

spec :: Spec
spec = do
  describe "True" $ do
    it "is truthy" $
      True `shouldBe` True

  describe "evalString" $ do
    "(+ 1 2)" `shouldEval` "3"
    "(- 5 3)" `shouldEval` "2"
    "(* 3 4)" `shouldEval` "12"
    "(/ 12 3)" `shouldEval` "4"
