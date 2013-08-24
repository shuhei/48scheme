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
      "(+ 1 2)"  `shouldEval` "3"
      "(- 5 3)"  `shouldEval` "2"
      "(* 3 4)"  `shouldEval` "12"
      "(/ 12 3)" `shouldEval` "4"

      "(+ 2 2)"             `shouldEval` "4"
      "(+ 2 (- 4 1))"       `shouldEval` "5"
      "(- (+ 4 6 3) 3 5 2)" `shouldEval` "3"

    describe "error checking" $ do
      "(+ 2 \"two\")" `shouldEval` "Invalid type: expected number, found \"two\""
      "(+ 2)"         `shouldEval` "Expected 2 args; found values 2"
      "(what? 2)"     `shouldEval` "Getting a unbound variable: what?"

    describe "binary operators" $ do
      "(< 2 3)"  `shouldEval` "#t"
      "(> 2 3)"  `shouldEval` "#f"
      "(<= 3 3)" `shouldEval` "#t"

      "(string=? \"test\" \"test\")" `shouldEval` "#t"
      "(string<? \"abc\" \"bba\")"   `shouldEval` "#t"

    describe "if clause" $ do
      "(if (> 2 3) \"no\" \"yes\")"              `shouldEval` "\"yes\""
      "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")" `shouldEval` "9"

    describe "list primitives" $ do
      describe "car" $ do
        "(car '(a b c))"   `shouldEval` "a"
        "(car '(a))"       `shouldEval` "a"
        "(car '(a b . c))" `shouldEval` "a"
        "(car 'a)"         `shouldEval` "Invalid type: expected pair, found a"
        "(car 'a 'b)"      `shouldEval` "Expected 1 args; found values a b"

      describe "cdr" $ do
        "(cdr '(a b c))"   `shouldEval` "(b c)"
        "(cdr '(a b))"     `shouldEval` "(b)"
        "(cdr '(a))"       `shouldEval` "()"
        "(cdr '(a . b))"   `shouldEval` "b"
        "(cdr '(a b . c))" `shouldEval` "(b . c)"
        "(cdr 'a)"         `shouldEval` "Invalid type: expected pair, found a"
        "(cdr 'a 'b)"      `shouldEval` "Expected 1 args; found values a b"

      describe "cons" $ do
        "(cons 'this '(is test))"     `shouldEval` "(this is test)"
        "(cons 'this '(is a . test))" `shouldEval` "(this is a . test)"
        "(cons '(this is) 'test)"     `shouldEval` "((this is) . test)"
        "(cons '(this is) '())"       `shouldEval` "((this is))"
        "(cons 'a)"                   `shouldEval` "Expected 2 args; found values a"
