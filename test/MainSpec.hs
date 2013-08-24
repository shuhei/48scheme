module MainSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "True" $ do
    it "is truthy" $
      True `shouldBe` True
