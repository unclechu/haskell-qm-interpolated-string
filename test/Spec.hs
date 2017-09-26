{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import "hspec" Test.Hspec (hspec, describe)

-- local imports
import qualified QM.Spec
import qualified QN.Spec
import qualified QY.Spec
-- import qualified QV.Spec


main :: IO ()
main = hspec $ do

  describe "QM" QM.Spec.spec
  describe "QN (QM but without interpolation)" QN.Spec.spec
  describe "QY (interpolated string with line-breaks)" QY.Spec.spec
  -- describe "QV (QY but without interpolation)" QV.Spec.spec

  {-
  describe "QK (interpolated string with line-breaks replaced with spaces)" $ do

    it "Works as expected" $ do
      [qk|
        foo
        {1+2}
        bar
      |] `shouldBe` "foo 3 bar"

  describe "QL (QK but without interpolation)" $ do

    it "Works as expected" $ do
      [ql|
        foo
        {1+2}
        bar
      |] `shouldBe` "foo {1+2} bar"
  -}
