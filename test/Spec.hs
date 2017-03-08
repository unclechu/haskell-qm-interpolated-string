{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import "hspec" Test.Hspec (hspec, describe, it, shouldBe)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm)


main :: IO ()
main = hspec $ do

  describe "Examples from README" $ do

    it "First (decorative spacing and escaping space symbol)" $
      [qm|   hello world,
           \ what's going on here?  |]
        `shouldBe` "hello world, what's going on here?"

    it "Second (merging lines without spaces)" $
      [qm|
            it's actual
            ly ignored
         |]
            `shouldBe` "it's actually ignored"

    it "Third (explicit line-breaks symbols)" $
      [qm|  \  You could explicitly escape indentation or\n
               line-breaks when you really need it!  \
         |] `shouldBe` "  You could explicitly escape indentation or\n\
                       \line-breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qm| {1+2} \{3+4} |] `shouldBe` "3 {3+4}"

  it "Type annotation in interpolation block" $
    [qm|{10 :: Float}|] `shouldBe` "10.0"

  it "Escaping interpolation symbols inside interpolation block" $ do
    [qm|foo {"b{a{r"} baz|] `shouldBe` "foo b{a{r baz"
    [qm|foo {"b\}a\}r"} baz|] `shouldBe` "foo b}a}r baz"
