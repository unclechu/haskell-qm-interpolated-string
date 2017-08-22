{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QM.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qm)


spec :: Spec
spec = do

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
    [qm|foo {"b{a{r"} baz|]   `shouldBe` "foo b{a{r baz"
    [qm|foo {"b\}a\}r"} baz|] `shouldBe` "foo b}a}r baz"

  it "Example from generated docs" $ [qm| foo {'b':'a':'r':""}
                                        \ baz |] `shouldBe` "foo bar baz"

  it "Escaping backslashes" $ do [qm| foo\bar |]    `shouldBe` "foo\\bar"
                                 [qm| foo\\bar |]   `shouldBe` "foo\\bar"
                                 [qm| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                 [qm| foo\\\\bar |] `shouldBe` "foo\\\\bar"

  it "Empty string" $ [qm|  |] `shouldBe` ""

  it "Escaping space by slash at EOL after space" $
    [qm| foo \
         bar |] `shouldBe` "foo bar"

  it "Escaped spaces at the edges" $ do [qm| foo\ |] `shouldBe` "foo "
                                        [qm|\ foo |] `shouldBe` " foo"

  describe "Tabs as indentation" $ do

    it "Tabs is only indentation at left side" $ do
			[qm|
				foo  bar  baz
			|] `shouldBe` "foo  bar  baz"

			[qm|			foo bar baz|] `shouldBe` "foo bar baz"

    it "Tabs also at EOL" $ do
			[qm|
				foo  bar  baz				
			|] `shouldBe` "foo  bar  baz"

			[qm|			foo bar baz				|] `shouldBe` "foo bar baz"

    it "Escaped tabs" $ do
      [qm|		\tfoo|]    `shouldBe` "\tfoo"
      [qm|		\	foo	|]   `shouldBe` "\tfoo"
      [qm|	foo		\	|]   `shouldBe` "foo\t\t\t"
      [qm|	foo	\		|]   `shouldBe` "foo\t\t"
      [qm|	foo\			|] `shouldBe` "foo\t"
