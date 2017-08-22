{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QV.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qv)


spec :: Spec
spec = do

  it "Works as expected" $ do
    [qv|
      foo
      {1+2}
      bar
    |] `shouldBe` "foo\n{1+2}\nbar"

  it "Slicing line break" $
    [qv|
      foo\
      {1+2}\
      bar\
      baz
    |] `shouldBe` "foo{1+2}barbaz"

  describe "Examples from README" $ do

    it "First (decorative spacing and escaping space symbol, double-space)" $
      [qv|   hello world,
           \ what's going on here?  |]
        `shouldBe` "hello world,  what's going on here?"

    it "Second (merging lines with spaces)" $
      [qv|
            it's actual
            ly NOT ignored
         |]
            `shouldBe` "it's actual ly ignored"

    it "Third (explicit line-breaks symbols, line-break plus space)" $
      [qv|  \  You could explicitly escape indentation or\n
               line-breaks when you really need it!  \
         |] `shouldBe` "  You could explicitly escape indentation or\n \
                       \line-breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qv| {1+2} \{3+4} |] `shouldBe` "{1+2} \\{3+4}"

    it "Example of `qv` QuasiQuoter" $
      [qv| foo {1+2} |] `shouldBe` "foo {1+2}"

  it "Type annotation in interpolation block" $
    [qv|{10 :: Float}|] `shouldBe` "{10 :: Float}"

  it "Escaping interpolation symbols inside interpolation block" $ do
    [qv|foo {"b{a{r"} baz|] `shouldBe` "foo {\"b{a{r\"} baz"
    [qv|foo {"b\}a\}r"} baz|] `shouldBe` "foo {\"b\\}a\\}r\"} baz"

  it "Example from generated docs (double-space)" $
    [qv| foo {'b':'a':'r':""}
       \ baz |] `shouldBe` "foo  {'b':'a':'r':\"\"} baz"

  it "Escaping backslashes" $ do [qv| foo\bar |]    `shouldBe` "foo\\bar"
                                 [qv| foo\\bar |]   `shouldBe` "foo\\bar"
                                 [qv| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                 [qv| foo\\\\bar |] `shouldBe` "foo\\\\bar"

  it "Empty string" $ [qv|  |] `shouldBe` ""

  it "Escaping space by slash at EOL after space (double-space)" $
    [qv| foo \
         bar |] `shouldBe` "foo  bar"

  it "Escaped spaces at the edges" $ do [qv| foo\ |] `shouldBe` "foo "
                                        [qv|\ foo |] `shouldBe` " foo"

  describe "Tabs as indentation" $ do

    it "Tabs is only indentation at left side" $ do
			[qv|
				foo  bar  baz
			|] `shouldBe` "foo  bar  baz"

			[qv|			foo bar baz|] `shouldBe` "foo bar baz"

    it "Tabs also at EOL" $ do
			[qv|
				foo  bar  baz				
			|] `shouldBe` "foo  bar  baz"

			[qv|			foo bar baz				|] `shouldBe` "foo bar baz"

    it "Escaped tabs" $ do
      [qv|		\tfoo|]    `shouldBe` "\tfoo"
      [qv|		\	foo	|]   `shouldBe` "\tfoo"
      [qv|	foo		\	|]   `shouldBe` "foo\t\t\t"
      [qv|	foo	\		|]   `shouldBe` "foo\t\t"
      [qv|	foo\			|] `shouldBe` "foo\t"
