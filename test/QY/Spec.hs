{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QY.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qy)


spec :: Spec
spec = do

  it "Works as expected" $
    [qy|
      foo
      {1+2}
      bar
    |] `shouldBe` "foo\n3\nbar"

  it "Explicitly slicing line-breaks" $
    [qy|
      foo\
      {1+2}\
      bar\
      baz
    |] `shouldBe` "foo3barbaz"

  describe "Examples from README" $ do

    it "First (decorative spacing and escaping space symbol)" $
      [qy|   hello world,
           \ what's going on here?  |]
        `shouldBe` "hello world,\n what's going on here?"

    it "Second (merging lines)" $
      [qy|
            it's actual
            ly NOT ignored
         |]
            `shouldBe` "it's actual\nly NOT ignored"

    it "Third (explicit line-breaks symbols)" $
      [qy|  \  You could explicitly escape indentation or\n
               line-breaks when you really need it!  \
         |] `shouldBe` "  You could explicitly escape indentation or\n\n\
                       \line-breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qy| {1+2} \{3+4} |] `shouldBe` "3 {3+4}"

  it "Type annotation in interpolation block" $
    [qy|{10 :: Float}|] `shouldBe` "10.0"

  it "Escaping interpolation symbols inside interpolation block" $ do
    [qy|foo {"b{a{r"} baz|]   `shouldBe` "foo b{a{r baz"
    [qy|foo {"b\}a\}r"} baz|] `shouldBe` "foo b}a}r baz"

  it "Example from generated docs" $ [qy| foo {'b':'a':'r':""}
                                        \ baz |] `shouldBe` "foo\n bar baz"

  it "Escaping backslashes" $ do [qy| foo\bar |]    `shouldBe` "foo\\bar"
                                 [qy| foo\\bar |]   `shouldBe` "foo\\bar"
                                 [qy| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                 [qy| foo\\\\bar |] `shouldBe` "foo\\\\bar"

  it "Empty string" $ [qy|  |] `shouldBe` ""

  it "Escaping space by slash at EOL after space (line-break is sliced)" $
    [qy| foo \
         bar |] `shouldBe` "foo bar"

  it "Escaped spaces at the edges" $ do [qy| foo\ |] `shouldBe` "foo "
                                        [qy|\ foo |] `shouldBe` " foo"

  describe "Tabs as indentation" $ do

    it "Tabs is only indentation at left side" $ do
			[qy|
				foo  bar  baz
			|] `shouldBe` "foo  bar  baz"

			[qy|			foo bar baz|] `shouldBe` "foo bar baz"

    it "Tabs also at EOL" $ do
			[qy|
				foo  bar  baz				
			|] `shouldBe` "foo  bar  baz"

			[qy|			foo bar baz				|] `shouldBe` "foo bar baz"

    it "Escaped tabs" $ do
      [qy|		\tfoo|]    `shouldBe` "\tfoo"
      [qy|		\	foo	|]   `shouldBe` "\tfoo"
      [qy|	foo		\	|]   `shouldBe` "foo\t\t\t"
      [qy|	foo	\		|]   `shouldBe` "foo\t\t"
      [qy|	foo\			|] `shouldBe` "foo\t"
