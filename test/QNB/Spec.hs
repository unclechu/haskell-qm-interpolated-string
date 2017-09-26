{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QNB.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qnb)


spec :: Spec
spec = do

  it "Works as expected" $
    [qnb|
      foo
      {1+2}
      bar
    |] `shouldBe` "foo\n{1+2}\nbar"

  it "Explicitly slicing line-breaks" $ do
    [qnb|
      foo\
      {1+2}\
      bar\
      baz
    |] `shouldBe` "foo{1+2}barbaz"
    [qnb|
      foo\
      {1+2}\
      bar\
      baz\
    |] `shouldBe` "foo{1+2}barbaz"

  describe "Examples from README" $ do

    it "First (decorative spacing and escaping space symbol)" $
      [qnb|   hello world,
            \ what's going on here?  |]
        `shouldBe` "hello world,\n what's going on here?"

    it "Second (breaking lines)" $
      [qnb|
            it's actual
            ly NOT ignored
         |]
            `shouldBe` "it's actual\nly NOT ignored"

    it "Third (explicit line-breaks symbols, line-break plus space)" $
      [qnb|  \  You could explicitly escape indentation or\n
                line-breaks when you really need it!  \
          |] `shouldBe` "  You could explicitly escape indentation or\n\n\
                        \line-breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qnb| {1+2} \{3+4} |] `shouldBe` "{1+2} \\{3+4}"

    it "Example of `qnb` QuasiQuoter" $
      [qnb| foo {1+2} |] `shouldBe` "foo {1+2}"

  it "Type annotation in interpolation block" $
    [qnb|{10 :: Float}|] `shouldBe` "{10 :: Float}"

  it "Escaping interpolation symbols inside interpolation block" $ do
    [qnb|foo {"b{a{r"} baz|] `shouldBe` "foo {\"b{a{r\"} baz"
    [qnb|foo {"b\}a\}r"} baz|] `shouldBe` "foo {\"b\\}a\\}r\"} baz"

  it "Example from generated docs (double-space)" $
    [qnb| foo {'b':'a':'r':""}
        \ baz |] `shouldBe` "foo {'b':'a':'r':\"\"}\n baz"

  it "Escaping backslashes" $ do [qnb| foo\bar |]    `shouldBe` "foo\\bar"
                                 [qnb| foo\\bar |]   `shouldBe` "foo\\bar"
                                 [qnb| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                 [qnb| foo\\\\bar |] `shouldBe` "foo\\\\bar"

  it "Empty string" $ [qnb|  |] `shouldBe` ""

  it "Escaping space by slash at EOL after space (line-break is sliced)" $
    [qnb| foo \
          bar |] `shouldBe` "foo bar"

  it "Escaped spaces at the edges" $ do [qnb| foo\ |] `shouldBe` "foo "
                                        [qnb|\ foo |] `shouldBe` " foo"

  describe "Tabs as indentation" $ do

    it "Tabs is only indentation at left side" $ do
			[qnb|
				foo  bar  baz
			|] `shouldBe` "foo  bar  baz"

			[qnb|			foo bar baz|] `shouldBe` "foo bar baz"

    it "Tabs also at EOL" $ do
			[qnb|
				foo  bar  baz				
			|] `shouldBe` "foo  bar  baz"

			[qnb|			foo bar baz				|] `shouldBe` "foo bar baz"

    it "Escaped tabs" $ do
      [qnb|		\tfoo|]    `shouldBe` "\tfoo"
      [qnb|		\	foo	|]   `shouldBe` "\tfoo"
      [qnb|	foo		\	|]   `shouldBe` "foo\t\t\t"
      [qnb|	foo	\		|]   `shouldBe` "foo\t\t"
      [qnb|	foo\			|] `shouldBe` "foo\t"

  it "Tails" $ do
    [qnb|    
           foo   
                 |] `shouldBe` "foo"
    [qnb|	 
         	
          foo	 
               
             	
                 |] `shouldBe` "foo"
    [qnb|				
            foo			
            				
            				|] `shouldBe` "foo"
