{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QMS.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qms)


spec :: Spec
spec = do

  it "Works as expected" $
    [qms|
      foo
      {1+2}
      bar
    |] `shouldBe` "foo 3 bar"

  it "Explicitly slicing line-breaks" $ do
    [qms|
      foo\
      {1+2}\
      bar\
      baz
    |] `shouldBe` "foo3barbaz"
    [qms|
      foo\
      {1+2}\
      bar\
      baz\
    |] `shouldBe` "foo3barbaz"

  describe "Examples from README" $ do

    it "First (decorative spacing and escaping space symbol)" $
      [qms|   hello world,
            \ what's going on here?  |]
        `shouldBe` "hello world,  what's going on here?"

    it "Second (breaking lines)" $
      [qms|
            it's actual
            ly NOT ignored
         |]
            `shouldBe` "it's actual ly NOT ignored"

    it "Third (explicit line-breaks symbols)" $
      [qms|  \  You could explicitly escape indentation or\n
                line-breaks when you really need it!  \
          |] `shouldBe` "  You could explicitly escape indentation or\n \
                        \line-breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qms| {1+2} \{3+4} |] `shouldBe` "3 {3+4}"

  it "Type annotation in interpolation block" $
    [qms|{10 :: Float}|] `shouldBe` "10.0"

  it "Escaping interpolation symbols inside interpolation block" $ do
    [qms|foo {"b{a{r"} baz|]   `shouldBe` "foo b{a{r baz"
    [qms|foo {"b\}a\}r"} baz|] `shouldBe` "foo b}a}r baz"

  it "Example from generated docs" $ [qms| foo {'b':'a':'r':""}
                                         \ baz |] `shouldBe` "foo bar  baz"

  it "Escaping backslashes" $ do [qms| foo\bar |]    `shouldBe` "foo\\bar"
                                 [qms| foo\\bar |]   `shouldBe` "foo\\bar"
                                 [qms| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                 [qms| foo\\\\bar |] `shouldBe` "foo\\\\bar"

  it "Empty string" $ [qms|  |] `shouldBe` ""

  it "Escaping space by slash at EOL after space (line-break is sliced)" $
    [qms| foo \
          bar |] `shouldBe` "foo bar"

  it "Escaped spaces at the edges" $ do [qms| foo\ |] `shouldBe` "foo "
                                        [qms|\ foo |] `shouldBe` " foo"

  describe "Tabs as indentation" $ do

    it "Tabs is only indentation at left side" $ do
			[qms|
				foo  bar  baz
			|] `shouldBe` "foo  bar  baz"

			[qms|			foo bar baz|] `shouldBe` "foo bar baz"

    it "Tabs also at EOL" $ do
			[qms|
				foo  bar  baz				
			|] `shouldBe` "foo  bar  baz"

			[qms|			foo bar baz				|] `shouldBe` "foo bar baz"

    it "Escaped tabs" $ do
      [qms|		\tfoo|]    `shouldBe` "\tfoo"
      [qms|		\	foo	|]   `shouldBe` "\tfoo"
      [qms|	foo		\	|]   `shouldBe` "foo\t\t\t"
      [qms|	foo	\		|]   `shouldBe` "foo\t\t"
      [qms|	foo\			|] `shouldBe` "foo\t"

  it "Tails" $ do
    [qms|    
           foo   
                 |] `shouldBe` "foo"
    [qms|	 
         	
          foo	 
               
             	
                 |] `shouldBe` "foo"
    [qms|				
            foo			
            				
            				|] `shouldBe` "foo"
