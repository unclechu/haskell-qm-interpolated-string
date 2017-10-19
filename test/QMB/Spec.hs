{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QMB.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qmb)


spec :: Spec
spec = do

  it "Works as expected" $
    [qmb|
      foo
      {1+2}
      bar
    |] `shouldBe` "foo\n3\nbar"

  it "Explicitly slicing line breaks" $ do
    [qmb|
      foo\
      {1+2}\
      bar\
      baz
    |] `shouldBe` "foo3barbaz"
    [qmb|
      foo\
      {1+2}\
      bar\
      baz\
    |] `shouldBe` "foo3barbaz"

  describe "Examples from README" $ do

    it "First (decorative spacing and escaping space symbol)" $
      [qmb|   hello world,
            \ what's going on here?  |]
        `shouldBe` "hello world,\n what's going on here?"

    it "Second (breaking lines)" $
      [qmb|
            it's actual
            ly NOT ignored
         |]
            `shouldBe` "it's actual\nly NOT ignored"

    it "Third (explicit line breaks symbols)" $
      [qmb|  \  You could explicitly escape indentation or\n
                line breaks when you really need it!  \
          |] `shouldBe` "  You could explicitly escape indentation or\n\n\
                        \line breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qmb| {1+2} \{3+4} |] `shouldBe` "3 {3+4}"

  it "Type annotation in interpolation block" $
    [qmb|{10 :: Float}|] `shouldBe` "10.0"

  it "Escaping interpolation symbols inside interpolation block" $ do
    [qmb|foo {"b{a{r"} baz|]   `shouldBe` "foo b{a{r baz"
    [qmb|foo {"b\}a\}r"} baz|] `shouldBe` "foo b}a}r baz"

  it "Example from generated docs" $ [qmb| foo {'b':'a':'r':""}
                                         \ baz |] `shouldBe` "foo bar\n baz"

  it "Escaping backslashes" $ do [qmb| foo\bar |]    `shouldBe` "foo\\bar"
                                 [qmb| foo\\bar |]   `shouldBe` "foo\\bar"
                                 [qmb| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                 [qmb| foo\\\\bar |] `shouldBe` "foo\\\\bar"

  it "Empty string" $ [qmb|  |] `shouldBe` ""

  it "Escaping space by slash at EOL after space (line break is sliced)" $
    [qmb| foo \
          bar |] `shouldBe` "foo bar"

  it "Escaped spaces at the edges" $ do [qmb| foo\ |] `shouldBe` "foo "
                                        [qmb|\ foo |] `shouldBe` " foo"

  describe "Tabs as indentation" $ do

    it "Tabs is only indentation at left side" $ do
			[qmb|
				foo  bar  baz
			|] `shouldBe` "foo  bar  baz"

			[qmb|			foo bar baz|] `shouldBe` "foo bar baz"

    it "Tabs also at EOL" $ do
			[qmb|
				foo  bar  baz				
			|] `shouldBe` "foo  bar  baz"

			[qmb|			foo bar baz				|] `shouldBe` "foo bar baz"

    it "Escaped tabs" $ do
      [qmb|		\tfoo|]    `shouldBe` "\tfoo"
      [qmb|		\	foo	|]   `shouldBe` "\tfoo"
      [qmb|	foo		\	|]   `shouldBe` "foo\t\t\t"
      [qmb|	foo	\		|]   `shouldBe` "foo\t\t"
      [qmb|	foo\			|] `shouldBe` "foo\t"

  it "Tails" $ do
    [qmb|    
           foo   
                 |] `shouldBe` "foo"
    [qmb|	 
         	
          foo	 
               
             	
                 |] `shouldBe` "foo"
    [qmb|				
            foo			
            				
            				|] `shouldBe` "foo"

  describe "New README examples" $ do

    it "Simple usage example" $ do
      let title = "Testing"
          text = "Some testing text"
      [qmb|
        <article>
          <h1>{title}</h1>
          <p>{text}</p>
        </article>
      |]
        `shouldBe`
          "<article>\n<h1>Testing</h1>\n<p>Some testing text</p>\n</article>"

    it "Interpolation" $ [qmb| foo {1+2} |] `shouldBe` "foo 3"

  it "Haddock example" $
    [qmb| foo
          {'b':'a':'r':""}
          baz |] `shouldBe` "foo\nbar\nbaz"
