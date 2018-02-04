-- WARNING! Do not remove trailing whitespace or tabs, it's part of the test!
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QN.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qn)


spec :: Spec
spec = do

  describe "Examples from README" $ do

    it "First (decorative spacing and escaping space symbol)" $
      [qn|   hello world,
           \ what's going on here?  |]
        `shouldBe` "hello world, what's going on here?"

    it "Second (merging lines without spaces)" $
      [qn|
            it's actual
            ly ignored
         |]
            `shouldBe` "it's actually ignored"

    it "Third (explicit line breaks symbols)" $
      [qn|  \  You could explicitly escape indentation or\n
               line breaks when you really need it!  \
         |] `shouldBe` "  You could explicitly escape indentation or\n\
                       \line breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qn| {1+2} \{3+4} |] `shouldBe` "{1+2} \\{3+4}"

  it "Empty string" $ [qn|  |] `shouldBe` ""

  it "Type annotation in interpolation block" $
    [qn|{10 :: Float}|] `shouldBe` "{10 :: Float}"

  describe "Escaping" $ do

    it "Escaping interpolation symbols inside interpolation block" $ do
      [qn|foo {"b{a{r"} baz|] `shouldBe` "foo {\"b{a{r\"} baz"
      [qn|foo {"b\}a\}r"} baz|] `shouldBe` "foo {\"b\\}a\\}r\"} baz"

    it "Escaping backslashes" $ do [qn| foo\bar |]    `shouldBe` "foo\\bar"
                                   [qn| foo\\bar |]   `shouldBe` "foo\\bar"
                                   [qn| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                   [qn| foo\\\\bar |] `shouldBe` "foo\\\\bar"

    it "Escaping space by slash at EOL after space" $
      [qn| foo \
           bar |] `shouldBe` "foo bar"

    it "Escaped spaces at the edges" $ do [qn| foo\ |] `shouldBe` "foo "
                                          [qn|\ foo |] `shouldBe` " foo"

    it "Escaping backslash itself when it makes sense" $ do
      [qn| foo\nbar   |] `shouldBe` "foo\nbar"
      [qn| foo\\nbar  |] `shouldBe` "foo\\nbar"
      [qn| foo\tbar   |] `shouldBe` "foo\tbar"
      [qn| foo\\tbar  |] `shouldBe` "foo\\tbar"
      [qn| foo\	bar   |] `shouldBe` "foo\tbar"
      [qn| foo\\	bar |] `shouldBe` "foo\\\tbar"
      [qn| foo\ bar   |] `shouldBe` "foo bar"
      [qn| foo\\ bar  |] `shouldBe` "foo\\ bar"

      [qn| foo\
           bar  |] `shouldBe` "foobar"
      [qn| foo\\
           bar  |] `shouldBe` "foo\\bar"

    -- By 'not really' it means we just shows here what happens with examples
    -- for QM when we use them with QN quoter.
    describe "Escaping inside interpolation blocks (not really!)" $ do

      it "Line-breaks must be interpreted as just haskell code\
         \ (not really!)" $ do
        [qn| {'\n'}          |] `shouldBe` "{'\n'}"
        [qn| {"foo\nbar"}    |] `shouldBe` "{\"foo\nbar\"}"
        [qn| {"foo\\nbar"}   |] `shouldBe` "{\"foo\\nbar\"}"
        [qn| {"foo\\\nbar"}  |] `shouldBe` "{\"foo\\\nbar\"}"
        [qn| {"foo\\\\nbar"} |] `shouldBe` "{\"foo\\\\nbar\"}"

      it "Escaping for multiline strings" $ do
        [qn| {"foo\
              \bar\
              \baz"} |] `shouldBe` "{\"foo\\bar\\baz\"}"
        [qn| {"foo \
              \bar\
              \ baz"} |] `shouldBe` "{\"foo \\bar baz\"}"
        [qn| x  \
             {"foo \
             \bar\
             \ baz"}  \
             y |] `shouldBe` "x  {\"foo \\bar baz\"}  y"

      it "Escaping of close-bracket '}'" $ do
        [qn| { testFoo {testBar = 9000\} } |]
          `shouldBe` "{ testFoo {testBar = 9000\\} }"
        [qn| foo{ testFoo {testBar = 9000\} }bar |]
          `shouldBe` "foo{ testFoo {testBar = 9000\\} }bar"
        [qn|foo{testFoo2 {test1 = (test1 testFoo2) {testBar = 9000\}\}}bar|]
          `shouldBe`
            "foo{testFoo2 {test1 = (test1 testFoo2) {testBar = 9000\\}\\}}bar"
        [qn| foo { testFoo2 {
               test1 = (test1 testFoo2)
                       { testBar = 9000 \}
                           \}
                 } bar |]
          `shouldBe` "foo { testFoo2 {test1 = (test1 testFoo2)\
                     \{ testBar = 9000 \\}\\}} bar"
        [qn| {"\}"}   |] `shouldBe` "{\"\\}\"}"
        [qn| {"\\\}"} |] `shouldBe` "{\"\\\\}\"}"
        [qn| {123}}   |] `shouldBe` "{123}}"

      it "When interpolation block is escaped\
         \ everything must be interpreted as usual (not really)" $ do
        [qn| \{ foo\nbar\\baz\} } |] `shouldBe` "\\{ foo\nbar\\baz\\} }"
        [qn| \{ foo\
                bar } |] `shouldBe` "\\{ foobar }"

      it "Tabs characters in string inside interpolation block\
         \ (not really!)" $ do
        [qn| {"foo\t\tbar" } |] `shouldBe` "{\"foo\t\tbar\" }"
        [qn| {"foo		bar" } |] `shouldBe` "{\"foo\t\tbar\" }"
        [qn| {"foo\\t\tbar"} |] `shouldBe` "{\"foo\\t\tbar\"}"

  describe "Tabs as indentation" $ do

    it "Tabs is only indentation at left side" $ do
			[qn|
				foo  bar  baz
			|] `shouldBe` "foo  bar  baz"

			[qn|			foo bar baz|] `shouldBe` "foo bar baz"

    it "Tabs also at EOL" $ do
			[qn|
				foo  bar  baz				
			|] `shouldBe` "foo  bar  baz"

			[qn|			foo bar baz				|] `shouldBe` "foo bar baz"

    it "Escaped tabs" $ do
      [qn|		\tfoo|]    `shouldBe` "\tfoo"
      [qn|		\	foo	|]   `shouldBe` "\tfoo"
      [qn|	foo		\	|]   `shouldBe` "foo\t\t\t"
      [qn|	foo	\		|]   `shouldBe` "foo\t\t"
      [qn|	foo\			|] `shouldBe` "foo\t"

  it "Tails" $ do
    [qn|    
           foo   
                 |] `shouldBe` "foo"
    [qn|	 
         	
          foo	 
               
             	
                 |] `shouldBe` "foo"
    [qn|				
            foo			
            				
            				|] `shouldBe` "foo"

  describe "New README examples" $ do

    it "First example" $
      [qn| Hello,
           world!
           Pi is {floor pi}.{floor $ (pi - 3) * 100}… |]
        `shouldBe` "Hello,world!Pi is {floor pi}.{floor $ (pi - 3) * 100}…"

    it "Simple usage example" $
      [qn|
        <article>
          <h1>{title}</h1>
          <p>{text}</p>
        </article>
      |] `shouldBe` "<article><h1>{title}</h1><p>{text}</p></article>"

    it "Can escape spaces" $
      [qn|   you can escape spaces
           \ when you need them    |]
        `shouldBe`
          "you can escape spaces when you need them"

    it "Indentation and line breaks are ignored" $
      [qn|
              indentation and li
        ne bre
         aks are i
             gno
           red
      |]
      `shouldBe` "indentation and line breaks are ignored"

    it "Escaping indentation or line breaks" $
      [qn|  \  You can escape indentation or\n
               line breaks when you need them! \  |]
        `shouldBe`
          "  You can escape indentation or\nline breaks when you need them!  "

    it "Interpolation blocks can be escaped too" $
      [qn| Interpolation blocks can be escaped too: {1+2} \{3+4} |]
        `shouldBe`
          "Interpolation blocks can be escaped too: {1+2} \\{3+4}"

    it "Interpolation" $ [qn| foo {1+2} |] `shouldBe` "foo {1+2}"

  it "Haddock example" $ do
    [qn| foo {'b':'a':'r':""}
       \ baz |] `shouldBe` "foo {'b':'a':'r':\"\"} baz"
    [qn| foo
       \ {'b':'a':'r':""}
       \ baz |] `shouldBe` "foo {'b':'a':'r':\"\"} baz"
    [qn| foo {'b':'a':'r':""}
         baz |] `shouldBe` "foo {'b':'a':'r':\"\"}baz"
