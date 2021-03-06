-- WARNING! Do not remove trailing whitespace or tabs, it's part of the test!
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QNS.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qns)


spec :: Spec
spec = do

  it "Works as expected" $
    [qns|
      foo
      {1+2}
      bar
    |] `shouldBe` "foo {1+2} bar"

  it "Explicitly cutting off line breaks" $ do
    [qns|
      foo\
      {1+2}\
      bar\
      baz
    |] `shouldBe` "foo{1+2}barbaz"
    [qns|
      foo\
      {1+2}\
      bar\
      baz\
    |] `shouldBe` "foo{1+2}barbaz"

  describe "Examples from README" $ do

    it "First (decorative spacing and escaping space symbol)" $
      [qns|   hello world,
            \ what's going on here?  |]
        `shouldBe` "hello world,  what's going on here?"

    it "Second (breaking lines)" $
      [qns|
            it's actual
            ly NOT ignored
         |]
            `shouldBe` "it's actual ly NOT ignored"

    it "Third (explicit line breaks symbols, line break plus space)" $
      [qns|  \  You could explicitly escape indentation or\n
                line breaks when you really need it!  \
          |] `shouldBe` "  You could explicitly escape indentation or\n \
                        \line breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qns| {1+2} \{3+4} |] `shouldBe` "{1+2} \\{3+4}"

  it "Empty string" $ [qns|  |] `shouldBe` ""

  it "Type annotation in interpolation block" $
    [qns|{10 :: Float}|] `shouldBe` "{10 :: Float}"

  describe "Escaping" $ do

    it "Escaping interpolation symbols inside interpolation block" $ do
      [qns|foo {"b{a{r"} baz|] `shouldBe` "foo {\"b{a{r\"} baz"
      [qns|foo {"b\}a\}r"} baz|] `shouldBe` "foo {\"b\\}a\\}r\"} baz"

    it "Escaping backslashes" $ do [qns| foo\bar |]    `shouldBe` "foo\\bar"
                                   [qns| foo\\bar |]   `shouldBe` "foo\\bar"
                                   [qns| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                   [qns| foo\\\\bar |] `shouldBe` "foo\\\\bar"

    it "Escaping space by backslash at EOL after space\
       \ (line break is cutted off)" $
      [qns| foo \
            bar |] `shouldBe` "foo bar"

    it "Escaped spaces at the edges" $ do [qns| foo\ |] `shouldBe` "foo "
                                          [qns|\ foo |] `shouldBe` " foo"

    it "Escaping backslash itself when it makes sense" $ do
      [qns| foo\nbar  |] `shouldBe` "foo\nbar"
      [qns| foo\\nbar |] `shouldBe` "foo\\nbar"
      [qns| foo\tbar  |] `shouldBe` "foo\tbar"
      [qns| foo\\tbar |] `shouldBe` "foo\\tbar"
      [qns| foo\	bar |] `shouldBe` "foo\tbar"
      [qns| foo\\	bar |] `shouldBe` "foo\\\tbar"
      [qns| foo\ bar  |] `shouldBe` "foo bar"
      [qns| foo\\ bar |] `shouldBe` "foo\\ bar"

      [qns| foo\
            bar  |] `shouldBe` "foobar"
      [qns| foo\\
            bar  |] `shouldBe` "foo\\ bar"

    -- By 'not really' it means we just shows here what happens with examples
    -- for QMS when we use them with QNS quoter.
    describe "Escaping inside interpolation blocks (not really!)" $ do

      it "Line-breaks must be interpreted as just haskell code\
         \ (not really!)" $ do
        [qns| {'\n'}          |] `shouldBe` "{'\n'}"
        [qns| {"foo\nbar"}    |] `shouldBe` "{\"foo\nbar\"}"
        [qns| {"foo\\nbar"}   |] `shouldBe` "{\"foo\\nbar\"}"
        [qns| {"foo\\\nbar"}  |] `shouldBe` "{\"foo\\\nbar\"}"
        [qns| {"foo\\\\nbar"} |] `shouldBe` "{\"foo\\\\nbar\"}"

      it "Escaping for multiline strings" $ do
        [qns| {"foo\
               \bar\
               \baz"} |] `shouldBe` "{\"foo\\bar\\baz\"}"
        [qns| {"foo \
               \bar\
               \ baz"} |] `shouldBe` "{\"foo \\bar baz\"}"
        [qns| x  \
              {"foo \
              \bar\
              \ baz"}  \
              y |] `shouldBe` "x  {\"foo \\bar baz\"}  y"

      it "Escaping of close-bracket '}'" $ do
        [qns| { testFoo {testBar = 9000\} } |]
          `shouldBe` "{ testFoo {testBar = 9000\\} }"
        [qns| foo{ testFoo {testBar = 9000\} }bar |]
          `shouldBe` "foo{ testFoo {testBar = 9000\\} }bar"
        [qns|foo{testFoo2 {test1 = (test1 testFoo2) {testBar = 9000\}\}}bar|]
          `shouldBe`
            "foo{testFoo2 {test1 = (test1 testFoo2) {testBar = 9000\\}\\}}bar"
        [qns| foo { testFoo2 {
                test1 = (test1 testFoo2)
                        { testBar = 9000 \}
                            \}
                  } bar |]
          `shouldBe` "foo { testFoo2 { test1 = (test1 testFoo2)\
                     \ { testBar = 9000 \\} \\} } bar"
        [qns| {"\}"}   |] `shouldBe` "{\"\\}\"}"
        [qns| {"\\\}"} |] `shouldBe` "{\"\\\\}\"}"
        [qns| {123}}   |] `shouldBe` "{123}}"

      it "When interpolation block is escaped\
         \ everything must be interpreted as usual (not really)" $ do
        [qns| \{ foo\nbar\\baz\} } |] `shouldBe` "\\{ foo\nbar\\baz\\} }"
        [qns| \{ foo\
                 bar } |] `shouldBe` "\\{ foobar }"

      it "Tabs characters in string inside interpolation block\
         \ (not really)" $ do
        [qns| {"foo\t\tbar" } |] `shouldBe` "{\"foo\t\tbar\" }"
        [qns| {"foo		bar"  } |] `shouldBe` "{\"foo\t\tbar\"  }"
        [qns| {"foo\\t\tbar"} |] `shouldBe` "{\"foo\\t\tbar\"}"

  describe "Tabs as indentation" $ do

    it "Tabs is only indentation at left side" $ do
			[qns|
				foo  bar  baz
			|] `shouldBe` "foo  bar  baz"

			[qns|			foo bar baz|] `shouldBe` "foo bar baz"

    it "Tabs also at EOL" $ do
			[qns|
				foo  bar  baz				
			|] `shouldBe` "foo  bar  baz"

			[qns|			foo bar baz				|] `shouldBe` "foo bar baz"

    it "Escaped tabs" $ do
      [qns|		\tfoo|]    `shouldBe` "\tfoo"
      [qns|		\	foo	|]   `shouldBe` "\tfoo"
      [qns|	foo		\	|]   `shouldBe` "foo\t\t\t"
      [qns|	foo	\		|]   `shouldBe` "foo\t\t"
      [qns|	foo\			|] `shouldBe` "foo\t"

  it "Tails" $ do
    [qns|    
           foo   
                 |] `shouldBe` "foo"
    [qns|	 
         	
          foo	 
               
             	
                 |] `shouldBe` "foo"
    [qns|				
            foo			
            				
            				|] `shouldBe` "foo"

  describe "New README examples" $ do

    it "First example" $
      [qns| Hello,
            world!
            Pi is {floor pi}.{floor $ (pi - 3) * 100}… |]
        `shouldBe` "Hello, world! Pi is {floor pi}.{floor $ (pi - 3) * 100}…"

    it "Simple usage example" $
      [qns|
        <article>
          <h1>{title}</h1>
          <p>{text}</p>
        </article>
      |]
        `shouldBe`
          "<article> <h1>{title}</h1> <p>{text}</p> </article>"

    it "Can escape spaces" $
      [qns|   you can escape spaces
            \ when you need them    |]
        `shouldBe`
          "you can escape spaces  when you need them"

    -- By 'not really' it means that it just copied from `QM` and shows what
    -- happens with the same input if you use it with this quoter.
    it "Indentation and line breaks are ignored (not really)" $
      [qns|
              indentation and li
        ne bre
         aks are i
             gno
           red
             (not really)
      |]
      `shouldBe` "indentation and li ne bre aks are i gno red (not really)"

    it "Escaping indentation or line breaks" $
      [qns|  \  You can escape indentation or\n
                line breaks when you need them! \  |]
        `shouldBe`
          "  You can escape indentation or\n line breaks when you need them!  "

    it "Interpolation blocks can be escaped too" $
      [qns| Interpolation blocks can be escaped too: {1+2} \{3+4} |]
        `shouldBe`
          "Interpolation blocks can be escaped too: {1+2} \\{3+4}"

    it "Interpolation" $ [qns| foo {1+2} |] `shouldBe` "foo {1+2}"

  it "Haddock example" $ do
    [qns| foo {'b':'a':'r':""}
          baz |] `shouldBe` "foo {'b':'a':'r':\"\"} baz"
    [qns| foo
          {'b':'a':'r':""}
          baz |] `shouldBe` "foo {'b':'a':'r':\"\"} baz"
    -- Double-space with escaping
    [qns| foo {'b':'a':'r':""}
        \ baz |] `shouldBe` "foo {'b':'a':'r':\"\"}  baz"
