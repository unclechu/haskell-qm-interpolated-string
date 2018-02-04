-- WARNING! Do not remove trailing whitespace or tabs, it's part of the test!
{-# OPTIONS_GHC -fno-warn-tabs #-}

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

  it "Explicitly cutting off line breaks" $ do
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

    it "Third (explicit line breaks symbols, line break plus space)" $
      [qnb|  \  You could explicitly escape indentation or\n
                line breaks when you really need it!  \
          |] `shouldBe` "  You could explicitly escape indentation or\n\n\
                        \line breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qnb| {1+2} \{3+4} |] `shouldBe` "{1+2} \\{3+4}"

  it "Empty string" $ [qnb|  |] `shouldBe` ""

  it "Type annotation in interpolation block" $
    [qnb|{10 :: Float}|] `shouldBe` "{10 :: Float}"

  describe "Escaping" $ do

    it "Escaping interpolation symbols inside interpolation block" $ do
      [qnb|foo {"b{a{r"} baz|] `shouldBe` "foo {\"b{a{r\"} baz"
      [qnb|foo {"b\}a\}r"} baz|] `shouldBe` "foo {\"b\\}a\\}r\"} baz"

    it "Escaping backslashes" $ do [qnb| foo\bar |]    `shouldBe` "foo\\bar"
                                   [qnb| foo\\bar |]   `shouldBe` "foo\\bar"
                                   [qnb| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                   [qnb| foo\\\\bar |] `shouldBe` "foo\\\\bar"

    it "Escaping space by backslash at EOL after space\
       \ (line break is cutted off)" $
      [qnb| foo \
            bar |] `shouldBe` "foo bar"

    it "Escaped spaces at the edges" $ do [qnb| foo\ |] `shouldBe` "foo "
                                          [qnb|\ foo |] `shouldBe` " foo"

    it "Escaping backslash itself when it makes sense" $ do
      [qnb| foo\nbar  |] `shouldBe` "foo\nbar"
      [qnb| foo\\nbar |] `shouldBe` "foo\\nbar"
      [qnb| foo\tbar  |] `shouldBe` "foo\tbar"
      [qnb| foo\\tbar |] `shouldBe` "foo\\tbar"
      [qnb| foo\	bar |] `shouldBe` "foo\tbar"
      [qnb| foo\\	bar |] `shouldBe` "foo\\\tbar"
      [qnb| foo\ bar  |] `shouldBe` "foo bar"
      [qnb| foo\\ bar |] `shouldBe` "foo\\ bar"

      [qnb| foo\
            bar  |] `shouldBe` "foobar"
      [qnb| foo\\
            bar  |] `shouldBe` "foo\\\nbar"

    -- By 'not really' it means we just shows here what happens with examples
    -- for QMB when we use them with QNB quoter.
    describe "Escaping inside interpolation blocks (not really!)" $ do

      it "Line-breaks must be interpreted as just haskell code\
         \ (not really!)" $ do
        [qnb| {'\n'}          |] `shouldBe` "{'\n'}"
        [qnb| {"foo\nbar"}    |] `shouldBe` "{\"foo\nbar\"}"
        [qnb| {"foo\\nbar"}   |] `shouldBe` "{\"foo\\nbar\"}"
        [qnb| {"foo\\\nbar"}  |] `shouldBe` "{\"foo\\\nbar\"}"
        [qnb| {"foo\\\\nbar"} |] `shouldBe` "{\"foo\\\\nbar\"}"

      it "Escaping for multiline strings" $ do
        [qnb| {"foo\
               \bar\
               \baz"} |] `shouldBe` "{\"foo\\bar\\baz\"}"
        [qnb| {"foo \
               \bar\
               \ baz"} |] `shouldBe` "{\"foo \\bar baz\"}"
        [qnb| x  \
              {"foo \
              \bar\
              \ baz"}  \
              y |] `shouldBe` "x  {\"foo \\bar baz\"}  y"

      it "Escaping of close-bracket '}'" $ do
        [qnb| { testFoo {testBar = 9000\} } |]
          `shouldBe` "{ testFoo {testBar = 9000\\} }"
        [qnb| foo{ testFoo {testBar = 9000\} }bar |]
          `shouldBe` "foo{ testFoo {testBar = 9000\\} }bar"
        [qnb|foo{testFoo2 {test1 = (test1 testFoo2) {testBar = 9000\}\}}bar|]
          `shouldBe`
            "foo{testFoo2 {test1 = (test1 testFoo2) {testBar = 9000\\}\\}}bar"
        [qnb| foo { testFoo2 {
                test1 = (test1 testFoo2)
                        { testBar = 9000 \}
                            \}
                  } bar |]
          `shouldBe` "foo { testFoo2 {\ntest1 = (test1 testFoo2)\n\
                     \{ testBar = 9000 \\}\n\\}\n} bar"
        [qnb| {"\}"}   |] `shouldBe` "{\"\\}\"}"
        [qnb| {"\\\}"} |] `shouldBe` "{\"\\\\}\"}"
        [qnb| {123}}   |] `shouldBe` "{123}}"

      it "When interpolation block is escaped\
         \ everything must be interpreted as usual (not really)" $ do
        [qnb| \{ foo\nbar\\baz\} } |] `shouldBe` "\\{ foo\nbar\\baz\\} }"
        [qnb| \{ foo\
                 bar } |] `shouldBe` "\\{ foobar }"

      it "Tabs characters in string inside interpolation block\
         \ (not really)" $ do
        [qnb| {"foo\t\tbar" } |] `shouldBe` "{\"foo\t\tbar\" }"
        [qnb| {"foo		bar"  } |] `shouldBe` "{\"foo\t\tbar\"  }"
        [qnb| {"foo\\t\tbar"} |] `shouldBe` "{\"foo\\t\tbar\"}"

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

  describe "New README examples" $ do

    it "First example" $
      [qnb| Hello,
            world!
            Pi is {floor pi}.{floor $ (pi - 3) * 100}… |]
        `shouldBe` "Hello,\nworld!\nPi is {floor pi}.{floor $ (pi - 3) * 100}…"

    it "Simple usage example" $
      [qnb|
        <article>
          <h1>{title}</h1>
          <p>{text}</p>
        </article>
      |]
        `shouldBe`
          "<article>\n<h1>{title}</h1>\n<p>{text}</p>\n</article>"

    it "Can escape spaces" $
      [qnb|   you can escape spaces
            \ when you need them    |]
        `shouldBe`
          "you can escape spaces\n when you need them"

    -- By 'not really' it means that it just copied from `QM` and shows what
    -- happens with the same input if you use it with this quoter.
    it "Indentation and line breaks are ignored (not really)" $
      [qnb|
              indentation and li
        ne bre
         aks are i
             gno
           red
             (not really)
      |]
      `shouldBe` "indentation and li\nne bre\naks are i\ngno\nred\n(not really)"

    it "Escaping indentation or line breaks" $
      [qnb|  \  You can escape indentation or\n
                line breaks when you need them! \  |]
        `shouldBe`
          "  You can escape indentation or\n\nline breaks when you need them!  "

    it "Interpolation blocks can be escaped too" $
      [qnb| Interpolation blocks can be escaped too: {1+2} \{3+4} |]
        `shouldBe`
          "Interpolation blocks can be escaped too: {1+2} \\{3+4}"

    it "Interpolation" $ [qnb| foo {1+2} |] `shouldBe` "foo {1+2}"

  it "Haddock example" $ do
    [qnb| foo {'b':'a':'r':""}
          baz |] `shouldBe` "foo {'b':'a':'r':\"\"}\nbaz"
    [qnb| foo
          {'b':'a':'r':""}
          baz |] `shouldBe` "foo\n{'b':'a':'r':\"\"}\nbaz"
    [qnb| foo {'b':'a':'r':""}
        \ baz |] `shouldBe` "foo {'b':'a':'r':\"\"}\n baz"
