-- WARNING! Do not remove trailing whitespace or tabs, it's part of the test!
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QMB.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qmb)

newtype TestFoo = TestFoo {testBar :: Int} deriving (Show)
newtype TestFoo2 = TestFoo2 {test1 :: TestFoo} deriving (Show)

testFoo :: TestFoo
testFoo = TestFoo 42

testFoo2 :: TestFoo2
testFoo2 = TestFoo2 testFoo


spec :: Spec
spec = do

  it "Works as expected" $
    [qmb|
      foo
      {1+2}
      bar
    |] `shouldBe` "foo\n3\nbar"

  it "Explicitly cutting off line breaks" $ do
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

  it "Empty string" $ [qmb|  |] `shouldBe` ""

  it "Type annotation in interpolation block" $
    [qmb|{10 :: Float}|] `shouldBe` "10.0"

  describe "Escaping" $ do

    it "Escaping interpolation symbols inside interpolation block" $ do
      [qmb|foo {"b{a{r"} baz|]   `shouldBe` "foo b{a{r baz"
      [qmb|foo {"b\}a\}r"} baz|] `shouldBe` "foo b}a}r baz"

    it "Escaping backslashes" $ do [qmb| foo\bar |]    `shouldBe` "foo\\bar"
                                   [qmb| foo\\bar |]   `shouldBe` "foo\\bar"
                                   [qmb| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                   [qmb| foo\\\\bar |] `shouldBe` "foo\\\\bar"

    it "Escaping space by backslash at EOL after space\
       \ (line break is cutted off)" $
      [qmb| foo \
            bar |] `shouldBe` "foo bar"

    it "Escaped spaces at the edges" $ do [qmb| foo\ |] `shouldBe` "foo "
                                          [qmb|\ foo |] `shouldBe` " foo"

    it "Escaping backslash itself when it makes sense" $ do
      [qmb| foo\nbar  |] `shouldBe` "foo\nbar"
      [qmb| foo\\nbar |] `shouldBe` "foo\\nbar"
      [qmb| foo\tbar  |] `shouldBe` "foo\tbar"
      [qmb| foo\\tbar |] `shouldBe` "foo\\tbar"
      [qmb| foo\	bar |] `shouldBe` "foo\tbar"
      [qmb| foo\\	bar |] `shouldBe` "foo\\\tbar"
      [qmb| foo\ bar  |] `shouldBe` "foo bar"
      [qmb| foo\\ bar |] `shouldBe` "foo\\ bar"

      [qmb| foo\
            bar  |] `shouldBe` "foobar"
      [qmb| foo\\
            bar  |] `shouldBe` "foo\\\nbar"

    describe "Escaping inside interpolation blocks" $ do

      it "Line-breaks must be interpreted as just haskell code" $ do
        [qmb| {'\n'}          |] `shouldBe` "\n"
        [qmb| {"foo\nbar"}    |] `shouldBe` "foo\nbar"
        [qmb| {"foo\\nbar"}   |] `shouldBe` "foo\\nbar"
        [qmb| {"foo\\\nbar"}  |] `shouldBe` "foo\\\nbar"
        [qmb| {"foo\\\\nbar"} |] `shouldBe` "foo\\\\nbar"

      it "Escaping for multiline strings" $ do
        [qmb| {"foo\
               \bar\
               \baz"} |] `shouldBe` "foobarbaz"
        [qmb| {"foo \
               \bar\
               \ baz"} |] `shouldBe` "foo bar baz"
        [qmb| x  \
              {"foo \
              \bar\
              \ baz"}  \
              y |] `shouldBe` "x  foo bar baz  y"

      it "Escaping of close-bracket '}'" $ do
        [qmb| { testFoo {testBar = 9000\} } |]
          `shouldBe` show testFoo {testBar = 9000}
        [qmb| foo{ testFoo {testBar = 9000\} }bar |]
          `shouldBe` "foo" ++ show testFoo {testBar = 9000} ++ "bar"
        [qmb|foo{testFoo2 {test1 = (test1 testFoo2) {testBar = 9000\}\}}bar|]
          `shouldBe` "foo" ++ show testFoo2 {test1 = TestFoo 9000} ++ "bar"
        [qmb| foo { testFoo2 {
                test1 = (test1 testFoo2)
                        { testBar = 9000 \}
                            \}
                  } bar |]
          `shouldBe` "foo " ++ show testFoo2 {test1 = TestFoo 9000} ++ " bar"
        [qmb| {"\}"}   |] `shouldBe` "}"
        [qmb| {"\\\}"} |] `shouldBe` "\\}"
        [qmb| {123}}   |] `shouldBe` "123}"

      it "When interpolation block is escaped\
         \ everything must be interpreted as usual" $ do
        [qmb| \{ foo\nbar\\baz\} } |] `shouldBe` "{ foo\nbar\\baz\\} }"
        [qmb| \{ foo\
                 bar } |] `shouldBe` "{ foobar }"

      it "Tabs characters in string inside interpolation block" $ do
        [qmb| {"foo\t\tbar" } |] `shouldBe` "foo\t\tbar"
        [qmb| {"foo		bar"  } |] `shouldBe` "foo\t\tbar"
        [qmb| {"foo\\t\tbar"} |] `shouldBe` "foo\\t\tbar"

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

    it "First example" $
      [qmb| Hello,
            world!
            Pi is {floor pi}.{floor $ (pi - 3) * 100}… |]
        `shouldBe` "Hello,\nworld!\nPi is 3.14…"

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

    it "Can escape spaces" $
      [qmb|   you can escape spaces
            \ when you need them    |]
        `shouldBe`
          "you can escape spaces\n when you need them"

    -- By 'not really' it means that it just copied from `QM` and shows what
    -- happens with the same input if you use it with this quoter.
    it "Indentation and line breaks are ignored (not really)" $
      [qmb|
              indentation and li
        ne bre
         aks are i
             gno
           red
             (not really)
      |]
      `shouldBe` "indentation and li\nne bre\naks are i\ngno\nred\n(not really)"

    it "Escaping indentation or line breaks" $
      [qmb|  \  You can escape indentation or\n
                line breaks when you need them! \  |]
        `shouldBe`
          "  You can escape indentation or\n\nline breaks when you need them!  "

    it "Interpolation blocks can be escaped too" $
      [qmb| Interpolation blocks can be escaped too: {1+2} \{3+4} |]
        `shouldBe`
          "Interpolation blocks can be escaped too: 3 {3+4}"

    it "Interpolation" $ [qmb| foo {1+2} |] `shouldBe` "foo 3"

  it "Haddock example" $ do
    [qmb| foo
          {'b':'a':'r':""}
          baz |] `shouldBe` "foo\nbar\nbaz"
    [qmb| foo
          {'b':'a':'r':""}
          baz |] `shouldBe` "foo\nbar\nbaz"
    -- Double-space with escaping
    [qmb| foo {'b':'a':'r':""}
        \ baz |] `shouldBe` "foo bar\n baz"
