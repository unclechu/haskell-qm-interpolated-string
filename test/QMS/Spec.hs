-- WARNING! Do not remove trailing whitespace or tabs, it's part of the test!
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module QMS.Spec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

-- local imports
import "qm-interpolated-string" Text.InterpolatedString.QM (qms)

newtype TestFoo = TestFoo {testBar :: Int} deriving (Show)
newtype TestFoo2 = TestFoo2 {test1 :: TestFoo} deriving (Show)

testFoo :: TestFoo
testFoo = TestFoo 42

testFoo2 :: TestFoo2
testFoo2 = TestFoo2 testFoo


spec :: Spec
spec = do

  it "Works as expected" $
    [qms|
      foo
      {1+2}
      bar
    |] `shouldBe` "foo 3 bar"

  it "Explicitly cutting off line breaks" $ do
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

    it "Third (explicit line breaks symbols)" $
      [qms|  \  You could explicitly escape indentation or\n
                line breaks when you really need it!  \
          |] `shouldBe` "  You could explicitly escape indentation or\n \
                        \line breaks when you really need it!  "

    it "Fourth (escaping interpolation blocks to show them as text)" $
      [qms| {1+2} \{3+4} |] `shouldBe` "3 {3+4}"

  it "Empty string" $ [qms|  |] `shouldBe` ""

  it "Type annotation in interpolation block" $
    [qms|{10 :: Float}|] `shouldBe` "10.0"

  describe "Escaping" $ do

    it "Escaping interpolation symbols inside interpolation block" $ do
      [qms|foo {"b{a{r"} baz|]   `shouldBe` "foo b{a{r baz"
      [qms|foo {"b\}a\}r"} baz|] `shouldBe` "foo b}a}r baz"

    it "Escaping backslashes" $ do [qms| foo\bar |]    `shouldBe` "foo\\bar"
                                   [qms| foo\\bar |]   `shouldBe` "foo\\bar"
                                   [qms| foo\\\bar |]  `shouldBe` "foo\\\\bar"
                                   [qms| foo\\\\bar |] `shouldBe` "foo\\\\bar"

    it "Escaping space by backslash at EOL after space\
       \ (line break is cutted off)" $
      [qms| foo \
            bar |] `shouldBe` "foo bar"

    it "Escaped spaces at the edges" $ do [qms| foo\ |] `shouldBe` "foo "
                                          [qms|\ foo |] `shouldBe` " foo"

    it "Escaping backslash itself when it makes sense" $ do
      [qms| foo\nbar  |] `shouldBe` "foo\nbar"
      [qms| foo\\nbar |] `shouldBe` "foo\\nbar"
      [qms| foo\tbar  |] `shouldBe` "foo\tbar"
      [qms| foo\\tbar |] `shouldBe` "foo\\tbar"
      [qms| foo\	bar |] `shouldBe` "foo\tbar"
      [qms| foo\\	bar |] `shouldBe` "foo\\\tbar"
      [qms| foo\ bar  |] `shouldBe` "foo bar"
      [qms| foo\\ bar |] `shouldBe` "foo\\ bar"

      [qms| foo\
            bar  |] `shouldBe` "foobar"
      [qms| foo\\
            bar  |] `shouldBe` "foo\\ bar"

    describe "Escaping inside interpolation blocks" $ do

      it "Line-breaks must be interpreted as just haskell code" $ do
        [qms| {'\n'}          |] `shouldBe` "\n"
        [qms| {"foo\nbar"}    |] `shouldBe` "foo\nbar"
        [qms| {"foo\\nbar"}   |] `shouldBe` "foo\\nbar"
        [qms| {"foo\\\nbar"}  |] `shouldBe` "foo\\\nbar"
        [qms| {"foo\\\\nbar"} |] `shouldBe` "foo\\\\nbar"

      it "Escaping for multiline strings" $ do
        [qms| {"foo\
               \bar\
               \baz"} |] `shouldBe` "foobarbaz"
        [qms| {"foo \
               \bar\
               \ baz"} |] `shouldBe` "foo bar baz"
        [qms| x  \
              {"foo \
              \bar\
              \ baz"}  \
              y |] `shouldBe` "x  foo bar baz  y"

      it "Escaping of close-bracket '}'" $ do
        [qms| { testFoo {testBar = 9000\} } |]
          `shouldBe` show testFoo {testBar = 9000}
        [qms| foo{ testFoo {testBar = 9000\} }bar |]
          `shouldBe` "foo" ++ show testFoo {testBar = 9000} ++ "bar"
        [qms|foo{testFoo2 {test1 = (test1 testFoo2) {testBar = 9000\}\}}bar|]
          `shouldBe` "foo" ++ show testFoo2 {test1 = TestFoo 9000} ++ "bar"
        [qms| foo { testFoo2 {
                test1 = (test1 testFoo2)
                        { testBar = 9000 \}
                            \}
                  } bar |]
          `shouldBe` "foo " ++ show testFoo2 {test1 = TestFoo 9000} ++ " bar"
        [qms| {"\}"}   |] `shouldBe` "}"
        [qms| {"\\\}"} |] `shouldBe` "\\}"
        [qms| {123}}   |] `shouldBe` "123}"

      it "When interpolation block is escaped\
         \ everything must be interpreted as usual" $ do
        [qms| \{ foo\nbar\\baz\} } |] `shouldBe` "{ foo\nbar\\baz\\} }"
        [qms| \{ foo\
                 bar } |] `shouldBe` "{ foobar }"

      it "Tabs characters in string inside interpolation block" $ do
        [qms| {"foo\t\tbar" } |] `shouldBe` "foo\t\tbar"
        [qms| {"foo		bar"  } |] `shouldBe` "foo\t\tbar"
        [qms| {"foo\\t\tbar"} |] `shouldBe` "foo\\t\tbar"

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

  describe "New README examples" $ do

    it "First example" $
      [qms| Hello,
            world!
            Pi is {floor pi}.{floor $ (pi - 3) * 100}… |]
        `shouldBe` "Hello, world! Pi is 3.14…"

    it "Simple usage example" $ do
      let title = "Testing"
          text = "Some testing text"
      [qms|
        <article>
          <h1>{title}</h1>
          <p>{text}</p>
        </article>
      |]
        `shouldBe`
          "<article> <h1>Testing</h1> <p>Some testing text</p> </article>"

    it "Can escape spaces" $
      [qms|   you can escape spaces
            \ when you need them    |]
        `shouldBe`
          "you can escape spaces  when you need them"

    -- By 'not really' it means that it just copied from `QM` and shows what
    -- happens with the same input if you use it with this quoter.
    it "Indentation and line breaks are ignored (not really)" $
      [qms|
              indentation and li
        ne bre
         aks are i
             gno
           red
             (not really)
      |]
      `shouldBe` "indentation and li ne bre aks are i gno red (not really)"

    it "Escaping indentation or line breaks" $
      [qms|  \  You can escape indentation or\n
                line breaks when you need them! \  |]
        `shouldBe`
          "  You can escape indentation or\n line breaks when you need them!  "

    it "Interpolation blocks can be escaped too" $
      [qms| Interpolation blocks can be escaped too: {1+2} \{3+4} |]
        `shouldBe`
          "Interpolation blocks can be escaped too: 3 {3+4}"

    it "Interpolation" $ [qms| foo {1+2} |] `shouldBe` "foo 3"

  it "Haddock example" $ do
    [qms| foo {'b':'a':'r':""}
          baz |] `shouldBe` "foo bar baz"
    [qms| foo
          {'b':'a':'r':""}
          baz |] `shouldBe` "foo bar baz"
    -- Double-space with escaping
    [qms| foo {'b':'a':'r':""}
        \ baz |] `shouldBe` "foo bar  baz"
