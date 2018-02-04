# [qm|interpolated-string|]

[![Hackage](https://img.shields.io/hackage/v/qm-interpolated-string.svg)](https://hackage.haskell.org/package/qm-interpolated-string)
[![Build Status](https://travis-ci.org/unclechu/haskell-qm-interpolated-string.svg?branch=master)](https://travis-ci.org/unclechu/haskell-qm-interpolated-string)

Implementation of interpolated multiline string
[QuasiQuoter](https://wiki.haskell.org/Quasiquotation)
that ignores indentation and trailing whitespaces.

Actually it's modification of
[interpolatedstring-perl6](https://github.com/audreyt/interpolatedstring-perl6)
package. I've forked it to implemenent my own strings I really like.

This implementation based on `qc` from **interpolatedstring-perl6** package
but ignores any indentation, line breaks
(except explicitly written using `\n` char)
and trailing whitespaces.

* 'm' in `qm` means '**M**ultiline'.
* 'n' in `qn` means '**N**o interpolation'.
* 'b' in `qmb`/`qnb` means 'line **B**reaks'.
* 's' in `qms`/`qns` means '**S**paces'.

Write a decoratively formatted string and your
decorative indentation and line breaks wont go to result string,
but when you really need it, you could just escape it using backslash.

## Usage example

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.InterpolatedString.QM

main :: IO ()
main = do
  -- Hello, world! Pi is 3.14…
  putStrLn [qms| Hello,
                 world!
                 Pi is {floor pi}.{floor $ (pi - 3) * 100}… |]

  -- Some examples with HTML below to demonstrate the difference
  -- between all of the quasi-quoters.

  let title = "Testing"
      text  = "Some testing text"

  -- <article><h1>Testing</h1><p>Some testing text</p></article>
  putStrLn [qm|
    <article>
      <h1>{title}</h1>
      <p>{text}</p>
    </article>
  |]

  -- <article><h1>{title}</h1><p>{text}</p></article>
  putStrLn [qn|
    <article>
      <h1>{title}</h1>
      <p>{text}</p>
    </article>
  |]

  -- <article> <h1>Testing</h1> <p>Some testing text</p> </article>
  putStrLn [qms|
    <article>
      <h1>{title}</h1>
      <p>{text}</p>
    </article>
  |]

  -- <article> <h1>{title}</h1> <p>{text}</p> </article>
  putStrLn [qns|
    <article>
      <h1>{title}</h1>
      <p>{text}</p>
    </article>
  |]

  -- <article>
  -- <h1>Testing</h1>
  -- <p>Some testing text</p>
  -- </article>
  putStrLn [qmb|
    <article>
      <h1>{title}</h1>
      <p>{text}</p>
    </article>
  |]

  -- <article>
  -- <h1>{title}</h1>
  -- <p>{text}</p>
  -- </article>
  putStrLn [qnb|
    <article>
      <h1>{title}</h1>
      <p>{text}</p>
    </article>
  |]
```

## Tables

### All QuasiQuoters

```
| QuasiQuoter | Interpolation | Indentation | Line breaks          | Trailing whitespaces |
|-------------|---------------|-------------|----------------------|----------------------|
| qm          | ✓             | Removed     | Removed              | Removed              |
| qn          | ✗             | Removed     | Removed              | Removed              |
| qmb         | ✓             | Removed     | Kept                 | Removed              |
| qnb         | ✗             | Removed     | Kept                 | Removed              |
| qms         | ✓             | Removed     | Replaced with spaces | Removed              |
| qns         | ✗             | Removed     | Replaced with spaces | Removed              |
```

### About naming logic

```
| Contains in its name | What means                       | QuasiQuoters |
|----------------------|----------------------------------|--------------|
| m                    | Resolves interpolation blocks    | qm, qmb, qms |
| n                    | Without interpolation            | qn, qnb, qns |
| b                    | Keeps line breaks                | qmb, qnb     |
| s                    | Replaces line breaks with spaces | qms, qns     |
```

## About escaping

### Symbols that can be escaped

Backslash is used for escaping these:

  1. `\n` - line break
  2. `\ ` - space (space is supposed to be escaped when you're going to keep
            some part of indentation)
  3. `\↵` - backslash just before end of line cuts off line break
            (makes sense for `qmb`, `qnb`, `qms` and `qns`)
  4. `\{` - opening bracket of interpolation block
            (only for `qm`, `qmb` and `qms`, to prevent interpolation
            and interpret this block as plain text)
  5. `\t` or `\‣` (where `‣` is real tab symbol) - tab (escaping it to keep some
     part of indentation, or if you need tab symbol for some reason,
     escaping real tabs makes sense only for keeping some part of indentation)
  6. `\\` - backslash itself (for situations when you don't want to escape
            other symbols but just want backslash symbol, `\\t`, `\\n`, `\\↵`,
            `\\{`, etc., if backslash doesn't come with any of symbols from
            this list it is interpreted just as backslash symbol, keep in mind
            that `\\\` (without any of symbols from this list after)
            and `\\\\` are producing same result - `\\`)

### Escaping examples

```haskell
[qm| foo\nbar  |] -- "foo\nbar"
[qm| foo\\nbar |] -- "foo\\nbar"
[qm| foo\tbar  |] -- "foo\tbar"
[qm| foo\\tbar |] -- "foo\\tbar"
[qm| foo\‣bar  |] -- "foo\tbar"   (`‣` is real tab symbol)
[qm| foo\\‣bar |] -- "foo\\\tbar" (`‣` is real tab symbol)
[qm| foo\ bar  |] -- "foo bar"
[qm| foo\\ bar |] -- "foo\\ bar"

[qm| foo\
     bar  |] -- "foobar"
[qm| foo\\
     bar  |] -- "foo\\bar"

[qmb| foo\
      bar  |] -- "foobar"
[qmb| foo\\
      bar  |] -- "foo\\\nbar"

[qm| foo\bar    |] -- "foo\\bar"
[qm| foo\\bar   |] -- "foo\\bar"
[qm| foo\\\bar  |] -- "foo\\\\bar"
[qm| foo\\\\bar |] -- "foo\\\\bar"
```

## More examples

```haskell
[qm|   you can escape spaces
     \ when you need them    |]
-- Result: "you can escape spaces when you need them"
```

```haskell
[qm|
        indentation and li
  ne bre
   aks are i
       gno
     red
|]
-- Result: "indentation and line breaks are ignored"
```

```haskell
[qm|  \  You can escape indentation or\n
         line breaks when you need them! \  |]
-- Result: "  You can escape indentation or\nline breaks when you need them!  "
```

```haskell
[qm| Interpolation blocks can be escaped too: {1+2} \{3+4} |]
-- Result: "Interpolation blocks can be escaped too: 3 {3+4}"
```

If you don't need interpolation - just replace `m` to `n` in quasi-quoter name:

```haskell
[qm| foo {1+2} |] -- Result: "foo 3"
[qn| foo {1+2} |] -- Result: "foo {1+2}"

[qms| foo {1+2} |] -- Result: "foo 3"
[qns| foo {1+2} |] -- Result: "foo {1+2}"

[qmb| foo {1+2} |] -- Result: "foo 3"
[qnb| foo {1+2} |] -- Result: "foo {1+2}"
```

## Author

[Viacheslav Lotsmanov](https://github.com/unclechu)

## License

[The Unlicense](LICENSE)
