# [qm|interpolated-string|]

[![Hackage](https://img.shields.io/hackage/v/qm-interpolated-string.svg)](https://hackage.haskell.org/package/qm-interpolated-string)
[![Build Status](https://travis-ci.org/unclechu/haskell-qm-interpolated-string.svg?branch=master)](https://travis-ci.org/unclechu/haskell-qm-interpolated-string)

Implementation of interpolated multiline string
[QuasiQuoter](https://wiki.haskell.org/Quasiquotation)
that ignores indentation and trailing whitespaces.

Actually it's modification of
[interpolatedstring-perl6](https://github.com/audreyt/interpolatedstring-perl6)
package. I used it to implemenent my own strings I really like.

This implementation looks just like `qc`
from **interpolatedstring-perl6** package but ignores any indentation,
line breaks (except explicitly written using `\n` char)
and trailing whitespaces.

'm' in 'qm' means 'multiline'.

You could write a decoratively formatted string and your
decorative indentation and line breaks wont go to the string,
but when you really need it, you could just escape it using backslash.

## Simple usage example

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.InterpolatedString.QM (qm)

main :: IO ()
main = putStrLn [qm| hello
                   \ world |]
```

## Tables

### All QuasiQuoters

| QuasiQuoter | Interpolation | Indentation | Line-breaks          | Trailing whitespaces |
|-------------|---------------|-------------|----------------------|----------------------|
| `qm`        | ✓             | Removed     | Removed              | Removed              |
| `qn`        | ✗             | Removed     | Removed              | Removed              |
| `qmb`       | ✓             | Removed     | Kept                 | Removed              |
| `qnb`       | ✗             | Removed     | Kept                 | Removed              |
| `qms`       | ✓             | Removed     | Replaced with spaces | Removed              |
| `qns`       | ✗             | Removed     | Replaced with spaces | Removed              |

### About naming logic

| Contains in its name | What means                       | QuasiQuoters       |
|----------------------|----------------------------------|--------------------|
| `m`                  | Resolves interpolation blocks    | `qm`, `qmb`, `qms` |
| `n`                  | Without interpolation            | `qn`, `qnb`, `qns` |
| `b`                  | Keeps line-breaks                | `qmb`, `qnb`       |
| `s`                  | Replaces line-breaks with spaces | `qms`, `qns`       |

## More examples

```haskell
[qm|   hello world,
     \ what's going on here?  |]
-- Result: "hello world, what's going on here?"
```

```haskell
[qm|
      it's actual
      ly ignored
   |]
-- Result: "it's actually ignored"
```

```haskell
[qm|  \  You could explicitly escape indentation or\n
         line-breaks when you really need it!  \
   |]
-- Result: "  You could explicitly escape indentation or\nline-breaks when you really need it!  "
```

```haskell
[qm| {1+2} \{3+4} |]
-- Result: "3 {3+4}"
```

There is also very similar to `qm` QuasiQuoter
named as `qn` that do the same except interpolation:

```haskell
[qn| foo {1+2} |]
-- Result: "foo {1+2}"
```

## Author

[Viacheslav Lotsmanov](https://github.com/unclechu)

## License

[The Unlicense](./LICENSE)
