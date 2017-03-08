# [qm|interpolated-string|]

[![Build Status](https://secure.travis-ci.org/unclechu/haskell-qm-interpolated-string.svg)](http://travis-ci.org/unclechu/haskell-qm-interpolated-string)

Implementation of interpolated multiline strings that ignores indentation
and trailing whitespaces.

It's [QuasiQuoter](https://wiki.haskell.org/Quasiquotation).

Actually it's modification of
[interpolatedstring-perl6](https://github.com/audreyt/interpolatedstring-perl6)
package. I used it to implemenent my own strings I really like.

This implementation looks just like `qc`
from **interpolatedstring-perl6** package but ignores any indentation,
line-breaks (except explicitly written using `\n` char)
and trailing whitespaces.

'm' in 'qm' means 'multiline'.

You could write a decoratively formatted string and your
decorative indentation and line-breaks wont go to the string,
but when you really need it, you could just escape it using backslash.

## Simple usage example

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.InterpolatedString.QM (qm)

main :: IO ()
main = putStrLn [qm| hello
                   \ world |]
```

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

## Author

[Viacheslav Lotsmanov](https://github.com/unclechu)

## License

[The Unlicense](./LICENSE)
