## <a name="v0.3.1.0"></a>0.3.1.0

* Support GHC 9.2.3
* Drop Travis CI (no longer available for free for open source projects)

* Add GitHub Actions CI configuration
  * Nix-based
  * Plain Cabal & Stack setups with multiple GHC versions

* Add Nix configuration (only affects development)
* Add [Stack](https://haskellstack.org) support

* Drop GHC 7.x support because I have no options on CI to test it. It doesn’t
  mean it will no longer work with 7.x for sure but it’s not tested anymore.

* Increase minimal Cabal version requirement from `1.8` to `1.10` because
  Hackage requires at least `1.10` now. Also add `default-language` to every
  component because Hackage reports this error:

  > Packages using 'cabal-version: >= 1.10' and before 'cabal-version: 3.4' must
  > specify the 'default-language' field for each component (e.g. Haskell98 or
  > Haskell2010).

## <a name="v0.3.0.0"></a>0.3.0.0

* Testing on GHC 8.2.2
* More tests to cover more usage scenarios
* Ability to use `}` inside an interpolation block by escaping it

### **WARNING!** Breaking changes

* `\r` characters are no longer pre-removed.

  Up to [v0.2.1.0](#v0.2.1.0) all `\r` characters were pre-removed. When you
  compile your code with GHC you can use either *LF* or *CRLF* for line-breaks
  but not *CR* alone. When I changed handling of interpolation blocks (see
  below) I needed to get contents of interpolations blocks without **any**
  modifications, so I replaced pre-removing all *CR*s with explicit handling of
  *CRLF* in patterns. If your code ever was depending on `\r` symbols appearing
  alone inside quoters (that I can't even imagine) it could break your code.
  But it will probably never happen, I'm just noticing it here.

* Fix for interpolation blocks parsing.

  Once I noticed that `[qm|{"\n"}|]` compiles to `"n"`, I considered this as a
  bug, I also realized that interpolation blocks aren't interpreted as a bare
  Haskell code as I was expecting. My bad, I've missed that, haven't written
  enough tests to cover such scenarios, it migrated from original
  *interpolatedstring-perl6* package. So I had to fix this mistake,
  notwithstanding it can break your code when you update the library. Now
  everything inside interpolation blocks is taken as bare haskell code as
  possible.

## <a name="v0.2.1.0"></a>0.2.1.0

* Support GHC 7.4.1
* Internal modules moved to `other-modules` section

## <a name="v0.2.0.0"></a>0.2.0.0

* Added tab (`\t`) symbol escaping
  (breaks backward compatibility with [v0.1.1.0](#v0.1.1.0))
* Support new [LTS Haskell 9.0 (ghc-8.0.2)](https://www.stackage.org/lts-9.0)
  (updated upper version of **haskell-src-meta** from 0.7.x to 0.8.x)
* Added `qmb` QuasiQuoter,
  it's `qm` + `b` (line-<b>B</b>reaks),
  it works just as `qm` but keeps line breaks (still ignores indendation)
* Added `qnb` QuasiQuoter (`qmb` without interpolation),
  it's `qn` + `b` (line-<b>B</b>reaks),
  it works just as `qn` but keeps line breaks (still ignores indendation)
* Added `qms` QuasiQuoter,
  it's `qm` + `s` (<b>S</b>paces),
  it works just as `qmb` but kept line breaks replaced with spaces
* Added `qns` QuasiQuoter (`qms` without interpolation),
  it's `qn` + `s` (<b>S</b>paces),
  it works just as `qnb` but kept line breaks replaced with spaces
* More docs and tests

## <a name="v0.1.1.0"></a>0.1.1.0

* Added `qn` QuasiQuoter as alternative to `qm` but without interpolation
* Some code refactoring
* More docs and tests
