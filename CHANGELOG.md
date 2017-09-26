## <a name="v0.x.x.x"></a>0.x.x.x

* Added tab (`\t`) symbol escaping
  (breaks backward compatibility with [v0.1.1.0](#v0.1.1.0))
* Support new [LTS Haskell 9.0 (ghc-8.0.2)](https://www.stackage.org/lts-9.0)
  (updated upper version of **haskell-src-meta** from 0.7.x to 0.8.x)
* Added `qmb` QuasiQuoter,
  it's `qm` + `b` (line-<b>B</b>reaks),
  it works just as `qm` but keeps line-breaks (still ignores indendation)
* Added `qnb` QuasiQuoter (`qmb` without interpolation),
  it's `qn` + `b` (line-<b>B</b>reaks),
  it works just as `qn` but keeps line-breaks (still ignores indendation)

## <a name="v0.1.1.0"></a>0.1.1.0

* Added `qn` QuasiQuoter as alternative to `qm` but without interpolation
* Some code refactoring
* More docs and tests
