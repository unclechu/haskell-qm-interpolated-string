-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

{-# LANGUAGE PackageImports #-}

module Text.InterpolatedString.QM (qm, qn, qmb, qnb, qms, qns, ShowQ (..)) where

import "template-haskell" Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))

-- local imports
import Text.InterpolatedString.QM.ShowQ.Class (ShowQ (..))
import qualified Text.InterpolatedString.QM.Internal.Parsers as Parsers


-- | QuasiQuoter for multiline interpolated string.
--
-- @
-- ['qm'| foo {'b':'a':'r':""}
--    \\ baz |] -- "foo bar baz"
-- @
--
-- Symbols that could be escaped:
--
--   * @\\@ - backslash itself (two backslashes one by one: @\\\\@)
--     @['qm'| foo\\\\bar |] -- "foo\\\\bar"@
--
--   * Space symbol at the edge
--     (to put it to the output instead of just ignoring it)
--     @['qm'| foo\\ |] -- "foo "@ or @['qm'|\\ foo |] -- " foo"@
--
--   * Line break @\\n@ (actual line breaks are ignored)
--
--   * Opening bracket of interpolation block @\\{@
--     to prevent interpolation and put it as it is
--     @['qm'| {1+2} \\{3+4} |] -- "3 {3+4}"@
--
qm :: QuasiQuoter
qm = QuasiQuoter Parsers.qm
  (error "Cannot use 'qm' as a pattern")
  (error "Cannot use 'qm' as a type")
  (error "Cannot use 'qm' as a dec")


-- | Works just like 'qm' but without interpolation
--   (just multiline string with decorative indentation).
--
-- @
-- ['qn'| foo {'b':'a':'r':""}
--    \\ baz |] -- "foo {'b':'a':'r':\\"\\"} baz"
-- @
--
-- Interpolation blocks goes just as text:
--
-- @['qn'| {1+2} \\{3+4} |] -- "{1+2} \\\\{3+4}"@
--
qn :: QuasiQuoter
qn = QuasiQuoter Parsers.qn
  (error "Cannot use 'qn' as a pattern")
  (error "Cannot use 'qn' as a type")
  (error "Cannot use 'qn' as a dec")


-- | 'qm' + `b` (line-/B/reaks)
--
-- @
-- ['qmb'| foo
--       {'b':'a':'r':""}
--       baz |] -- "foo\\nbar\\nbaz"
-- @
--
qmb :: QuasiQuoter
qmb = QuasiQuoter Parsers.qmb
  (error "Cannot use 'qmb' as a pattern")
  (error "Cannot use 'qmb' as a type")
  (error "Cannot use 'qmb' as a dec")


-- | 'qn' + `b` (line-/B/reaks)
-- Works just like 'qmb' but without interpolation.
--
-- @
-- ['qnb'| foo
--       {'b':'a':'r':""}
--       baz |] -- "foo\\n{'b':'a':'r':\\"\\"}\\nbaz"
-- @
qnb :: QuasiQuoter
qnb = QuasiQuoter Parsers.qnb
  (error "Cannot use 'qnb' as a pattern")
  (error "Cannot use 'qnb' as a type")
  (error "Cannot use 'qnb' as a dec")


-- | 'qm' + `s` (/S/paces)
--
-- @
-- ['qms'| foo
--       {'b':'a':'r':""}
--       baz |] -- "foo bar baz"
-- @
qms :: QuasiQuoter
qms = QuasiQuoter Parsers.qms
  (error "Cannot use 'qms' as a pattern")
  (error "Cannot use 'qms' as a type")
  (error "Cannot use 'qms' as a dec")


-- | 'qn' + `s` (/S/paces)
--
-- Works just like 'qms' but without interpolation.
--
-- @
-- ['qns'| foo
--       {'b':'a':'r':""}
--       baz |] -- "foo {'b':'a':'r':\\"\\"} baz"
-- @
qns :: QuasiQuoter
qns = QuasiQuoter Parsers.qns
  (error "Cannot use 'qns' as a pattern")
  (error "Cannot use 'qns' as a type")
  (error "Cannot use 'qns' as a dec")
