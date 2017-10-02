-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Text.InterpolatedString.QM.Parsers (qm, qn, qmb, qnb) where

import qualified "template-haskell" Language.Haskell.TH as TH

-- local imports

import Text.InterpolatedString.QM.Parsers.TH (parserTpl)

import Text.InterpolatedString.QM.Parsers.Types ( Parser
                                                , StringPart (..)
                                                , LineBreaks (..)
                                                )

import Text.InterpolatedString.QM.Parsers.Helpers ( unQX
                                                  , clearIndentAtStart
                                                  , clearIndentAtSOF
                                                  , clearIndentTillEOF
                                                  , clearFirstQXBLineBreak
                                                  , clearLastQXBLineBreak
                                                  , makeExpr
                                                  )


$(parserTpl "parseQM"  True  IgnoreLineBreaks)
$(parserTpl "parseQN"  False IgnoreLineBreaks)
$(parserTpl "parseQMB" True  KeepLineBreaks)
$(parserTpl "parseQNB" False KeepLineBreaks)


-- With interpolation blocks (line-breaks and indentation are ignored)
qm :: String -> TH.ExpQ
qm = makeExpr . parseQM "" . clearIndentAtStart . filter (/= '\r')


-- No interpolation block (line-breaks and indentation are ignored)
qn :: String -> TH.ExpQ
qn = makeExpr . parseQN "" . clearIndentAtStart . filter (/= '\r')


-- With interpolation blocks (line-breaks are kept, indentation is ignored)
qmb :: String -> TH.ExpQ
qmb = makeExpr
    . parseQMB ""
    . clearFirstQXBLineBreak
    . clearIndentAtStart
    . filter (/= '\r')


-- No interpolation block (line-breaks are kept, indentation is ignored)
qnb :: String -> TH.ExpQ
qnb = makeExpr
    . parseQNB ""
    . clearFirstQXBLineBreak
    . clearIndentAtStart
    . filter (/= '\r')
