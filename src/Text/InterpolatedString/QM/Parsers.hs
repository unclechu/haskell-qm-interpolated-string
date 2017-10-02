-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE CPP #-}

module Text.InterpolatedString.QM.Parsers (qm, qn, qmb, qnb) where

import "base" GHC.Exts (IsString (fromString))

import qualified "template-haskell" Language.Haskell.TH as TH
import "haskell-src-meta" Language.Haskell.Meta.Parse (parseExp)

#if MIN_VERSION_base(4,8,0)
#else
import "base" Data.Monoid (mempty, mappend)
#endif

-- local imports
import Text.InterpolatedString.QM.ShowQ.Class (ShowQ (..))
import Text.InterpolatedString.QM.TH ( StringPart (..)
                                     , LineBreaks (..)
                                     , parserTpl

                                       -- Required for generated code
                                     , Parser
                                     , unQX
                                     , clearIndentAtSOF
                                     , clearIndentTillEOF
                                     , clearLastQXBLineBreak
                                     )

class QQ a string where
  toQQ :: a -> string

instance IsString s => QQ s s where
  toQQ = id

instance (ShowQ a, IsString s) => QQ a s where
  toQQ = fromString . showQ


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


clearFirstQXBLineBreak :: String -> String
clearFirstQXBLineBreak ""                          = ""
clearFirstQXBLineBreak s@(x:xs) | x `elem` "\t\n " = cutOff xs
                                | otherwise        = s
  where cutOff ""                          = ""
        cutOff c@(y:ys) | y `elem` "\t\n " = cutOff ys
                        | otherwise        = c


clearIndentAtStart :: String -> String
clearIndentAtStart ""                        = ""
clearIndentAtStart s@(x:xs) | x `elem` "\t " = clearIndentAtStart xs
                            | otherwise      = s


makeExpr :: [StringPart] -> TH.ExpQ
makeExpr [] = [| mempty |]
makeExpr (Literal a : xs) =
  TH.appE [| mappend (fromString a) |]    $ makeExpr xs
makeExpr (AntiQuote a : xs) =
  TH.appE [| mappend (toQQ $(reify a)) |] $ makeExpr xs
  where reify :: String -> TH.Q TH.Exp
        reify s = case parseExp s of
                       Left  e -> TH.reportError e >> [| mempty |]
                       Right e -> return e
