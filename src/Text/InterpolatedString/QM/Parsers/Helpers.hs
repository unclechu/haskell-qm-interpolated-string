-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.InterpolatedString.QM.Parsers.Helpers
  ( unQX
  , clearIndentAtStart
  , clearIndentAtSOF
  , clearIndentTillEOF
  , clearFirstQXBLineBreak
  , clearLastQXBLineBreak
  , makeExpr
  ) where

import "base" GHC.Exts (IsString (fromString))
import "haskell-src-meta" Language.Haskell.Meta.Parse (parseExp)
import qualified "template-haskell" Language.Haskell.TH as TH

#if MIN_VERSION_base(4,8,0)
#else
import "base" Data.Monoid (mempty, mappend)
#endif

-- local imports
import Text.InterpolatedString.QM.ShowQ.Class (ShowQ (..))
import Text.InterpolatedString.QM.Parsers.Types (Parser, StringPart (..))


class QQ a string where
  toQQ :: a -> string

instance IsString s => QQ s s where
  toQQ = id

instance (ShowQ a, IsString s) => QQ a s where
  toQQ = fromString . showQ


-- Parser for interpolation block
unQX :: Parser -> Parser
unQX _ a ""          = [Literal (reverse a)]
unQX f a ('\\':x:xs) = unQX f (x:a) xs
unQX f a ("\\")      = unQX f ('\\':a) ""
unQX f a ('}':xs)    = AntiQuote (reverse a) : f "" xs
unQX f a (x:xs)      = unQX f (x:a) xs


clearIndentAtSOF :: String -> Maybe String
clearIndentAtSOF ""                                 = Nothing
clearIndentAtSOF s@(x:xs) | x == '\n' && hasChanges = Just processed
                          | otherwise               = Nothing

  where processed  = '\n' : cutOff xs
        hasChanges = processed /= s

        cutOff ""                        = ""
        cutOff z@(y:ys) | y `elem` "\t " = cutOff ys
                        | otherwise      = z


clearIndentTillEOF :: String -> Maybe String
clearIndentTillEOF ""                       = Nothing
clearIndentTillEOF s@(x:_) | x `elem` "\t " = cutOff s
                           | otherwise      = Nothing

  where cutOff ""                      = Just ""
        cutOff z@('\n':_)              = Just z
        cutOff (y:ys) | y `elem` "\t " = cutOff ys
                      | otherwise      = Nothing


clearLastQXBLineBreak :: String -> Bool
-- Cannot be empty (matched in `parseQMB`)
clearLastQXBLineBreak ""                        = False
clearLastQXBLineBreak (x:xs) | x `elem` "\t\n " = f xs
                             | otherwise        = False

  where f ""                        = True
        f (y:ys) | y `elem` "\t\n " = f ys
                 | otherwise        = False


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
