{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.InterpolatedString.QM.Internal.Parsers.Helpers
  ( unQX
  , clearIndentAtStart
  , clearIndentAtSOF
  , clearIndentTillEOF
  , clearFirstQXXLineBreak
  , clearLastQXXLineBreak
  , makeExpr
  ) where

import "base" GHC.Exts (IsString (fromString))
import "haskell-src-meta" Language.Haskell.Meta.Parse (parseExp)
import qualified "template-haskell" Language.Haskell.TH as TH

#if !MIN_VERSION_base(4,8,0)
import "base" Data.Monoid (mempty, mappend)
#endif

-- local imports

import Text.InterpolatedString.QM.ShowQ.Class (ShowQ (..))

import Text.InterpolatedString.QM.Internal.Parsers.Types ( Parser
                                                         , StringPart (..)
                                                         )


class    QQ a string                     where toQQ :: a -> string
instance IsString s => QQ s s            where toQQ = id
instance (ShowQ a, IsString s) => QQ a s where toQQ = fromString . showQ


-- Parser for interpolation block
unQX :: Parser -> Parser
unQX _ a ""            = [Literal (reverse a)] -- Error, block isn't closed
unQX f a ('\\':'}':xs) = unQX f ('}':a) xs
unQX f a ('}':xs)      = AntiQuote (reverse a) : f "" xs
unQX f a (x:xs)        = unQX f (x:a) xs


clearIndentAtSOF :: String -> Maybe String
clearIndentAtSOF "" = Nothing
clearIndentAtSOF ('\r':'\n':xs) = clearIndentAtSOF $ '\n' : xs
clearIndentAtSOF s@(x:xs) | x == '\n' && hasChanges = Just processed
                          | otherwise               = Nothing

  where processed  = '\n' : cutOff xs
        hasChanges = processed /= s

        cutOff "" = ""
        cutOff z@(y:ys) | y `elem` "\t " = cutOff ys
                        | otherwise      = z


clearIndentTillEOF :: String -> Maybe String
clearIndentTillEOF "" = Nothing
clearIndentTillEOF s@(x:_) | x `elem` "\t " = cutOff s
                           | otherwise      = Nothing

  where cutOff "" = Just ""
        cutOff ('\r':'\n':xs) = cutOff $ '\n' : xs
        cutOff z@('\n':_) = Just z
        cutOff (y:ys) | y `elem` "\t " = cutOff ys
                      | otherwise      = Nothing


clearLastQXXLineBreak :: String -> Bool
-- Cannot really be empty (matched in `parseQMB`)
clearLastQXXLineBreak "" = False
clearLastQXXLineBreak ('\r':'\n':xs) = clearLastQXXLineBreak $ '\n' : xs
clearLastQXXLineBreak (x:xs) | x `elem` "\t \n" = f xs
                             | otherwise        = False

  where f "" = True
        f ('\r':'\n':ys) = f $ '\n' : ys
        f (y:ys) | y `elem` "\t \n" = f ys
                 | otherwise        = False


clearFirstQXXLineBreak :: String -> String
clearFirstQXXLineBreak "" = ""
clearFirstQXXLineBreak ('\r':'\n':xs) = clearFirstQXXLineBreak $ '\n' : xs
clearFirstQXXLineBreak s@(x:xs) | x `elem` "\t \n" = cutOff xs
                                | otherwise        = s

  where cutOff "" = ""
        cutOff ('\r':'\n':ys) = cutOff $ '\n' : ys
        cutOff c@(y:ys) | y `elem` "\t \n" = cutOff ys
                        | otherwise        = c


clearIndentAtStart :: String -> String
clearIndentAtStart "" = ""
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
#if MIN_VERSION_template_haskell(2,8,0)
                       Left  err  -> TH.reportError err >> [| mempty |]
#else
                       Left  err  -> TH.report True err >> [| mempty |]
#endif
                       Right expr -> return expr
