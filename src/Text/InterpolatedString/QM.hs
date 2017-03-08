-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE CPP #-}

module Text.InterpolatedString.QM (qm, ShowQ(..)) where

import "base" GHC.Exts (IsString(..))
import qualified "template-haskell" Language.Haskell.TH as TH
import "template-haskell" Language.Haskell.TH.Quote
import "haskell-src-meta" Language.Haskell.Meta.Parse
import "bytestring" Data.ByteString.Char8 as Strict (ByteString, unpack)
import "bytestring" Data.ByteString.Lazy.Char8 as Lazy (ByteString, unpack)
import "text" Data.Text as T (Text, unpack)
import "text" Data.Text.Lazy as LazyT (Text, unpack)

#if MIN_VERSION_base(4,8,0)
#else
import "base" Data.Monoid (mempty, mappend)
#endif


class ShowQ a where
  showQ :: a -> String

instance ShowQ Char where
  showQ = (:[])

instance ShowQ String where
  showQ = id

instance ShowQ Strict.ByteString where
  showQ = Strict.unpack

instance ShowQ Lazy.ByteString where
  showQ = Lazy.unpack

instance ShowQ T.Text where
  showQ = T.unpack

instance ShowQ LazyT.Text where
  showQ = LazyT.unpack

instance Show a => ShowQ a where
  showQ = show

class QQ a string where
  toQQ :: a -> string

instance IsString s => QQ s s where
  toQQ = id

instance (ShowQ a, IsString s) => QQ a s where
  toQQ = fromString . showQ

data StringPart = Literal String | AntiQuote String deriving Show


unQM :: String -> String -> [StringPart]
unQM a []          = [Literal (reverse a)]
unQM a ('\\':x:xs) = unQM (x:a) xs
unQM a ("\\")      = unQM ('\\':a) []
unQM a ('}':xs)    = AntiQuote (reverse a) : parseQM [] xs
unQM a (x:xs)      = unQM (x:a) xs

parseQM :: String -> String -> [StringPart]
parseQM a []             = [Literal (reverse a)]
parseQM a ('\\':'\\':xs) = parseQM ('\\':a) xs
parseQM a ('\\':'{':xs)  = parseQM ('{':a) xs
parseQM a ('\\':' ':xs)  = parseQM (' ':a) xs
parseQM a ('\\':'\n':xs) = parseQM a ('\n':xs)
parseQM a ('\\':'n':xs)  = parseQM ('\n':a) xs
parseQM a ("\\")         = parseQM ('\\':a) []
parseQM a ('{':xs)       = Literal (reverse a) : unQM [] xs
parseQM a (clearIndentAtSOF   -> Just clean) = parseQM a clean
parseQM a (clearIndentTillEOF -> Just clean) = parseQM a clean
parseQM a ('\n':xs)      = parseQM a xs -- cut off line breaks
parseQM a (x:xs)         = parseQM (x:a) xs

clearIndentTillEOF :: String -> Maybe String
clearIndentTillEOF str@((ifMaybe (`elem` "\t ") -> Just _) : _) = cutOff str
  where cutOff :: String -> Maybe String
        cutOff ""            = Just ""
        cutOff eof@('\n':_) = Just eof
        cutOff ((ifMaybe (`elem` "\t ") -> Just _) : xs) = cutOff xs
        cutOff _ = Nothing
clearIndentTillEOF _ = Nothing

clearIndentAtSOF :: String -> Maybe String
clearIndentAtSOF ('\n' : xs) = if result /= xs
                                  then Just $ '\n' : cutOff xs
                                  else Nothing
  where cutOff :: String -> String
        cutOff ((ifMaybe (`elem` "\t ") -> Just _) : ys) = cutOff ys
        cutOff s = s
        result = cutOff xs
clearIndentAtSOF _ = Nothing

clearIndentAtStart :: String -> String
clearIndentAtStart ((ifMaybe (`elem` "\t ") -> Just _) : xs) =
  clearIndentAtStart xs
clearIndentAtStart s = s


makeExpr :: [StringPart] -> TH.ExpQ
makeExpr [] = [| mempty |]
makeExpr (Literal a : xs) =
  TH.appE [| mappend (fromString a) |]    $ makeExpr xs
makeExpr (AntiQuote a : xs) =
  TH.appE [| mappend (toQQ $(reify a)) |] $ makeExpr xs

reify :: String -> TH.Q TH.Exp
reify s =
  case parseExp s of
       Left  e -> TH.reportError e >> [| mempty |]
       Right e -> return e


qm :: QuasiQuoter
qm = QuasiQuoter f
  (error "Cannot use qm as a pattern")
  (error "Cannot use qm as a type")
  (error "Cannot use qm as a dec")
  where f = makeExpr . parseQM [] . clearIndentAtStart . filter (/= '\r')


ifMaybe :: (a -> Bool) -> a -> Maybe a
ifMaybe f x = if f x then Just x else Nothing
