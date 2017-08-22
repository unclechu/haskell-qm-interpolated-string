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

module Text.InterpolatedString.QM.Parsers (qm, qn, qy, qv) where

import "base" GHC.Exts (IsString (fromString))

import qualified "template-haskell" Language.Haskell.TH as TH
import "haskell-src-meta" Language.Haskell.Meta.Parse (parseExp)

#if MIN_VERSION_base(4,8,0)
#else
import "base" Data.Monoid (mempty, mappend)
#endif

-- local imports
import Text.InterpolatedString.QM.ShowQ.Class (ShowQ(..))


class QQ a string where
  toQQ :: a -> string

instance IsString s => QQ s s where
  toQQ = id

instance (ShowQ a, IsString s) => QQ a s where
  toQQ = fromString . showQ

data StringPart = Literal String | AntiQuote String deriving Show

type Parser = String -> String -> [StringPart]


unQX :: Parser -> Parser
unQX _ a ""          = [Literal (reverse a)]
unQX p a ('\\':x:xs) = unQX p (x:a) xs
unQX p a ("\\")      = unQX p ('\\':a) ""
unQX p a ('}':xs)    = AntiQuote (reverse a) : p "" xs
unQX p a (x:xs)      = unQX p (x:a) xs


parseQM :: Parser
parseQM a ""             = [Literal (reverse a)]
parseQM a ('\\':'\\':xs) = parseQM ('\\':a) xs
parseQM a ('\\':'{':xs)  = parseQM ('{':a) xs
parseQM a ('\\':' ':xs)  = parseQM (' ':a) xs
parseQM a ('\\':'\n':xs) = parseQM a ('\n':xs)
parseQM a ('\\':'n':xs)  = parseQM ('\n':a) xs
parseQM a ('\\':'\t':xs) = parseQM ('\t':a) xs
parseQM a ('\\':'t':xs)  = parseQM ('\t':a) xs
parseQM a ("\\")         = parseQM ('\\':a) ""
parseQM a ('{':xs)       = Literal (reverse a) : (unQX parseQM) "" xs
parseQM a (clearIndentAtSOF   -> Just clean) = parseQM a clean
parseQM a (clearIndentTillEOF -> Just clean) = parseQM a clean
parseQM a ('\n':xs)      = parseQM a xs -- cut off line breaks
parseQM a (x:xs)         = parseQM (x:a) xs

qm :: String -> TH.ExpQ
qm = makeExpr . parseQM "" . clearIndentAtStart . filter (/= '\r')


parseQN :: Parser
parseQN a ""             = [Literal (reverse a)]
parseQN a ('\\':'\\':xs) = parseQN ('\\':a) xs
parseQN a ('\\':' ':xs)  = parseQN (' ':a) xs
parseQN a ('\\':'\n':xs) = parseQN a ('\n':xs)
parseQN a ('\\':'n':xs)  = parseQN ('\n':a) xs
parseQN a ('\\':'\t':xs) = parseQN ('\t':a) xs
parseQN a ('\\':'t':xs)  = parseQN ('\t':a) xs
parseQN a ("\\")         = parseQN ('\\':a) ""
parseQN a (clearIndentAtSOF   -> Just clean) = parseQN a clean
parseQN a (clearIndentTillEOF -> Just clean) = parseQN a clean
parseQN a ('\n':xs)      = parseQN a xs -- cut off line breaks
parseQN a (x:xs)         = parseQN (x:a) xs

qn :: String -> TH.ExpQ
qn = makeExpr . parseQN "" . clearIndentAtStart . filter (/= '\r')


parseQY :: Parser
parseQY _ _ = []

qy :: String -> TH.ExpQ
qy = makeExpr . parseQY "" . clearIndentAtStart . filter (/= '\r')


parseQV :: Parser
parseQV _ _ = []

qv :: String -> TH.ExpQ
qv = makeExpr . parseQV "" . clearIndentAtStart . filter (/= '\r')


clearIndentTillEOF :: String -> Maybe String
clearIndentTillEOF s | s == ""             = Nothing
                     | head s `elem` "\t " = cutOff s
                     | otherwise           = Nothing

  where cutOff x | x == ""             = Just ""
                 | head x == '\n'      = Just x
                 | head x `elem` "\t " = cutOff $ tail x
                 | otherwise           = Nothing


clearIndentAtSOF :: String -> Maybe String
clearIndentAtSOF s | s == ""                      = Nothing
                   | head s == '\n' && hasChanges = Just processed
                   | otherwise                    = Nothing

  where processed  = '\n' : cutOff (tail s)
        hasChanges = processed /= s

        cutOff x | x == ""             = ""
                 | head x `elem` "\t " = cutOff $ tail x
                 | otherwise           = x


clearIndentAtStart :: String -> String
clearIndentAtStart s | s == ""             = ""
                     | head s `elem` "\t " = clearIndentAtStart $ tail s
                     | otherwise           = s


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
