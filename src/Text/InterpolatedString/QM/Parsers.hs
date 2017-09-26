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

-- With interpolation blocks (line-breaks and indentation are ignored)
qm :: String -> TH.ExpQ
qm = makeExpr . parseQM "" . clearIndentAtStart . filter (/= '\r')


-- Copy-pasted `parseQM` and removed lines where `{` or `}` are parsed
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

-- No interpolation block (line-breaks and indentation are ignored)
qn :: String -> TH.ExpQ
qn = makeExpr . parseQN "" . clearIndentAtStart . filter (/= '\r')


parseQMB :: Parser
parseQMB a ""             = [Literal (reverse a)]
parseQMB a (clearLastQXBLineBreak -> True) = parseQMB a ""
parseQMB a ('\\':'\\':xs) = parseQMB ('\\':a) xs
parseQMB a ('\\':'{':xs)  = parseQMB ('{':a) xs
parseQMB a ('\\':' ':xs)  = parseQMB (' ':a) xs
parseQMB a ('\\':'\n':xs) = -- explicitly slicing line-breaks
                            parseQMB a $ maybe xs tail
                                       $ clearIndentAtSOF $ '\n' : xs
parseQMB a ('\\':'n':xs)  = parseQMB ('\n':a) xs
parseQMB a ('\\':'\t':xs) = parseQMB ('\t':a) xs
parseQMB a ('\\':'t':xs)  = parseQMB ('\t':a) xs
parseQMB a ("\\")         = parseQMB ('\\':a) ""
parseQMB a ('{':xs)       = Literal (reverse a) : (unQX parseQMB) "" xs
parseQMB a (clearIndentAtSOF   -> Just clean) = parseQMB a clean
parseQMB a (clearIndentTillEOF -> Just clean) = parseQMB a clean
parseQMB a (x:xs)         = parseQMB (x:a) xs

-- With interpolation blocks (line-breaks are kept, indentation is ignored)
qmb :: String -> TH.ExpQ
qmb = makeExpr
    . parseQMB ""
    . clearFirstQXBLineBreak
    . clearIndentAtStart
    . filter (/= '\r')


-- Copy-pasted `parseQMB` and removed lines where `{` or `}` are parsed
parseQNB :: Parser
parseQNB a ""             = [Literal (reverse a)]
parseQNB a (clearLastQXBLineBreak -> True) = parseQNB a ""
parseQNB a ('\\':'\\':xs) = parseQNB ('\\':a) xs
parseQNB a ('\\':' ':xs)  = parseQNB (' ':a) xs
parseQNB a ('\\':'\n':xs) = -- explicitly slicing line-breaks
                            parseQNB a $ maybe xs tail
                                       $ clearIndentAtSOF $ '\n' : xs
parseQNB a ('\\':'n':xs)  = parseQNB ('\n':a) xs
parseQNB a ('\\':'\t':xs) = parseQNB ('\t':a) xs
parseQNB a ('\\':'t':xs)  = parseQNB ('\t':a) xs
parseQNB a ("\\")         = parseQNB ('\\':a) ""
parseQNB a (clearIndentAtSOF   -> Just clean) = parseQNB a clean
parseQNB a (clearIndentTillEOF -> Just clean) = parseQNB a clean
parseQNB a (x:xs)         = parseQNB (x:a) xs

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

clearLastQXBLineBreak :: String -> Bool
-- Cannot be empty (matched in `parseQMB`)
clearLastQXBLineBreak ""                        = False
clearLastQXBLineBreak (x:xs) | x `elem` "\t\n " = f xs
                             | otherwise        = False
  where f ""                        = True
        f (y:ys) | y `elem` "\t\n " = f ys
                 | otherwise        = False


clearIndentTillEOF :: String -> Maybe String
clearIndentTillEOF ""                       = Nothing
clearIndentTillEOF s@(x:_) | x `elem` "\t " = cutOff s
                           | otherwise      = Nothing

  where cutOff ""                      = Just ""
        cutOff c@('\n':_)              = Just c
        cutOff (y:ys) | y `elem` "\t " = cutOff ys
                      | otherwise      = Nothing


clearIndentAtSOF :: String -> Maybe String
clearIndentAtSOF ""                                 = Nothing
clearIndentAtSOF s@(x:xs) | x == '\n' && hasChanges = Just processed
                          | otherwise               = Nothing

  where processed  = '\n' : cutOff xs
        hasChanges = processed /= s

        cutOff ""                        = ""
        cutOff c@(y:ys) | y `elem` "\t " = cutOff ys
                        | otherwise      = c


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
