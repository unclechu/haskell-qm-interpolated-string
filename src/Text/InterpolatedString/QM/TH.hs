-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Text.InterpolatedString.QM.TH
  ( StringPart (..)
  , Parser
  , tplParseQM
  , unQX
  , clearIndentAtSOF
  , clearIndentTillEOF
  ) where

import qualified "template-haskell" Language.Haskell.TH as TH
import "base" Data.Maybe (catMaybes)


data StringPart = Literal String | AntiQuote String deriving Show

type Parser = String -> String -> [StringPart]


tplParseQM :: String
           -- ^ Parser name
           -> Bool
           -- ^ Enable interpolation
           -> TH.DecsQ
tplParseQM (TH.mkName -> n) withInterpolation = pure

  [ TH.SigD n (TH.ConT $ TH.mkName "Parser")
  , TH.FunD n (catMaybes d)
  ]

  where d = [ fu (lp [])
                 (le [apps [ce "Literal", apps [ve "reverse", av]]])

            , fu (consP [chrP '\\', chrP '\\', vp "xs"])
                 (apps [fe, consE [chrE '\\', av], ve "xs"])

            , fi withInterpolation
                 (consP [chrP '\\', chrP '{', vp "xs"])
                 (apps [fe, consE [chrE '{', av], ve "xs"])

            , fu (consP [chrP '\\', chrP ' ', vp "xs"])
                 (apps [fe, consE [chrE ' ', av], ve "xs"])

            , fu (consP [chrP '\\', chrP '\n', vp "xs"])
                 (apps [fe, av, consE [chrE '\n', ve "xs"]])

            , fu (consP [chrP '\\', chrP 'n', vp "xs"])
                 (apps [fe, consE [chrE '\n', av], ve "xs"])

            , fu (consP [chrP '\\', chrP '\t', vp "xs"])
                 (apps [fe, consE [chrE '\t', av], ve "xs"])

            , fu (consP [chrP '\\', chrP 't', vp "xs"])
                 (apps [fe, consE [chrE '\t', av], ve "xs"])

            , fu (strP "\\")
                 (apps [fe, consE [chrE '\\', av], strE ""])

            , fi withInterpolation
                 (consP [chrP '{', vp "xs"])
                 (consE [ apps [ce "Literal", apps [ve "reverse", av]]
                        , apps [ve "unQX", fe, strE "", ve "xs"]
                        ])

            , fu (TH.ViewP (ve "clearIndentAtSOF") $ cp "Just" [vp "clean"])
                 (apps [fe, av, ve "clean"])

            , fu (TH.ViewP (ve "clearIndentTillEOF") $ cp "Just" [vp "clean"])
                 (apps [fe, av, ve "clean"])

              -- Cut off line-breaks
            , fu (consP [chrP '\n', vp "xs"])
                 (apps [fe, av, ve "xs"])

            , fu (consP [vp "x", vp "xs"])
                 (apps [fe, consE [ve "x", av], ve "xs"])
            ]

        fe = TH.VarE n
        a = TH.mkName "a" ; ap = TH.VarP a ; av = TH.VarE a

        vp = TH.VarP . TH.mkName ; ve = TH.VarE . TH.mkName
        cp = TH.ConP . TH.mkName ; ce = TH.ConE . TH.mkName
        lp = TH.ListP            ; le = TH.ListE

        chrP = TH.LitP . TH.CharL   ; chrE = TH.LitE . TH.CharL
        strP = TH.LitP . TH.StringL ; strE = TH.LitE . TH.StringL

        fu pat body = Just $ TH.Clause (ap : pat : []) (TH.NormalB body) []
        fi cond pat body = if cond then fu pat body else Nothing

        apps [x] = x
        apps (x:y:zs) = apps $ TH.AppE x y : zs
        apps [] = error "apps []"

        consP [x] = x
        consP (x:y:zs) = consP $ TH.UInfixP x (TH.mkName ":") y : zs
        consP [] = error "consP []"

        consE [x] = x
        consE (x:y:zs) = consE $ TH.UInfixE x (ce ":") y : zs
        consE [] = error "consE []"


-- Parser for interpolation block
unQX :: Parser -> Parser
unQX _ a ""          = [Literal (reverse a)]
unQX p a ('\\':x:xs) = unQX p (x:a) xs
unQX p a ("\\")      = unQX p ('\\':a) ""
unQX p a ('}':xs)    = AntiQuote (reverse a) : p "" xs
unQX p a (x:xs)      = unQX p (x:a) xs


clearIndentAtSOF :: String -> Maybe String
clearIndentAtSOF ""                                 = Nothing
clearIndentAtSOF s@(x:xs) | x == '\n' && hasChanges = Just processed
                          | otherwise               = Nothing

  where processed  = '\n' : cutOff xs
        hasChanges = processed /= s

        cutOff ""                        = ""
        cutOff c@(y:ys) | y `elem` "\t " = cutOff ys
                        | otherwise      = c


clearIndentTillEOF :: String -> Maybe String
clearIndentTillEOF ""                       = Nothing
clearIndentTillEOF s@(x:_) | x `elem` "\t " = cutOff s
                           | otherwise      = Nothing

  where cutOff ""                      = Just ""
        cutOff c@('\n':_)              = Just c
        cutOff (y:ys) | y `elem` "\t " = cutOff ys
                      | otherwise      = Nothing
