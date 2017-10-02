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
import "base" Control.Arrow ((&&&))


data Decl
  = Decl
  { c :: Bool   -- (c)ondition
  , p :: TH.Pat -- (p)attern
  , e :: TH.Exp -- (e)xpression
  }
  deriving (Show, Eq)


data StringPart = Literal String | AntiQuote String deriving Show

type Parser = String -> String -> [StringPart]


tplParseQM :: String
           -- ^ Parser name
           -> Bool
           -- ^ Enable interpolation
           -> TH.DecsQ
tplParseQM (TH.mkName -> n) withInterpolation = return

  [ TH.SigD n (TH.ConT $ TH.mkName "Parser")
  , TH.FunD n $ map (uncurry f . (p &&& e)) decls
  ]

  where decls = filter c
          [ d { p = lp []
              , e = le [apps [ce "Literal", apps [ve "reverse", av]]]
              }

          , d { p = consP [chrP '\\', chrP '\\', vp "xs"]
              , e = apps [fe, consE [chrE '\\', av], ve "xs"]
              }

          , d { c = withInterpolation
              , p = consP [chrP '\\', chrP '{', vp "xs"]
              , e = apps [fe, consE [chrE '{', av], ve "xs"]
              }

          , d { p = consP [chrP '\\', chrP ' ', vp "xs"]
              , e = apps [fe, consE [chrE ' ', av], ve "xs"]
              }

          , d { p = consP [chrP '\\', chrP '\n', vp "xs"]
              , e = apps [fe, av, consE [chrE '\n', ve "xs"]]
              }

          , d { p = consP [chrP '\\', chrP 'n', vp "xs"]
              , e = apps [fe, consE [chrE '\n', av], ve "xs"]
              }

          , d { p = consP [chrP '\\', chrP '\t', vp "xs"]
              , e = apps [fe, consE [chrE '\t', av], ve "xs"]
              }

          , d { p = consP [chrP '\\', chrP 't', vp "xs"]
              , e = apps [fe, consE [chrE '\t', av], ve "xs"]
              }

          , d { p = strP "\\"
              , e = apps [fe, consE [chrE '\\', av], strE ""]
              }

          , d { c = withInterpolation
              , p = consP [chrP '{', vp "xs"]
              , e = consE [ apps [ce "Literal", apps [ve "reverse", av]]
                          , apps [ve "unQX", fe, strE "", ve "xs"]
                          ]
              }

          , d { p = TH.ViewP (ve "clearIndentAtSOF") $ cp "Just" [vp "clean"]
              , e = apps [fe, av, ve "clean"]
              }

          , d { p = TH.ViewP (ve "clearIndentTillEOF") $ cp "Just" [vp "clean"]
              , e = apps [fe, av, ve "clean"]
              }

            -- Cutting off line-breaks
          , d { p = consP [chrP '\n', vp "xs"]
              , e = apps [fe, av, ve "xs"]
              }

          , d { p = consP [vp "x", vp "xs"]
              , e = apps [fe, consE [ve "x", av], ve "xs"]
              }
          ]

        fe = TH.VarE n
        a = TH.mkName "a" ; ap = TH.VarP a ; av = TH.VarE a

        vp = TH.VarP . TH.mkName ; ve = TH.VarE . TH.mkName
        cp = TH.ConP . TH.mkName ; ce = TH.ConE . TH.mkName
        lp = TH.ListP            ; le = TH.ListE

        chrP = TH.LitP . TH.CharL   ; chrE = TH.LitE . TH.CharL
        strP = TH.LitP . TH.StringL ; strE = TH.LitE . TH.StringL

        f pat body = TH.Clause (ap : pat : []) (TH.NormalB body) []

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


-- Default value
d :: Decl
d = Decl { c = True, p = TH.WildP, e = TH.VarE $ TH.mkName "_" }
