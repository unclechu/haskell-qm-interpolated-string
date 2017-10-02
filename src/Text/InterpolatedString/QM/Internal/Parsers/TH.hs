-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Text.InterpolatedString.QM.Internal.Parsers.TH (parserTpl) where

import qualified "template-haskell" Language.Haskell.TH as TH

-- local imports
import Text.InterpolatedString.QM.Internal.Parsers.Types (LineBreaks (..))


data Decl
  = C (Bool, TH.Pat, TH.Exp)
  | D (TH.Pat, TH.Exp)
  deriving (Show, Eq)


parserTpl :: String
          -- ^ Parser name
          -> Bool
          -- ^ Enable interpolation
          -> LineBreaks
          -> TH.DecsQ
parserTpl (TH.mkName -> n) withInterpolation lineBreaks = return

  [ TH.SigD n (TH.ConT $ TH.mkName "Parser")
  , TH.FunD n decls
  ]

  where

    decls =

      map    (uncurry f) $
      map    (\case C (_, x, y) -> (x, y) ; D x -> x   ) $
      filter (\case C (x, _, _) -> x      ; D _ -> True)

      [ D ( lp []
          , le [apps [ce "Literal", apps [ve "reverse", ae]]]
          )

      , C ( lineBreaks == KeepLineBreaks
          , TH.ViewP (ve "clearLastQXBLineBreak") $ cp "True" []
          , apps [fe, ae, strE ""]
          )

      , D ( consP [chrP '\\', chrP '\\', vp "xs"]
          , apps [fe, consE [chrE '\\', ae], ve "xs"]
          )

      , C ( withInterpolation
          , consP [chrP '\\', chrP '{', vp "xs"]
          , apps [fe, consE [chrE '{', ae], ve "xs"]
          )

      , D ( consP [chrP '\\', chrP ' ', vp "xs"]
          , apps [fe, consE [chrE ' ', ae], ve "xs"]
          )

      , C ( lineBreaks == IgnoreLineBreaks
          , consP [chrP '\\', chrP '\n', vp "xs"]
          , apps [fe, ae, consE [chrE '\n', ve "xs"]]
          )

        -- Explicitly slicing line-breaks
      , C ( lineBreaks == KeepLineBreaks
          , consP [chrP '\\', chrP '\n', vp "xs"]
          , apps [apps [fe, ae, apps [ ve "maybe", ve "xs", ve "tail"
                                     , apps [ ve "clearIndentAtSOF"
                                            , consE [chrE '\n', ve "xs"]
                                            ]]]]
          )

      , D ( consP [chrP '\\', chrP 'n', vp "xs"]
          , apps [fe, consE [chrE '\n', ae], ve "xs"]
          )

      , D ( consP [chrP '\\', chrP '\t', vp "xs"]
          , apps [fe, consE [chrE '\t', ae], ve "xs"]
          )

      , D ( consP [chrP '\\', chrP 't', vp "xs"]
          , apps [fe, consE [chrE '\t', ae], ve "xs"]
          )

      , D ( strP "\\"
          , apps [fe, consE [chrE '\\', ae], strE ""]
          )

      , C ( withInterpolation
          , consP [chrP '{', vp "xs"]
          , consE [ apps [ce "Literal", apps [ve "reverse", ae]]
                  , apps [ve "unQX", fe, strE "", ve "xs"]
                  ]
          )

      , D ( TH.ViewP (ve "clearIndentAtSOF") $ cp "Just" [vp "clean"]
          , apps [fe, ae, ve "clean"]
          )

      , D ( TH.ViewP (ve "clearIndentTillEOF") $ cp "Just" [vp "clean"]
          , apps [fe, ae, ve "clean"]
          )

        -- Cutting off line-breaks
      , C ( lineBreaks == IgnoreLineBreaks
          , consP [chrP '\n', vp "xs"]
          , apps [fe, ae, ve "xs"]
          )

      , D ( consP [vp "x", vp "xs"]
          , apps [fe, consE [ve "x", ae], ve "xs"]
          )
      ]

    fe = TH.VarE n
    a = TH.mkName "a" ; ap = TH.VarP a ; ae = TH.VarE a

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
