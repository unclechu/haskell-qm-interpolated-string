-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afE6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Text.InterpolatedString.QM.Internal.Parsers.TH (parserTpl) where

import           "base" Control.Arrow ((&&&))
import qualified "template-haskell" Language.Haskell.TH as TH
import           "template-haskell" Language.Haskell.TH ( Pat (ListP, ViewP)
                                                        , Exp (ListE)
                                                        )

-- local imports
import Text.InterpolatedString.QM.Internal.Parsers.Types (LineBreaks (..))


data Decl

  =  C (Bool, TH.Pat, TH.Exp)
  -- ^ 'C' means 'Conditional'.
  --   First value of tuple is condition to add pattern or not.

  |  D (TH.Pat, TH.Exp)
  -- ^ 'D' means 'Declarative'.
  --   A pattern always will be added.

  deriving (Show, Eq)


parserTpl :: String
          -- ^ Parser name
          -> Bool
          -- ^ Enable interpolation
          -> LineBreaks
          -> TH.DecsQ
parserTpl (TH.mkName &&& varE -> (n, fE)) withInterpolation lineBreaks = return

  [ TH.SigD n (TH.ConT $ TH.mkName "Parser") -- Type annotation for parser
  , TH.FunD n decls                          -- All patterns to match
  ]

  -- About naming variables:
  --   Suffixes (where `foo` is a variable name):
  --     * `fooP` means Pattern
  --     * `fooE` means Expression
  -- Variables:
  --   * `fE` - Quoter-parser's name (like `parseQM`), for recursive calls
  --   * `aE` - Result accumulator, defined for each pattern, like:
  --              `parseQM a …` where `…` is pattern and body
  --            For each from `decls` this prefix shown above
  --              is aproduced by `f` helper.
  --            For instance first pattern would be:
  --              `parseQM a [] = [Literal (reverse a)]`
  --   * `f` - Helper to prefix each pattern with `parseQM a`
  --           where `parseQM` is a name of parser from first argument.
  --  `apps` produces Expression,
  --    multiple function application with variate arity.
  where

    decls =

      map    (uncurry f) $
      map    (\x -> case x of C (_, y, z) -> (y, z) ; D y -> y   ) $
      filter (\x -> case x of C (y, _, _) -> y      ; D _ -> True)

      [ D ( ListP []
          , ListE [apps [conE "Literal", apps [varE "reverse", aE]]]
          )

      , C ( lineBreaks `elem` [KeepLineBreaks, ReplaceLineBreaksWithSpaces]
          , ViewP (varE "clearLastQXXLineBreak") $ conP "True" []
          , apps [fE, aE, strE ""]
          )

      , D ( consP [chrP '\\', chrP '\\', varP "xs"]
          , apps [fE, consE [chrE '\\', aE], varE "xs"]
          )

      , C ( withInterpolation
          , consP [chrP '\\', chrP '{', varP "xs"]
          , apps [fE, consE [chrE '{', aE], varE "xs"]
          )

      , D ( consP [chrP '\\', chrP ' ', varP "xs"]
          , apps [fE, consE [chrE ' ', aE], varE "xs"]
          )

      , C ( lineBreaks == IgnoreLineBreaks
          , consP [chrP '\\', chrP '\n', varP "xs"]
          , apps [fE, aE, consE [chrE '\n', varE "xs"]]
          )

        -- Explicitly cutting off line breaks
      , C ( lineBreaks `elem` [KeepLineBreaks, ReplaceLineBreaksWithSpaces]
          , consP [chrP '\\', chrP '\n', varP "xs"]

          , let cutOffFakeLnOrUseXS maybeVal =
                  apps [varE "maybe", varE "xs", varE "tail", maybeVal]

                clearNextLineIndentFromXS =
                  -- Fake '\n' here to make `clearIndentAtSOF` works for this
                  apps [varE "clearIndentAtSOF", consE [chrE '\n', varE "xs"]]

             in -- Recursively do stuff
                apps [fE, aE, cutOffFakeLnOrUseXS clearNextLineIndentFromXS]
          )

      , D ( consP [chrP '\\', chrP 'n', varP "xs"]
          , apps [fE, consE [chrE '\n', aE], varE "xs"]
          )

      , D ( consP [chrP '\\', chrP '\t', varP "xs"]
          , apps [fE, consE [chrE '\t', aE], varE "xs"]
          )

      , D ( consP [chrP '\\', chrP 't', varP "xs"]
          , apps [fE, consE [chrE '\t', aE], varE "xs"]
          )

      , D ( strP "\\"
          , apps [fE, consE [chrE '\\', aE], strE ""]
          )

      , C ( withInterpolation
          , consP [chrP '{', varP "xs"]
          , consE [ apps [conE "Literal", apps [varE "reverse", aE]]
                  , apps [varE "unQX", fE, strE "", varE "xs"]
                  ]
          )

      , D ( ViewP (varE "clearIndentAtSOF") $ conP "Just" [varP "clean"]
          , apps [fE, aE, varE "clean"]
          )

      , D ( ViewP (varE "clearIndentTillEOF") $ conP "Just" [varP "clean"]
          , apps [fE, aE, varE "clean"]
          )

        -- Cutting off line breaks
      , C ( lineBreaks == IgnoreLineBreaks
          , consP [chrP '\n', varP "xs"]
          , apps [fE, aE, varE "xs"]
          )

        -- Replacing line breaks with spaces
      , C ( lineBreaks == ReplaceLineBreaksWithSpaces
          , consP [chrP '\n', varP "xs"]
          , apps [fE, consE [chrE ' ', aE], varE "xs"]
          )

      , D ( consP [varP "x", varP "xs"]
          , apps [fE, consE [varE "x", aE], varE "xs"]
          )
      ]

    aE = varE "a"
    f pat body = TH.Clause [varP "a", pat] (TH.NormalB body) []

    apps [x] = x
    apps (x:y:zs) = apps $ TH.AppE x y : zs
    apps [] = error "apps []"

    consP [x] = x
    consP (x:y:zs) = consP $ TH.UInfixP x (TH.mkName ":") y : zs
    consP [] = error "consP []"

    consE [x] = x
    consE (x:y:zs) = consE $ TH.UInfixE x (conE ":") y : zs
    consE [] = error "consE []"


varP :: String ->             TH.Pat ; varP = TH.VarP . TH.mkName
varE :: String ->             TH.Exp ; varE = TH.VarE . TH.mkName
conP :: String -> [TH.Pat] -> TH.Pat ; conP = TH.ConP . TH.mkName
conE :: String ->             TH.Exp ; conE = TH.ConE . TH.mkName

chrP :: Char   -> TH.Pat ; chrP = TH.LitP . TH.CharL
chrE :: Char   -> TH.Exp ; chrE = TH.LitE . TH.CharL
strP :: String -> TH.Pat ; strP = TH.LitP . TH.StringL
strE :: String -> TH.Exp ; strE = TH.LitE . TH.StringL
