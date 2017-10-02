-- Fork of: https://github.com/audreyt/interpolatedstring-perl6/blob/63d91a83eb5e48740c87570a8c7fd4668afe6832/src/Text/InterpolatedString/Perl6.hs
-- Author of the 'interpolatedstring-perl6' package: Audrey Tang

module Text.InterpolatedString.QM.Parsers.Types
  ( Parser
  , StringPart (..)
  , LineBreaks (..)
  ) where


type Parser = String -> String -> [StringPart]

data LineBreaks
  = IgnoreLineBreaks
  | KeepLineBreaks
  | ReplaceLineBreaksWithSpaces -- TODO implement
  deriving (Show, Eq)

data StringPart = Literal String | AntiQuote String deriving Show
