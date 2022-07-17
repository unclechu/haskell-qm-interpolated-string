module Text.InterpolatedString.QM.Internal.Parsers.Types
  ( Parser
  , StringPart (..)
  , LineBreaks (..)
  ) where


type Parser = String -> String -> [StringPart]

data LineBreaks
  = IgnoreLineBreaks
  | KeepLineBreaks
  | ReplaceLineBreaksWithSpaces
  deriving (Show, Eq)

data StringPart = Literal String | AntiQuote String deriving Show
