module Expressions
  ( Expr (..)
  , tryParse
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (skipSpaces, string)
import Text.Parsing.Parser.Token (letter)

data Expr
  = SymbolExpr String
  | NegExpr Expr
  | ImplExpr Expr Expr 
  | AndExpr Expr Expr
  | OrExpr Expr Expr

derive instance eqExpr :: Eq Expr
derive instance ordExpr :: Ord Expr
derive instance genExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show expr = showPrec 0 expr

showPrec :: Int -> Expr -> String
showPrec _ (SymbolExpr s) = s
showPrec _ (NegExpr expr) = 
  "~" <> showPrec negPrec expr
  where negPrec = 9
showPrec prec (AndExpr a b) = 
  wrap (prec > andPrec) (showPrec andPrec a <> " & " <> showPrec andPrec b)
  where andPrec = 7
showPrec prec (OrExpr a b) = 
  wrap (prec > orPrec) (showPrec orPrec a <> " | " <> showPrec orPrec b)
  where orPrec = 5
showPrec prec (ImplExpr a b) = 
  wrap (prec > implPrec) (showPrec (implPrec+1) a <> " => " <> showPrec implPrec b)
  where implPrec = 3

wrap :: Boolean -> String -> String
wrap true s = "(" <> s <> ")"
wrap false s = s

tryParse :: String -> Either ParseError Expr
tryParse s = runParser s exprParser

exprParser :: Parser String Expr
exprParser = skipSpaces *> fix go
  where 
    go cont = buildExprParser
      [ [ Prefix (string "~" <* skipSpaces $> NegExpr) ]
      , [ Infix (string "&" <* skipSpaces $> AndExpr) AssocRight ]
      , [ Infix (skipSpaces *> string "|" <* skipSpaces $> OrExpr) AssocRight ]
      , [ Infix (string "=>" <* skipSpaces $> ImplExpr) AssocRight ]
      ] 
      (between (string "(" <* skipSpaces) (string ")" <* skipSpaces) cont
      <|> SymbolExpr <<< fromCharArray <$> (many letter <* skipSpaces) 
      )