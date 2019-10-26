module Data.Expressions
  ( Expr (..)
  , tryParse
  , RenderConfig
  , render
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (some)
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
  | BiCondExpr Expr Expr

derive instance eqExpr :: Eq Expr
derive instance ordExpr :: Ord Expr
derive instance genExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show = render stringConfig

stringConfig :: RenderConfig String String
stringConfig =
  { wrap: identity
  , and: \a b -> a <> " & " <> b
  , or: \a b -> a <> " | " <> b
  , impl: \a b -> a <> " => " <> b
  , biCond: \a b -> a <> " <=> " <> b
  , not: \a -> "~" <> a
  , inBraces: \a -> "(" <> a <> ")"
  , renderString: identity
  }

type RenderConfig a out =
  { wrap :: a -> out
  , and :: a -> a -> a
  , or :: a -> a -> a
  , impl :: a -> a -> a
  , biCond :: a -> a -> a
  , not :: a -> a
  , inBraces :: a -> a
  , renderString :: String -> a
  }

render :: forall a out. RenderConfig a out -> Expr -> out
render config = renderPrec config 0

renderPrec :: forall a out. RenderConfig a out -> Int -> Expr -> out
renderPrec config p = config.wrap <<< go p
  where
  go _ (SymbolExpr s) = config.renderString s
  go prec (NegExpr expr) = 
    wrap (prec > negPrec) $ config.not $ go (negPrec+1) expr
    where negPrec = 9
  go prec (AndExpr a b) = 
    wrap (prec > andPrec) $ go (andPrec+1) a `config.and` go (andPrec+1) b
    where andPrec = 7
  go prec (OrExpr a b) = 
    wrap (prec > orPrec) $ go (orPrec+1) a `config.or` go (orPrec+1) b
    where orPrec = 5
  go prec (ImplExpr a b) = 
    wrap (prec > implPrec) $ go (implPrec+1) a `config.impl` go (implPrec+1) b
    where implPrec = 3
  go prec (BiCondExpr a b) = 
    wrap (prec > biCondPrec) $ go (biCondPrec+1) a `config.biCond` go (biCondPrec+1) b
    where biCondPrec = 1
  wrap true = config.inBraces 
  wrap false = identity

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
      , [ Infix (string "<=>" <* skipSpaces $> BiCondExpr) AssocRight ]
      ] 
      (between (string "(" <* skipSpaces) (string ")" <* skipSpaces) cont
      <|> SymbolExpr <<< fromCharArray <$> (some letter <* skipSpaces) 
      )