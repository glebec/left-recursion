module Broken where

import Text.Trifecta

import Control.Applicative ((<|>))
import Data.Char (digitToInt)

import Expr

{----------  Grammar -----------------
EXPR  = SUB | GROUP | LIT
SUB   = EXPR, "-", EXPR
GROUP = "(", EXPR, ")"
LIT   = "0" | "1" | "2" | ... | "9"
--------------------------------------}

-- Grammar rule: EXPR = SUB | GROUP | LIT
expr :: Parser Expr
expr = sub <|> group <|> lit -- first try `sub`...

-- Grammar rule: SUB = EXPR, "-", EXPR
sub :: Parser Expr
sub = do
  e1 <- expr -- now do `expr`. WARNING: infinite recursion!
  char '-'
  e2 <- expr
  pure $ Sub e1 e2

-- Grammar rule: GROUP = "(", EXPR, ")"
group :: Parser Expr
group = do
  char '('
  e <- expr
  char ')'
  pure e

-- Grammar rule: LIT = "0" | "1" | "2" | ... | "9"
lit :: Parser Expr
lit = do
  d <- digit
  pure . Lit . digitToInt $ d

-- do not do this, it is an infinite loop (if forced):
-- result = parseString expr mempty "1"
