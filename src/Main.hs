module Main where

import Text.Trifecta

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Char (digitToInt)

import Broken ()
import Expr

{---------- Grammar ------------------
EXPR  = START, END
START = GROUP | LIT
END   = "-", EXPR | NOTHING
GROUP = "(", EXPR, ")"
LIT   = "0" | "1" | "2" | ... | "9"
--------------------------------------}

-- Grammar rule: EXPR = START, END
expr :: Parser Expr
expr = do
  e1 <- start
  mE2 <- end
  case mE2 of
    Nothing -> pure e1
    Just e2 -> pure $ Sub e1 e2

-- Grammar rule: START = GROUP | LIT
start :: Parser Expr
start = group <|> lit

-- Grammar rule: END = "-", EXPR | NOTHING
end :: Parser (Maybe Expr)
end = getEnd <|> pure Nothing where
  getEnd = do
    char '-'
    e <- expr
    pure $ Just e

-------- Everything below is the same as before ------------------------

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

main :: IO ()
main = forM_
  [ "1"
  , "2-3"
  , "1-1-1-1"
  , "(3)-(2-((1)-0-5))-1"
  ]
  (print . parseString expr mempty)
