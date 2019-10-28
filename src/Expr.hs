module Expr where

data Expr = Lit Int | Sub Expr Expr deriving Show

-- alternatively, could use infix contructor:
-- data Expr = Lit Int | Expr :-: Expr deriving Show
