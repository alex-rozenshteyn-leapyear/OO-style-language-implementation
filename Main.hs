{-# LANGUAGE LambdaCase #-}
module Main where

data Expr = I Int | Plus Expr Expr

data Obj a = Obj {eval :: a}

mkObj :: Expr -> Obj Int
mkObj = \case
  I i -> Obj i
  Plus e1 e2 -> Obj $ eval (mkObj e1) + eval (mkObj e2)

testExpr = (I 1 `Plus` I 2) `Plus` I 3

main = print $ eval (mkObj testExpr)
