{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Data where

data Expr = B Bool | I Int | Plus Expr Expr

data TObj a = TObj {eval :: a}

data Obj = Obj
  { check :: Maybe (Tagged TObj)
  , printExpr :: String
  }

data Tag a where
  BTag :: Tag Bool
  ITag :: Tag Int

data Tagged f = forall a. Tagged (Tag a) (f a)
