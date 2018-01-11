{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Obj where

import Data

mkObj :: Expr -> Obj
mkObj = \case
  B b -> Obj {check = Just $ BTag `Tagged` (TObj b), printExpr = show b}
  I i -> Obj {check = Just $ ITag `Tagged` (TObj i), printExpr = show i}
  Plus e1 e2 -> arithObj ArithOps{opEval = (+), opName = "+"} e1 e2

data ArithOps = ArithOps
  { opEval :: Int -> Int -> Int
  , opName :: String
  }

arithObj :: ArithOps -> Expr -> Expr -> Obj
arithObj ArithOps{..} e1 e2 = Obj {printExpr = myPrintExpr, check = myCheck}
  where
    myCheck = (ITag `Tagged`) <$> do
      Tagged ITag te1 <- check $ mkObj e1
      Tagged ITag te2 <- check $ mkObj e2
      return $ TObj $ eval te1 `opEval` eval te2
    myPrintExpr = concat
      [ "("
      , printExpr $ mkObj e1
      , ")"
      , " "
      , opName
      , " "
      , "("
      , printExpr $ mkObj e2
      , ")"
      ]

