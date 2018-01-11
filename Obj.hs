{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Obj where

import Data

mkObj :: Expr -> Obj
mkObj = \case
  B b -> let o = Obj {check = Just $ BTag `Tagged` (TObj {eval = b, uncheck = o}), printExpr = show b} in o
  I i -> let o = Obj {check = Just $ ITag `Tagged` (TObj {eval = i, uncheck = o}), printExpr = show i} in o
  Plus e1 e2 -> arithObj ArithOps{opEval = (+), opName = "+"} e1 e2

data ArithOps = ArithOps
  { opEval :: Int -> Int -> Int
  , opName :: String
  }

arithObj :: ArithOps -> Expr -> Expr -> Obj
arithObj ArithOps{..} e1 e2 = o
  where
    o = Obj {printExpr = myPrintExpr, check = myCheck}
    myCheck = (ITag `Tagged`) <$> do
      Tagged ITag te1 <- check $ mkObj e1
      Tagged ITag te2 <- check $ mkObj e2
      return $ TObj {eval = eval te1 `opEval` eval te2, uncheck = o}
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

