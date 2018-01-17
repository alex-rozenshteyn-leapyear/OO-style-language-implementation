{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Arith where

import Control.Applicative

import Data

data ArithOps = ArithOps
  { opEval :: forall a. Num a => a -> a -> a
  , opName :: String
  }

plusObj = arithObj (ArithOps (+) "+")
minusObj = arithObj (ArithOps (-) "-")
timesObj = arithObj (ArithOps (*) "*")

arithObj :: ArithOps -> Obj -> Obj -> Obj
arithObj ArithOps{..} o1 o2 = o
  where
    o = Obj {printExpr = myPrintExpr, check = myCheckI <|> myCheckD}
    myCheckD = (DTag `Tagged`) <$> do
      Tagged DTag to1 <- check o1
      Tagged DTag to2 <- check o2
      return $ TObj {eval = eval to1 `opEval` eval to2, uncheck = o}
    myCheckI = (ITag `Tagged`) <$> do
      Tagged ITag to1 <- check o1
      Tagged ITag to2 <- check o2
      return $ TObj {eval = eval to1 `opEval` eval to2, uncheck = o}
    myPrintExpr = concat
      [ "("
      , printExpr o1
      , ")"
      , " "
      , opName
      , " "
      , "("
      , printExpr o2
      , ")"
      ]

