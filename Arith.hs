{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Arith where

import Data

data ArithOps = ArithOps
  { opEval :: Int -> Int -> Int
  , opName :: String
  }

plusObj = arithObj (ArithOps (+) "+")
minusObj = arithObj (ArithOps (-) "-")
timesObj = arithObj (ArithOps (*) "*")

arithObj :: ArithOps -> Obj -> Obj -> Obj
arithObj ArithOps{..} o1 o2 = o
  where
    o = Obj {printExpr = myPrintExpr, check = myCheck}
    myCheck = (ITag `Tagged`) <$> do
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

