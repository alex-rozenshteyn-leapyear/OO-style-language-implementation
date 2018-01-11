{-# LANGUAGE GADTs #-}
module Cond where

import Data

condObj :: Obj -> Obj -> Obj -> Obj
condObj o1 o2 o3 = o
  where
    o = Obj {printExpr = myPrintExpr, check = myCheck}
    myPrintExpr = concat
      [ "if "
      , printExpr o1
      , " then "
      , printExpr o2
      , " else "
      , printExpr o3
      ]
    myCheck = do
      Tagged BTag to1 <- check o1
      Tagged t2 to2 <- check o2
      Tagged t3 to3 <- check o3
      case t2 of
        BTag -> case t3 of 
          BTag -> Just $ Tagged t2 $
            TObj { eval = if eval to1 then eval to2 else eval to3
                 , uncheck = o}
          _ -> Nothing
        ITag -> case t3 of 
          ITag -> Just $ Tagged t2 $
            TObj { eval = if eval to1 then eval to2 else eval to3
                 , uncheck = o}
          _ -> Nothing

