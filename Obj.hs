{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Obj where

import Data
import Arith
import Cond

mkObj :: Expr -> Obj
mkObj = \case
  B b -> let o = Obj {check = Just $ BTag `Tagged` (TObj {eval = b, uncheck = o}), printExpr = show b} in o
  I i -> let o = Obj {check = Just $ ITag `Tagged` (TObj {eval = i, uncheck = o}), printExpr = show i} in o
  Plus e1 e2 -> plusObj (mkObj e1) (mkObj e2)
  Minus e1 e2 -> minusObj (mkObj e1) (mkObj e2)
  Times e1 e2 -> timesObj (mkObj e1) (mkObj e2)
  Cond e1 e2 e3 -> condObj (mkObj e1) (mkObj e2) (mkObj e3)
