{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Obj where

import Data

mkObj :: Expr -> Obj
mkObj = \case
  B b -> Obj $ Just $ BTag `Tagged` (TObj b)
  I i -> Obj $ Just $ ITag `Tagged` (TObj i)
  Plus e1 e2 -> Obj . fmap (ITag `Tagged`) $ do
    Tagged ITag te1 <- check $ mkObj e1
    Tagged ITag te2 <- check $ mkObj e2
    return $ TObj $ eval te1 + eval te2
