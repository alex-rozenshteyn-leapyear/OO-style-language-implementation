{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where

data Expr = B Bool | I Int | Plus Expr Expr

data TObj a = TObj {eval :: a}

data Obj = Obj {check :: Maybe (Tagged TObj)}

data Tag a where
  BTag :: Tag Bool
  ITag :: Tag Int

data Tagged f = forall a. Tagged (Tag a) (f a)

mkObj :: Expr -> Obj
mkObj = \case
  B b -> Obj $ Just $ BTag `Tagged` (TObj b)
  I i -> Obj $ Just $ ITag `Tagged` (TObj i)
  Plus e1 e2 -> Obj . fmap (ITag `Tagged`) $ do
    Tagged ITag te1 <- check $ mkObj e1
    Tagged ITag te2 <- check $ mkObj e2
    return $ TObj $ eval te1 + eval te2

testExpr1 = (I 1 `Plus` I 2) `Plus` I 3
testExpr2 = (I 1 `Plus` I 2) `Plus` B True
testExpr3 = B True

printEval :: Maybe (Tagged TObj) -> IO ()
printEval = \case
  Nothing -> print "type error"
  Just te -> case te of
    Tagged BTag e -> print $ eval e
    Tagged ITag e -> print $ eval e

main = mapM_ (printEval . check . mkObj) [testExpr1, testExpr2, testExpr3]
