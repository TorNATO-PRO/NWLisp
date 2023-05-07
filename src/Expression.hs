{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- Copyright (c) 2023 Nathan Waltz
-- All rights reserved.
--
-- Expression interpreters using GADTs.

module Expression
  ( Atom (..),
    Expression (..),
    evalExpr,
    evalExprToString,
    infixTokenizeExpr,
    prefixTokenizeExpr,
  )
where

import Data.Foldable (Foldable (fold))

newtype Atom = Atom String

instance Show Atom where
  show (Atom a) = a

data Expression a where
  LispInt :: Int -> Expression Int
  LispFloat :: Float -> Expression Float
  LispBool :: Bool -> Expression Bool
  LispString :: String -> Expression String
  LispAtom :: Atom -> Expression Atom
  LispVoid :: () -> Expression ()
  AddInts :: Expression Int -> Expression Int -> Expression Int
  AddFloats :: Expression Float -> Expression Float -> Expression Float
  MultInts :: Expression Int -> Expression Int -> Expression Int
  MultFloats :: Expression Float -> Expression Float -> Expression Float
  SubInts :: Expression Int -> Expression Int -> Expression Int
  SubFloats :: Expression Float -> Expression Float -> Expression Float
  DivInts :: Expression Int -> Expression Int -> Expression Int
  DivFloats :: Expression Float -> Expression Float -> Expression Float
  Equals :: (Eq a, Show a) => Expression a -> Expression a -> Expression Bool
  NotEquals :: (Eq a, Show a) => Expression a -> Expression a -> Expression Bool
  GreaterThan :: (Ord a, Show a) => Expression a -> Expression a -> Expression Bool
  GreaterThanEq :: (Ord a, Show a) => Expression a -> Expression a -> Expression Bool
  LessThan :: (Ord a, Show a) => Expression a -> Expression a -> Expression Bool
  LessThanEq :: (Ord a, Show a) => Expression a -> Expression a -> Expression Bool
  If :: Expression Bool -> Expression a -> Expression a -> Expression a


evalExpr :: forall a. Show a => Expression a -> a
evalExpr (LispInt n) = n
evalExpr (LispBool b) = b
evalExpr (LispFloat f) = f
evalExpr (LispString s) = s
evalExpr (LispAtom a) = a
evalExpr (LispVoid v) = v
evalExpr (AddInts e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (AddFloats e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (MultInts e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (MultFloats e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (SubInts e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (SubFloats e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (DivInts e1 e2) = evalExpr e1 `div` evalExpr e2
evalExpr (DivFloats e1 e2) = evalExpr e1 / evalExpr e2
evalExpr (Equals e1 e2) = evalExpr e1 == evalExpr e2
evalExpr (NotEquals e1 e2) = evalExpr e1 /= evalExpr e2
evalExpr (GreaterThan e1 e2) = evalExpr e1 > evalExpr e2
evalExpr (GreaterThanEq e1 e2) = evalExpr e1 >= evalExpr e2
evalExpr (LessThan e1 e2) = evalExpr e1 < evalExpr e2
evalExpr (LessThanEq e1 e2) = evalExpr e1 <= evalExpr e2
evalExpr (If eb e1 e2) = if evalExpr eb then evalExpr e1 else evalExpr e2

evalExprToString :: forall a. Show a => Expression a -> String
evalExprToString = show . evalExpr

infixTokenizeExpr :: forall a. Expression a -> String
infixTokenizeExpr (LispInt n) = show n
infixTokenizeExpr (LispBool b) = show b
infixTokenizeExpr (LispFloat f) = show f
infixTokenizeExpr (LispVoid _) = "()"
infixTokenizeExpr (LispAtom (Atom a)) = a
infixTokenizeExpr (LispString s) = show s
infixTokenizeExpr (AddInts e1 e2) = fold ["(", infixTokenizeExpr e1, " + ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (AddFloats e1 e2) = fold ["(", infixTokenizeExpr e1, " + ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (MultInts e1 e2) = fold ["(", infixTokenizeExpr e1, " * ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (MultFloats e1 e2) = fold ["(", infixTokenizeExpr e1, " * ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (SubInts e1 e2) = fold ["(", infixTokenizeExpr e1, " - ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (SubFloats e1 e2) = fold ["(", infixTokenizeExpr e1, " - ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (DivInts e1 e2) = fold ["(", infixTokenizeExpr e1, " / ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (DivFloats e1 e2) = fold ["(", infixTokenizeExpr e1, " / ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (Equals e1 e2) = fold ["(", infixTokenizeExpr e1, " == ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (NotEquals e1 e2) = fold ["(", infixTokenizeExpr e1, " != ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (GreaterThan e1 e2) = fold ["(", infixTokenizeExpr e1, " > ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (GreaterThanEq e1 e2) = fold ["(", infixTokenizeExpr e1, " >= ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (LessThan e1 e2) = fold ["(", infixTokenizeExpr e1, " < ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (LessThanEq e1 e2) = fold ["(", infixTokenizeExpr e1, " <= ", infixTokenizeExpr e2, ")"]
infixTokenizeExpr (If eb e1 e2) = fold ["(if ", infixTokenizeExpr eb, " then ", infixTokenizeExpr e1, " else ", infixTokenizeExpr e2]

prefixTokenizeExpr :: forall a. Expression a -> String
prefixTokenizeExpr (LispInt n) = show n
prefixTokenizeExpr (LispBool b) = show b
prefixTokenizeExpr (LispFloat f) = show f
prefixTokenizeExpr (LispAtom (Atom a)) = a
prefixTokenizeExpr (LispString s) = show s
prefixTokenizeExpr (LispVoid _) = "()"
prefixTokenizeExpr (AddInts e1 e2) = fold ["(+ ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (AddFloats e1 e2) = fold ["(+ ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (MultInts e1 e2) = fold ["(* ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (MultFloats e1 e2) = fold ["(* ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (SubInts e1 e2) = fold ["(- ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (SubFloats e1 e2) = fold ["(- ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (DivInts e1 e2) = fold ["(/ ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (DivFloats e1 e2) = fold ["(/ ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (Equals e1 e2) = fold ["(== ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (NotEquals e1 e2) = fold ["(!= ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (GreaterThan e1 e2) = fold ["(> ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (GreaterThanEq e1 e2) = fold ["(>= ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (LessThan e1 e2) = fold ["(< ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (LessThanEq e1 e2) = fold ["(<= ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
prefixTokenizeExpr (If eb e1 e2) = fold ["(if ", prefixTokenizeExpr eb, " ", prefixTokenizeExpr e1, " ", prefixTokenizeExpr e2, ")"]
