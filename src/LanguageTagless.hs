{-# Language BangPatterns #-}

-- Copyright (c) 2023 Nathan Waltz
-- All rights reserved.
--
-- Remove reliance on the free monad, and evaluate the
-- entire embedded, typed lisp using Tagless final pattern.

module LanguageTagless
  ( ArithSYM (..),
    LogicSYM (..),
    IOSYM (..),
    ControlFlowSYM (..),
    Interpreter,
    eval,
  )
where
import Control.DeepSeq (deepseq)

class ArithSYM repr where
  lispInt :: Int -> repr Int
  lispFloat :: Float -> repr Float
  add' :: Num a => repr a -> repr a -> repr a
  mul' :: Num a => repr a -> repr a -> repr a
  sub' :: Num a => repr a -> repr a -> repr a
  div' :: Fractional a => repr a -> repr a -> repr a
  idiv' :: Integral a => repr a -> repr a -> repr a

class LogicSYM repr where
  lispBool :: Bool -> repr Bool
  eq' :: Eq a => repr a -> repr a -> repr Bool
  geq' :: Ord a => repr a -> repr a -> repr Bool
  gt' :: Ord a => repr a -> repr a -> repr Bool
  leq' :: Ord a => repr a -> repr a -> repr Bool
  lt' :: Ord a => repr a -> repr a -> repr Bool

class IOSYM repr where
  print' :: Show a => repr a -> repr (IO ())

class ControlFlowSYM repr where
  if' :: repr Bool -> repr a -> repr a -> repr a
  for' :: repr Int -> repr Int -> repr (IO ()) -> repr (IO ())

newtype Interpreter a = Interpreter {runExp :: a}

instance ArithSYM Interpreter where
  lispInt = Interpreter
  lispFloat = Interpreter
  add' a b = Interpreter $ runExp a + runExp b
  mul' a b = Interpreter $ runExp a * runExp b
  sub' a b = Interpreter $ runExp a - runExp b
  div' a b = Interpreter $ runExp a / runExp b
  idiv' a b = Interpreter $ runExp a `div` runExp b

instance LogicSYM Interpreter where
  lispBool = Interpreter
  eq' a b = Interpreter $ runExp a == runExp b
  geq' a b = Interpreter $ runExp a >= runExp b
  gt' a b = Interpreter $ runExp a > runExp b
  leq' a b = Interpreter $ runExp a <= runExp b
  lt' a b = Interpreter $ runExp a < runExp b

instance IOSYM Interpreter where
  print' a = Interpreter $ print (runExp a)

instance ControlFlowSYM Interpreter where
  if' (Interpreter cond) left right = (Interpreter . runExp) $ if cond then left else right
  for' (Interpreter start) (Interpreter stop) action
    | start == stop = Interpreter $ pure ()
    | start > stop = Interpreter $ pure ()
    | otherwise = Interpreter $ do
        runExp action
        runExp $ for' (Interpreter (start + 1)) (Interpreter stop) action

eval :: Interpreter a -> a
eval = runExp
