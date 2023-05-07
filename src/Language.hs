{-# LANGUAGE DeriveFunctor #-}

-- Copyright (c) 2023 Nathan Waltz
-- All rights reserved.
--
-- A language syntax AST defined and
-- interpreted using the Free Monad.

module Language
  ( lispPrint,
    computeFree,
    tokenizeFree,
    FreeNWLispAST,
  )
where

import Control.Monad.Trans.Free (Free, FreeF (Free, Pure), liftF, runFree)
import Expression (Expression, evalExpr, prefixTokenizeExpr)
import Data.Foldable (Foldable(fold))

data NWLispAST a next
  = Print (Expression a) next
  deriving (Functor)

type FreeNWLispAST a = Free (NWLispAST a) 

lispPrint :: Expression a -> FreeNWLispAST a ()
lispPrint expr = liftF $ Print expr ()

computeFree :: Show a => FreeNWLispAST a () -> IO ()
computeFree freeAst = case runFree freeAst of
  Pure r -> return r
  Free (Print expr next) -> print (evalExpr expr) >> computeFree next

tokenizeFree :: Show a => FreeNWLispAST a n -> String
tokenizeFree freeAst = case runFree freeAst of
    Pure _ -> ""
    Free (Print expr next) -> fold ["(print ", prefixTokenizeExpr expr, ") "] <> tokenizeFree next
