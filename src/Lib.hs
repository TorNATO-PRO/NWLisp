-- Copyright (c) 2023 Nathan Waltz
-- All rights reserved.
--
-- Here is an embedded LISP implementation.

module Lib
  ( someFunc,
  )
where

import Expression (Atom (Atom), Expression (..))
import Language (FreeNWLispAST, lispPrint)
import LanguageTagless (ArithSYM (..), ControlFlowSYM (if', for'), IOSYM (print'), LogicSYM (eq'), eval)

atomicExpr :: FreeNWLispAST Atom ()
atomicExpr = lispPrint (LispAtom (Atom "hi"))

intExpr :: FreeNWLispAST Int ()
intExpr = lispPrint (LispInt 5)

strExpr :: FreeNWLispAST String ()
strExpr = lispPrint $ If (Equals (LispString "Cool") (LispString "Cool")) (LispString "My toy example works") (LispString "My toy example fails")

smallExpression :: (IOSYM repr, LogicSYM repr, ArithSYM repr, ControlFlowSYM repr) => repr (IO ())
smallExpression = for' (lispInt 0) (lispInt 5) $ print' $ if' (lispInt 5 `eq'` lispInt 10) (lispInt 5 `mul'` lispInt 6) ((lispInt 5 `mul'` lispInt 500) `idiv'` lispInt 7)

someFunc :: IO ()
someFunc = eval smallExpression
