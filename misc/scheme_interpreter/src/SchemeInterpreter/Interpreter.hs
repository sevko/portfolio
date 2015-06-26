module SchemeInterpreter.Interpreter where

import qualified SchemeInterpreter.Types as Types

eval :: Types.LispVal -> Types.LispVal
eval (Types.List [Types.Atom "quote", list]) = list
eval val = val
