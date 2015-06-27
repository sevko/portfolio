{-
 - The interpreter that evaluates parsed Scheme code.
 -}

module SchemeInterpreter.Interpreter where

import qualified SchemeInterpreter.Types as Types

eval :: Types.LispVal -> Types.LispVal
eval (Types.List [Types.Atom "quote", list]) = list
eval (Types.List (Types.Atom funcName : args)) = applyFunc funcName $
	map eval args
eval val = val

applyFunc :: String -> [Types.LispVal] -> Types.LispVal
applyFunc func args = maybe (Types.Bool False) ($ args) $
	lookup func primitiveFuncs

primitiveFuncs :: [(String, [Types.LispVal] -> Types.LispVal)]
primitiveFuncs = [
	("+", numericBinOp (+)),
	("-", numericBinOp (-)),
	("*", numericBinOp (*)),
	("/", numericBinOp div),
	("mod", numericBinOp mod),
	("quotient", numericBinOp quot),
	("remainder", numericBinOp rem)]

numericBinOp :: (Integer -> Integer -> Integer) -> [Types.LispVal] ->
	Types.LispVal
numericBinOp op args = Types.Number $ foldl1 op $ map coerceNum args

coerceNum :: Types.LispVal -> Integer
coerceNum (Types.Number num) = num
coerceNum (Types.String str) = case reads str of
	(num, _):_ -> num
	_ -> 0
coerceNum (Types.List [num]) = coerceNum num
coerceNum _ = 0
