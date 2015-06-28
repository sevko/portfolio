{-
 - The interpreter that evaluates parsed Scheme code.
 -}

module SchemeInterpreter.Interpreter where

import qualified SchemeInterpreter.Types as Types
import qualified SchemeInterpreter.Error as Error

eval :: Types.LispVal -> Error.ThrowsError Types.LispVal
eval val@(Types.String _) = return val
eval val@(Types.Number _) = return val
eval val@(Types.Bool _) = return val
eval (Types.List [Types.Atom "quote", val]) = return val
eval (Types.List (Types.Atom func : args)) = mapM eval args >>= applyFunc func
eval badForm = Error.throwError $
	Error.BadSpecialForm "Unrecognized special form" badForm

applyFunc :: String -> [Types.LispVal] -> Error.ThrowsError Types.LispVal
applyFunc funcName args = case lookup funcName primitiveFuncs of
	Just func -> func args
	Nothing -> Error.throwError $
		Error.NotFunction "Unrecognized primitive function args" funcName

primitiveFuncs ::
	[(String, [Types.LispVal] -> Error.ThrowsError Types.LispVal)]
primitiveFuncs = [
	("+", numericBinOp (+)),
	("-", numericBinOp (-)),
	("*", numericBinOp (*)),
	("/", numericBinOp div),
	("mod", numericBinOp mod),
	("quotient", numericBinOp quot),
	("remainder", numericBinOp rem),
	("number?", isNumber),
	("string?", isString)]

isNumber :: [Types.LispVal] -> Error.ThrowsError Types.LispVal
isNumber [(Types.Number _)] = return $ Types.Bool True
isNumber [_] = return $ Types.Bool False
isNumber args = Error.throwError $ Error.NumArgs 1 args

isString :: [Types.LispVal] -> Error.ThrowsError Types.LispVal
isString [(Types.String _)] = return $ Types.Bool True
isString [_] = return $ Types.Bool False
isString args = Error.throwError $ Error.NumArgs 1 args

numericBinOp :: (Integer -> Integer -> Integer) -> [Types.LispVal] ->
	Error.ThrowsError Types.LispVal
numericBinOp op args
	| length args <= 1 = Error.throwError $ Error.NumArgs 2 args
	| otherwise = fmap (Types.Number . foldl1 op) $
		sequence $ map coerceNum args

coerceNum :: Types.LispVal -> Error.ThrowsError Integer
coerceNum (Types.Number num) = return num
coerceNum val@(Types.String str) = case reads str of
	(num, _):_ -> return num
	_ -> Error.throwError $ Error.TypeMismatch "number" $ val
coerceNum (Types.List [num]) = coerceNum num
coerceNum notANum = Error.throwError $ Error.TypeMismatch "number" notANum
