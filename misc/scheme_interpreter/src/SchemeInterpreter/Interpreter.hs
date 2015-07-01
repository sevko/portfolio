{-# LANGUAGE ExistentialQuantification #-}

{-
 - The interpreter that evaluates parsed Scheme code.
 -}

module SchemeInterpreter.Interpreter where

import qualified SchemeInterpreter.Types as Types
import qualified SchemeInterpreter.Types as Error
import qualified SchemeInterpreter.State as State
import qualified Control.Applicative as Applicative

data TypeCoercer = forall a. Eq a =>
	AnyCoercer (Types.LispVal -> Error.ThrowsError a)

eval :: Types.Env -> Types.LispVal -> Types.IOThrowsError Types.LispVal
eval _ val@(Types.String _) = return val
eval _ val@(Types.Number _) = return val
eval _ val@(Types.Bool _) = return val
eval _ val@(Types.DottedList _ _) = return val
eval env (Types.Atom variable) = State.getVar env variable
eval env (Types.List [Types.Atom "set!", Types.Atom var, form]) =
	eval env form >>= State.setVar env var
eval env (Types.List [Types.Atom "define", Types.Atom var, form]) =
    eval env form >>= State.defineVar env var
eval _ (Types.List [Types.Atom "quote", val]) = return val
eval env (Types.List [Types.Atom "if", condition, ifClause, thenClause]) = do
	result <- eval env condition
	case result of
		Types.Bool True -> eval env ifClause
		Types.Bool False -> eval env thenClause
		_ -> Error.throwError $ Error.TypeMismatch
			"if condition must evaluate to a boolean"
			result
eval env (Types.List ((Types.Atom "cond"):condClauses)) =
	evalClauses condClauses
	where
		evalClauses :: [Types.LispVal] -> Types.IOThrowsError Types.LispVal
		evalClauses (clause:clauses) = do
			clauseResult <- evalClause clause
			case clauseResult of
				Just result -> return result
				Nothing -> evalClauses clauses
		evalClauses [] = Error.throwError $ Error.ProgramStructure
			"All `cond` conditions evaluated to False and no `else` \
			\clause was provided, so this `cond` expr has no value."

		evalClause :: Types.LispVal ->
			Types.IOThrowsError (Maybe Types.LispVal)
		evalClause (Types.List ((Types.Atom "else"):exprs)) =
			fmap (Just . last) $ mapM (eval env) exprs
		evalClause (Types.List (cond:exprs)) = do
			result <- eval env cond
			case result of
				Types.Bool True -> if null exprs
					then return $ Just result
					else fmap (Just . last) $ mapM (eval env) exprs
				Types.Bool False -> return Nothing
				_ -> Error.throwError $ Error.TypeMismatch
					"cond condition must evaluate to a boolean"
					result
		evalClause badType = Error.throwError $ Error.TypeMismatch
			"clause with one condition and one or more expressions"
			badType

eval env (Types.List (Types.Atom func : args)) = mapM (eval env) args >>=
	State.liftThrows . applyFunc func
eval _ badForm = Error.throwError $
	Error.BadSpecialForm "Unrecognized special form" badForm

applyFunc :: String -> [Types.LispVal] -> Error.ThrowsError Types.LispVal
applyFunc funcName args = case lookup funcName primitiveFuncs of
	Just func -> func args
	Nothing -> Error.throwError $
		Error.NotFunction "Unrecognized primitive function" funcName

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
	("string?", isString),
	("=", numBoolBinOp (==)),
	("<", numBoolBinOp (<)),
	(">", numBoolBinOp (>)),
	("/=", numBoolBinOp (/=)),
	(">=", numBoolBinOp (>=)),
	("<=", numBoolBinOp (<=)),
	("&&", boolBoolBinOp (&&)),
	("||", boolBoolBinOp (||)),
	("string=?", strBoolBinOp (==)),
	("string<?", strBoolBinOp (<)),
	("string>?", strBoolBinOp (>)),
	("string<=?", strBoolBinOp (<=)),
	("string>=?", strBoolBinOp (>=)),
	("cons", cons),
	("cdr", cdr),
	("car", car),
	("eq?", eqv),
	("eqv?", eqv),
	("equal?", equal)]

boolBinOp ::
	(Types.LispVal -> Error.ThrowsError a) ->
	(a -> a -> Bool) ->
	[Types.LispVal] ->
	Error.ThrowsError Types.LispVal
boolBinOp typeCoercer op [a, b] = do
	a' <- typeCoercer a
	b' <- typeCoercer b
	return $ Types.Bool $ op a' b'
boolBinOp _ _ args = Error.throwError $ Error.NumArgs 2 args

numBoolBinOp = boolBinOp coerceNum
strBoolBinOp = boolBinOp coerceStr
boolBoolBinOp = boolBinOp coerceBool

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

coerceStr :: Types.LispVal -> Error.ThrowsError String
coerceStr (Types.String str) = return str
coerceStr (Types.Number num) = return $ show num
coerceStr (Types.Bool bool) = return $ show bool
coerceStr notStr = Error.throwError $ Error.TypeMismatch "string" notStr

coerceBool :: Types.LispVal -> Error.ThrowsError Bool
coerceBool (Types.Bool bool) = return bool
coerceBool notBool = Error.throwError $ Error.TypeMismatch "boolean" notBool

coerceEquals :: Types.LispVal -> Types.LispVal -> TypeCoercer ->
	Error.ThrowsError Bool
coerceEquals a b (AnyCoercer coerce) = Error.catchError
	(Applicative.liftA2 (==) (coerce a) $ coerce b)
	(const $ return False)

car :: [Types.LispVal] -> Error.ThrowsError Types.LispVal
car [(Types.List (head':_))] = return head'
car [badType] = Error.throwError $
	Error.TypeMismatch "list with one or more elements" badType
car wrongNumArgs = Error.throwError $
	Error.NumArgs 1 wrongNumArgs

cdr :: [Types.LispVal] -> Error.ThrowsError Types.LispVal
cdr [(Types.List (_:tail'))] = return $ Types.List tail'
cdr [badType] = Error.throwError $
	Error.TypeMismatch "list with one or more elements" badType
cdr wrongNumArgs = Error.throwError $
	Error.NumArgs 1 wrongNumArgs

cons :: [Types.LispVal] -> Error.ThrowsError Types.LispVal
cons [item, Types.List items] = return $ Types.List $ item : items
cons [item, Types.DottedList items tail'] = return $
	Types.DottedList (item : items) tail'
cons [a, b] = return $ Types.DottedList [a] b
cons wrongNumArgs = Error.throwError $ Error.NumArgs 2 wrongNumArgs

eqv :: [Types.LispVal] -> Error.ThrowsError Types.LispVal
eqv [a, b] = return $ Types.Bool $ a == b
eqv badNumArgs = Error.throwError $ Error.NumArgs 2 badNumArgs

equal :: [Types.LispVal] -> Error.ThrowsError Types.LispVal
equal args@[a, b] = do
	primitiveEquals <- fmap or $ mapM (coerceEquals a b) [
		AnyCoercer coerceNum, AnyCoercer coerceStr, AnyCoercer coerceBool]
	(Types.Bool areEqual) <- eqv args
	return $ Types.Bool $ primitiveEquals || areEqual
equal wrongNumArgs = Error.throwError $ Error.NumArgs 2 wrongNumArgs
