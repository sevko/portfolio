{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

{-
 - The interpreter that evaluates parsed Scheme code.
 -}

module SchemeInterpreter.Interpreter where

import qualified SchemeInterpreter.Types as Types
import qualified SchemeInterpreter.State as State
import qualified Control.Monad.Error as Error
import qualified Control.Monad as Monad
import qualified Control.Applicative as Applicative

data TypeCoercer = forall a. Eq a =>
	AnyCoercer (Types.LispVal -> Types.ThrowsError a)

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
		_ -> Error.throwError $ Types.TypeMismatch
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
		evalClauses [] = Error.throwError $ Types.ProgramStructure
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
				_ -> Error.throwError $ Types.TypeMismatch
					"cond condition must evaluate to a boolean"
					result
		evalClause badType = Error.throwError $ Types.TypeMismatch
			"clause with one condition and one or more expressions"
			badType
eval env (Types.List (Types.Atom "define" : defineArg1 : body)) =
	case defineArg1 of
		Types.List (Types.Atom funcName : params) ->
			bindFunc funcName $ Types.Func {
				Types.params = map show params,
				Types.varargs = Nothing,
				Types.body = body,
				Types.closure = env
			}
		Types.DottedList (Types.Atom funcName : params) varArgs ->
			case varArgs of
				(Types.Atom varArgName) -> bindFunc funcName $
					Types.Func {
						Types.params = map show params,
						Types.varargs = Just $ varArgName,
						Types.body = body,
						Types.closure = env
					}
				_ -> Error.throwError $ Types.TypeMismatch "atom" varArgs
		_ -> Error.throwError $
			Types.TypeMismatch "list or dotted list" defineArg1
	where bindFunc = State.defineVar env
eval env (Types.List (Types.Atom "lambda" : lambdaArg1 : body)) =
	return $ case lambdaArg1 of
		Types.List params -> createFunc params Nothing
		Types.DottedList params varargs ->
			createFunc params (Just $ show varargs)
		varargs@(Types.Atom _) ->
			createFunc ([] :: [Types.LispVal]) (Just $ show varargs)
	where createFunc params varargs = Types.Func {
		Types.params = map show params,
		Types.varargs = varargs,
		Types.body = body,
		Types.closure = env
	}
eval env (Types.List (function : args)) = do
	 eval'dFunc <- eval env function
	 eval'dArgs <- mapM (eval env) args
	 applyFunc eval'dFunc eval'dArgs
eval _ badForm = Error.throwError $
	Types.BadSpecialForm "Unrecognized special form" badForm

applyFunc :: Types.LispVal -> [Types.LispVal] ->
	Types.IOThrowsError Types.LispVal
applyFunc (Types.IOFunc func) args = func args
applyFunc (Types.PrimitiveFunc func) args = State.liftThrows $ func args
applyFunc (Types.Func {
	Types.params, Types.varargs, Types.body, Types.closure}) args = do
	let numParams = length params
	if length args /= numParams && varargs == Nothing
		then Error.throwError $ Types.NumArgs (fromIntegral numParams) args
		else let varArgs = drop (length params) args in
			(Error.liftIO $ State.defineVars closure $ zip params args) >>=
			defineVarArgs varArgs >>=
			evalBody
	where
		defineVarArgs varArgList env = case varargs of
			Just varArgName -> Error.liftIO $ State.defineVars env $
				[(varArgName, Types.List varArgList)]
			Nothing -> return env

		evalBody env = Monad.liftM last $ mapM (eval env) body
applyFunc notAFunc _ = Error.throwError $
	Types.TypeMismatch "expected function" notAFunc

primitiveEnv :: IO Types.Env
primitiveEnv = State.nullEnv >>= (flip State.defineVars $
	map (makeFunc Types.PrimitiveFunc) primitiveFuncs ++
	map (makeFunc Types.IOFunc) primitiveIOFuncs)
	where makeFunc constructor (funcName, func) = (funcName, constructor func)

primitiveFuncs ::
	[(String, [Types.LispVal] -> Types.ThrowsError Types.LispVal)]
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
	(Types.LispVal -> Types.ThrowsError a) ->
	(a -> a -> Bool) ->
	[Types.LispVal] ->
	Types.ThrowsError Types.LispVal
boolBinOp typeCoercer op [a, b] = do
	a' <- typeCoercer a
	b' <- typeCoercer b
	return $ Types.Bool $ op a' b'
boolBinOp _ _ args = Error.throwError $ Types.NumArgs 2 args

numBoolBinOp = boolBinOp coerceNum
strBoolBinOp = boolBinOp coerceStr
boolBoolBinOp = boolBinOp coerceBool

isNumber :: [Types.LispVal] -> Types.ThrowsError Types.LispVal
isNumber [(Types.Number _)] = return $ Types.Bool True
isNumber [_] = return $ Types.Bool False
isNumber args = Error.throwError $ Types.NumArgs 1 args

isString :: [Types.LispVal] -> Types.ThrowsError Types.LispVal
isString [(Types.String _)] = return $ Types.Bool True
isString [_] = return $ Types.Bool False
isString args = Error.throwError $ Types.NumArgs 1 args

numericBinOp :: (Integer -> Integer -> Integer) -> [Types.LispVal] ->
	Types.ThrowsError Types.LispVal
numericBinOp op args
	| length args <= 1 = Error.throwError $ Types.NumArgs 2 args
	| otherwise = fmap (Types.Number . foldl1 op) $
		sequence $ map coerceNum args

coerceNum :: Types.LispVal -> Types.ThrowsError Integer
coerceNum (Types.Number num) = return num
coerceNum val@(Types.String str) = case reads str of
	(num, _):_ -> return num
	_ -> Error.throwError $ Types.TypeMismatch "number" $ val
coerceNum (Types.List [num]) = coerceNum num
coerceNum notANum = Error.throwError $ Types.TypeMismatch "number" notANum

coerceStr :: Types.LispVal -> Types.ThrowsError String
coerceStr (Types.String str) = return str
coerceStr (Types.Number num) = return $ show num
coerceStr (Types.Bool bool) = return $ show bool
coerceStr notStr = Error.throwError $ Types.TypeMismatch "string" notStr

coerceBool :: Types.LispVal -> Types.ThrowsError Bool
coerceBool (Types.Bool bool) = return bool
coerceBool notBool = Error.throwError $ Types.TypeMismatch "boolean" notBool

coerceEquals :: Types.LispVal -> Types.LispVal -> TypeCoercer ->
	Types.ThrowsError Bool
coerceEquals a b (AnyCoercer coerce) = Error.catchError
	(Applicative.liftA2 (==) (coerce a) $ coerce b)
	(const $ return False)

car :: [Types.LispVal] -> Types.ThrowsError Types.LispVal
car [(Types.List (head':_))] = return head'
car [badType] = Error.throwError $
	Types.TypeMismatch "list with one or more elements" badType
car wrongNumArgs = Error.throwError $
	Types.NumArgs 1 wrongNumArgs

cdr :: [Types.LispVal] -> Types.ThrowsError Types.LispVal
cdr [(Types.List (_:tail'))] = return $ Types.List tail'
cdr [badType] = Error.throwError $
	Types.TypeMismatch "list with one or more elements" badType
cdr wrongNumArgs = Error.throwError $
	Types.NumArgs 1 wrongNumArgs

cons :: [Types.LispVal] -> Types.ThrowsError Types.LispVal
cons [item, Types.List items] = return $ Types.List $ item : items
cons [item, Types.DottedList items tail'] = return $
	Types.DottedList (item : items) tail'
cons [a, b] = return $ Types.DottedList [a] b
cons wrongNumArgs = Error.throwError $ Types.NumArgs 2 wrongNumArgs

eqv :: [Types.LispVal] -> Types.ThrowsError Types.LispVal
eqv [a, b] = return $ Types.Bool $ a == b
eqv badNumArgs = Error.throwError $ Types.NumArgs 2 badNumArgs

equal :: [Types.LispVal] -> Types.ThrowsError Types.LispVal
equal args@[a, b] = do
	primitiveEquals <- fmap or $ mapM (coerceEquals a b) [
		AnyCoercer coerceNum, AnyCoercer coerceStr, AnyCoercer coerceBool]
	(Types.Bool areEqual) <- eqv args
	return $ Types.Bool $ primitiveEquals || areEqual
equal wrongNumArgs = Error.throwError $ Types.NumArgs 2 wrongNumArgs
