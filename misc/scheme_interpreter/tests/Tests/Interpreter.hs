{-
 - Tests for `SchemeInterpreter.Interpreter`.
 -}

module Tests.Interpreter where

import qualified SchemeInterpreter.Interpreter as Interpreter
import qualified SchemeInterpreter.Types as Error
import qualified SchemeInterpreter.Types as Types
import qualified SchemeInterpreter.State as State
import qualified Control.Monad.Error as MonadError
import qualified Test.HUnit as HUnit

testEval :: HUnit.Test
testEval = HUnit.TestList $ map evalTest [
	(Types.String "abc", return $ Types.String "abc"),
	(Types.Number 9, return $ Types.Number 9),
	(createIf (Types.Bool True) (Types.Number 1) (Types.Number 2),
		return $ Types.Number 1),
	(createIf (Types.Bool False) (Types.Number 1) (Types.Number 2),
		return $ Types.Number 2),
	(createIf (Types.Number 1) (Types.Number 1) (Types.Number 2),
		Error.throwError $ Error.TypeMismatch "if condition must evaluate to a boolean" $
			Types.Number 1),
	(Types.List [
		Types.Atom "+", Types.Number 1, Types.Number 2],
		return $ Types.Number 3),
	(Types.List [
		Types.Atom "+",
		Types.List[Types.Atom "+", Types.Number 1, Types.Number 2],
		Types.Number 4,
		Types.List[Types.Atom "-", Types.Number 10, Types.Number 2]],
		return $ Types.Number 15),
	(Types.List [
		Types.Atom "+",
		Types.Number 3],
		Error.throwError $ Error.NumArgs 2 [Types.Number 3]),
	(Types.List [
		Types.Atom "string?",
		Types.String "a",
		Types.String "b"],
		Error.throwError $ Error.NumArgs 1 [Types.String "a", Types.String "b"]),
	(Types.List [
		Types.Atom "car",
		Types.Number 1,
		Types.Number 4],
		Error.throwError $ Error.NumArgs 1 [Types.Number 1, Types.Number 4]),
	(Types.List [Types.Atom "car", createList []],
		Error.throwError $ Error.TypeMismatch "list with one or more elements" $
			Types.List []),
	(Types.List [Types.Atom "car", createList $ map Types.Number [1, 2, 3]],
		return $ Types.Number 1),
	(Types.List [
		Types.Atom "cdr",
		createList $ map Types.Number [1, 2, 3]],
		return $ Types.List [Types.Number 2, Types.Number 3]),
	(Types.List [Types.Atom "cons", Types.Number 1, createList []],
		return $ Types.List [Types.Number 1]),
	(Types.List [Types.Atom "cons", createList [], Types.String "foo"],
		return $ Types.DottedList [Types.List []] $ Types.String "foo"),
	(Types.List [
		Types.Atom "cons", createList [], Types.Number 1, Types.String "foo"],
		Error.throwError $ Error.NumArgs 2 [
			Types.List [], Types.Number 1, Types.String "foo"]),
	(Types.List [Types.Atom "eqv?", Types.Number 1, Types.Number 1],
		return $ Types.Bool $ True),
	(Types.List [Types.Atom "eqv?", Types.Number 1, Types.Number 5],
		return $ Types.Bool $ False),
	(Types.List [Types.Atom "eq?", createList [], createList [Types.Number 1]],
		return $ Types.Bool $ False),
	(let list = createList [Types.String "abc def"] in
		Types.List [Types.Atom "eqv?", list, list],
		return $ Types.Bool $ True),
	(Types.List [
		Types.Atom "eqv?",
		Types.DottedList [Types.Number 10, Types.String "abc def"] $
			Types.String "foo",
		Types.DottedList [Types.Number 10, Types.String "abc def"] $
			Types.String "foa"],
		return $ Types.Bool $ False),
	(Types.List [
		Types.Atom "cond",
		Types.List [Types.Bool False, Types.Number 1, Types.Number 2],
		Types.List [Types.Bool True, Types.Number 1, Types.Number 3]],
		return $ Types.Number 3),
	(Types.List [
		Types.Atom "cond",
		Types.List [Types.Bool False],
		Types.List [Types.Bool True]],
		return $ Types.Bool True),
	(Types.List [Types.Atom "cond"], defaultProgramStructErr)
		]
	where
		createIf cond ifClause thenClause = Types.List [
			Types.Atom "if", cond, ifClause, thenClause]

		createList list = Types.List [Types.Atom "quote", Types.List list]

		{-
		 - The `Error.ProgramStructure` error contains a lengthy, freeform
		 - explanatory message that doesn't make sense to check for equality
		 - with the expected error.
		 - -}
		defaultProgramStructErr = Error.throwError $ Error.ProgramStructure ""
		evalTest (input, expectedOutput) = HUnit.TestCase $ do
			env <- State.nullEnv
			res <- MonadError.runErrorT $ Interpreter.eval env input
			HUnit.assertEqual "" expectedOutput $ case res of
				(Left (Error.ProgramStructure _)) -> defaultProgramStructErr
				result -> result

testCoerceEquals = HUnit.TestList $
	map (\ (a, b, coercer, expected) -> HUnit.TestCase $
		HUnit.assertEqual "" (return expected) $
		Interpreter.coerceEquals a b $ coercer) [
		(Types.Number 10, Types.String "10",
			Interpreter.AnyCoercer Interpreter.coerceNum, True),
		(Types.Bool True, Types.Bool True,
			Interpreter.AnyCoercer Interpreter.coerceStr, True),
		(Types.Bool True, Types.Bool False,
			Interpreter.AnyCoercer Interpreter.coerceStr, False),
		(Types.List [], Types.Atom "foo",
			Interpreter.AnyCoercer Interpreter.coerceBool, False)]

tests = [testEval, testCoerceEquals]
