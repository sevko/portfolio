{-
 - Tests for `SchemeInterpreter.Interpreter`.
 -}

module Tests.Interpreter where

import qualified SchemeInterpreter.Interpreter as Interpreter
import qualified SchemeInterpreter.Error as Error
import qualified SchemeInterpreter.Types as Types
import qualified Test.HUnit as HUnit

testEval :: HUnit.Test
testEval = HUnit.TestList $ map evalTest [
	(Types.String "abc", Right $ Types.String "abc"),
	(Types.Number 9, Right $ Types.Number 9),
	(createIf (Types.Bool True) (Types.Number 1) (Types.Number 2),
		Right $ Types.Number 1),
	(createIf (Types.Bool False) (Types.Number 1) (Types.Number 2),
		Right $ Types.Number 2),
	(createIf (Types.Number 1) (Types.Number 1) (Types.Number 2),
		Left $ Error.TypeMismatch "if condition must evaluate to a boolean" $
			Types.Number 1),
	(Types.List [
		Types.Atom "+", Types.Number 1, Types.Number 2],
		Right $ Types.Number 3),
	(Types.List [
		Types.Atom "+",
		Types.List[Types.Atom "+", Types.Number 1, Types.Number 2],
		Types.Number 4,
		Types.List[Types.Atom "-", Types.Number 10, Types.Number 2]],
		Right $ Types.Number 15),
	(Types.List [
		Types.Atom "+",
		Types.Number 3],
		Left $ Error.NumArgs 2 [Types.Number 3]),
	(Types.List [
		Types.Atom "string?",
		Types.String "a",
		Types.String "b"],
		Left $ Error.NumArgs 1 [Types.String "a", Types.String "b"]),
	(Types.List [
		Types.Atom "car",
		Types.Number 1,
		Types.Number 4],
		Left $ Error.NumArgs 1 [Types.Number 1, Types.Number 4]),
	(Types.List [Types.Atom "car", createList []],
		Left $ Error.TypeMismatch "list with one or more elements" $
			Types.List []),
	(Types.List [Types.Atom "car", createList $ map Types.Number [1, 2, 3]],
		Right $ Types.Number 1),
	(Types.List [
		Types.Atom "cdr",
		createList $ map Types.Number [1, 2, 3]],
		Right $ Types.List [Types.Number 2, Types.Number 3]),
	(Types.List [Types.Atom "cons", Types.Number 1, createList []],
		Right $ Types.List [Types.Number 1]),
	(Types.List [Types.Atom "cons", createList [], Types.String "foo"],
		Right $ Types.DottedList [Types.List []] $ Types.String "foo"),
	(Types.List [
		Types.Atom "cons", createList [], Types.Number 1, Types.String "foo"],
		Left $ Error.NumArgs 2 [
			Types.List [], Types.Number 1, Types.String "foo"]),
	(Types.List [Types.Atom "eqv?", Types.Number 1, Types.Number 1],
		Right $ Types.Bool $ True),
	(Types.List [Types.Atom "eqv?", Types.Number 1, Types.Number 5],
		Right $ Types.Bool $ False),
	(Types.List [Types.Atom "eq?", createList [], createList [Types.Number 1]],
		Right $ Types.Bool $ False),
	(let list = createList [Types.String "abc def"] in
		Types.List [Types.Atom "eqv?", list, list],
		Right $ Types.Bool $ True),
	(Types.List [
		Types.Atom "eqv?",
		Types.DottedList [Types.Number 10, Types.String "abc def"] $
			Types.String "foo",
		Types.DottedList [Types.Number 10, Types.String "abc def"] $
			Types.String "foa"],
		Right $ Types.Bool $ False),
	(Types.List [
		Types.Atom "cond",
		Types.List [Types.Bool False, Types.Number 1, Types.Number 2],
		Types.List [Types.Bool True, Types.Number 1, Types.Number 3]],
		Right $ Types.Number 3),
	(Types.List [
		Types.Atom "cond",
		Types.List [Types.Bool False],
		Types.List [Types.Bool True]],
		Right $ Types.Bool True),
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
		defaultProgramStructErr = Left $ Error.ProgramStructure ""
		evalTest (input, expectedOutput) = HUnit.TestCase $
			HUnit.assertEqual "" expectedOutput $
			case Interpreter.eval input of
				{-
				 - -}
				(Left (Error.ProgramStructure _)) -> defaultProgramStructErr
				result -> result

testCoerceEquals = HUnit.TestList $
	map (\ (a, b, coercer, expected) -> HUnit.TestCase $
		HUnit.assertEqual "" (Right expected) $
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
