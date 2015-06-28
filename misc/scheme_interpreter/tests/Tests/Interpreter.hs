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
			Types.List [], Types.Number 1, Types.String "foo"])
		]
	where
		createIf cond ifClause thenClause = Types.List [
			Types.Atom "if", cond, ifClause, thenClause]

		createList list = Types.List [Types.Atom "quote", Types.List list]

		evalTest (input, expectedOutput) = HUnit.TestCase $
			HUnit.assertEqual "" expectedOutput $
			Interpreter.eval input

tests = [testEval]
