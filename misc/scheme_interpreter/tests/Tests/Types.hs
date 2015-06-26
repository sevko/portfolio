{-
 - Unit tests for `SchemeInterpreter.Types`.
 -}

module Tests.Types where

import qualified SchemeInterpreter.Types as Types
import qualified Test.HUnit as HUnit

testShow :: HUnit.Test
testShow = HUnit.TestList $
	map (\ (lispVal, expectedStr) ->
		HUnit.TestCase $ HUnit.assertEqual "" expectedStr $ show lispVal)
	testCases
	where testCases = [
		(Types.Atom "foo", "foo"),
		(Types.Number 1331, "1331"),
		(Types.Float 131.59, "131.59"),
		(Types.String "abc", "\"abc\""),
		(Types.Char 'a', "'a'"),
		(Types.Bool True, "#t"),
		(Types.List [
			Types.Char 'a', Types.Number 1, Types.String "fo"],
			"('a' 1 \"fo\")"),
		(Types.DottedList
			[Types.Float 2.0, Types.String "exe", Types.Bool False] $
			Types.Atom "bar",
			"(2.0 \"exe\" #f . bar)")]

tests = [testShow]
