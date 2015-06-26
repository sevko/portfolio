{-
 - Unit tests for `SchemeInterpreter.Parser`.
 -}

module Tests.Parser where

import qualified SchemeInterpreter.Parser as Parser
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Test.HUnit as HUnit

{-
 - Convenience function for applying a certain parser to a string.
 -}
applyParser parser = Parsec.parse parser ""

{-
 - Convenience function for creating a list of test-cases from a list of tuples
 - containing an input string and expected value.
 -}
createTestList parser createLispVal testCases =
	HUnit.TestList $ map (\ (inputStr, expectedVal) ->
		HUnit.TestCase $
		HUnit.assertEqual ""
			(Right $ createLispVal expectedVal)
			(applyParser parser inputStr)) testCases

testShow :: HUnit.Test
testShow = HUnit.TestList $
	map (\ (lispVal, expectedStr) ->
		HUnit.TestCase $ HUnit.assertEqual "" expectedStr $ show lispVal)
	testCases
	where testCases = [
		(Parser.Atom "foo", "foo"),
		(Parser.Number 1331, "1331"),
		(Parser.Float 131.59, "131.59"),
		(Parser.String "abc", "\"abc\""),
		(Parser.Char 'a', "'a'"),
		(Parser.Bool True, "#t"),
		(Parser.List [
			Parser.Char 'a', Parser.Number 1, Parser.String "fo"],
			"('a' 1 \"fo\")"),
		(Parser.DottedList
			[Parser.Float 2.0, Parser.String "exe", Parser.Bool False] $
			Parser.Atom "bar",
			"(2.0 \"exe\" #f . bar)")]

testParseNumber :: HUnit.Test
testParseNumber = HUnit.TestLabel "parseNumber" $ HUnit.TestList [
	HUnit.TestLabel "valid input strings" $ HUnit.TestList validTests,
	HUnit.TestLabel "invalid input strings" $ HUnit.TestList invalidTests]
	where
		parseStr = applyParser Parser.parseNumber

		validNumbers = [
			("#o100", 64),
			("#o221", 145),
			("#x78e7", 30951),
			("#x65", 101),
			("#b111100011", 483),
			("#d939", 939),
			("939", 939)]
		validTests = map (\ (str, expectedNum) ->
			HUnit.TestCase $ HUnit.assertEqual ""
				(Right $ Parser.Number expectedNum) $ parseStr str)
			validNumbers

		isLeft (Left _) = True
		isLeft (Right _) = False

		invalidNumbers = ["#ba", "#xxx", "a", "#o9", "#bz1010"]
		invalidTests = map
			(HUnit.TestCase . HUnit.assertBool "" . isLeft . parseStr)
			invalidNumbers

testParseString :: HUnit.Test
testParseString = HUnit.TestLabel "parses escaped characters" $
	HUnit.TestCase $ HUnit.assertEqual ""
		(Right $ Parser.String "a\nb\tc\rd\\e")
		((applyParser Parser.parseString) "\"a\\nb\\tc\\rd\\\\e\"")

testParseChar :: HUnit.Test
testParseChar = createTestList Parser.parseChar Parser.Char [
	("#\\c", 'c'),
	("#\\n", 'n'),
	("#\\_", '_'),
	("#\\ ", ' '),
	("#\\space", ' '),
	("#\\newline", '\n')]

testParseFloat :: HUnit.Test
testParseFloat = createTestList Parser.parseFloat Parser.Float [
	("131.11", 131.11),
	("131.0", 131.0)]

testParseList :: HUnit.Test
testParseList = createTestList Parser.parseList Parser.List [
	("(#\\a #\\b #\\c)", map Parser.Char ['a', 'b', 'c']),
	("(#\\a 9)", [Parser.Char 'a', Parser.Number 9]),
	("(\"abc\" #\\a 9 \"def\" #\\e)", [
		Parser.String "abc",
		Parser.Char 'a',
		Parser.Number 9,
		Parser.String "def",
		Parser.Char 'e']),
	("(9.0)", [Parser.Float 9.0])]

testParseDottedList :: HUnit.Test
testParseDottedList = createTestList Parser.parseDottedList
	(\ (a, b) -> Parser.DottedList a b) [
		(
			"(#\\a #\\b . #\\c)",
			(map Parser.Char ['a', 'b'], Parser.Char 'c')),
		("(#\\a . 9)", ([Parser.Char 'a'], Parser.Number 9)),
		("(\"abc\" #\\a 9 \"def\" . #\\e)", ([
			Parser.String "abc",
			Parser.Char 'a',
			Parser.Number 9,
			Parser.String "def"],
			Parser.Char 'e')),
		("(. 9.0)", ([], Parser.Float 9.0))]

tests = HUnit.TestList [
	testParseNumber, testParseString, testParseChar, testParseFloat,
	testParseList, testParseDottedList, testShow]
