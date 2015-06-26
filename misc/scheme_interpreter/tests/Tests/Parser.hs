{-
 - Unit tests for `SchemeInterpreter.Parser`.
 -}

module Tests.Parser where

import qualified SchemeInterpreter.Parser as Parser
import qualified SchemeInterpreter.Types as Types
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
				(Right $ Types.Number expectedNum) $ parseStr str)
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
		(Right $ Types.String "a\nb\tc\rd\\e")
		((applyParser Parser.parseString) "\"a\\nb\\tc\\rd\\\\e\"")

testParseChar :: HUnit.Test
testParseChar = createTestList Parser.parseChar Types.Char [
	("#\\c", 'c'),
	("#\\n", 'n'),
	("#\\_", '_'),
	("#\\ ", ' '),
	("#\\space", ' '),
	("#\\newline", '\n')]

testParseFloat :: HUnit.Test
testParseFloat = createTestList Parser.parseFloat Types.Float [
	("131.11", 131.11),
	("131.0", 131.0)]

testParseList :: HUnit.Test
testParseList = createTestList Parser.parseList Types.List [
	("(#\\a #\\b #\\c)", map Types.Char ['a', 'b', 'c']),
	("(#\\a 9)", [Types.Char 'a', Types.Number 9]),
	("(\"abc\" #\\a 9 \"def\" #\\e)", [
		Types.String "abc",
		Types.Char 'a',
		Types.Number 9,
		Types.String "def",
		Types.Char 'e']),
	("(9.0)", [Types.Float 9.0])]

testParseDottedList :: HUnit.Test
testParseDottedList = createTestList Parser.parseDottedList
	(\ (a, b) -> Types.DottedList a b) [
		(
			"(#\\a #\\b . #\\c)",
			(map Types.Char ['a', 'b'], Types.Char 'c')),
		("(#\\a . 9)", ([Types.Char 'a'], Types.Number 9)),
		("(\"abc\" #\\a 9 \"def\" . #\\e)", ([
			Types.String "abc",
			Types.Char 'a',
			Types.Number 9,
			Types.String "def"],
			Types.Char 'e')),
		("(. 9.0)", ([], Types.Float 9.0))]

tests = [
	testParseNumber, testParseString, testParseChar, testParseFloat,
	testParseList, testParseDottedList]
