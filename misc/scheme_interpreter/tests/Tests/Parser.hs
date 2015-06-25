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

testParseString = HUnit.TestLabel "parses escaped characters" $
	HUnit.TestCase $ HUnit.assertEqual ""
		(Right $ Parser.String "a\nb\tc\rd\\e")
		((applyParser Parser.parseString) "\"a\\nb\\tc\\rd\\\\e\"")

testParseChar = HUnit.TestLabel "parses chars correctly" $
	HUnit.TestList charTests
	where
		charTestCases = [
			("#\\c", 'c'),
			("#\\n", 'n'),
			("#\\_", '_'),
			("#\\ ", ' '),
			("#\\space", ' '),
			("#\\newline", '\n')]

		charTests = map (\ (inputStr, expectedChar) ->
			HUnit.TestCase $
			HUnit.assertEqual ""
				(Right $ Parser.Char expectedChar)
				(applyParser Parser.parseChar inputStr))
			charTestCases

testParseFloat = HUnit.TestLabel "parses floats" $
	HUnit.TestList tests
	where
		testCases = [
			("131.11", 131.11),
			("131.0", 131.0)]

		tests = map (\ (inputStr, expectedFloat) ->
			HUnit.TestCase $
			HUnit.assertEqual ""
				(Right $ Parser.Float expectedFloat)
				(applyParser Parser.parseFloat inputStr))
			testCases

testParseList = HUnit.TestList tests
	where
		testCases = [
			("#\\a #\\b #\\c", map Parser.Char ['a', 'b', 'c']),
			("#\\a 9", [Parser.Char 'a', Parser.Number 9]),
			("\"abc\" #\\a 9 \"def\" #\\e", [
				Parser.String "abc",
				Parser.Char 'a',
				Parser.Number 9,
				Parser.String "def",
				Parser.Char 'e']),
			("9.0", [Parser.Float 9.0])
			]

		tests = map (\ (inputStr, expectedList) ->
			HUnit.TestCase $
			HUnit.assertEqual ""
				(Right $ Parser.List expectedList)
				(applyParser Parser.parseList inputStr))
			testCases

testParseDottedList = HUnit.TestList tests
	where
		testCases = [
			(
				"#\\a #\\b . #\\c",
				(map Parser.Char ['a', 'b'], Parser.Char 'c')),
			("#\\a . 9", ([Parser.Char 'a'], Parser.Number 9)),
			("\"abc\" #\\a 9 \"def\" . #\\e", ([
				Parser.String "abc",
				Parser.Char 'a',
				Parser.Number 9,
				Parser.String "def"],
				Parser.Char 'e')),
			(". 9.0", ([], Parser.Float 9.0))
			]

		tests = map (\ (inputStr, (expectedHead, expectedTail)) ->
			HUnit.TestCase $
			HUnit.assertEqual ""
				(Right $ Parser.DottedList expectedHead expectedTail)
				(applyParser Parser.parseDottedList inputStr))
			testCases

tests = HUnit.TestList [
	testParseNumber, testParseString, testParseChar, testParseFloat,
	testParseList, testParseDottedList]
