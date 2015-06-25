{-
 - Unit tests for `SchemeInterpreter.Parser`.
 -}

module Tests.Parser where

import qualified SchemeInterpreter.Parser as Parser
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Test.HUnit as HUnit

testParseNumber = HUnit.TestLabel "parseNumber" $ HUnit.TestList [
	HUnit.TestLabel "valid input strings" $ HUnit.TestList validTests,
	HUnit.TestLabel "invalid input strings" $ HUnit.TestList invalidTests]
	where
		parseStr = Parsec.parse Parser.parseNumber ""

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

tests = HUnit.TestList [testParseNumber]
