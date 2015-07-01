{-
 - A Parsec parser for Scheme.
 -}

module SchemeInterpreter.Parser where

import qualified SchemeInterpreter.Types as Types

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Data.List as List
import qualified Control.Applicative as Applicative
import Control.Applicative ((*>), (<*))
import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error
import Text.ParserCombinators.Parsec ((<|>))

symbol :: Parsec.Parser Char
symbol = Parsec.oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parsec.Parser ()
spaces = Parsec.skipMany1 Parsec.space

parseList :: Parsec.Parser Types.LispVal
parseList = Monad.liftM Types.List $
	Parsec.char '(' *> Parsec.sepBy parser Parsec.spaces <* Parsec.char ')'

parseDottedList :: Parsec.Parser Types.LispVal
parseDottedList = do
	Parsec.char '('
	listHead <- Parsec.endBy parser Parsec.spaces
	Parsec.char '.'
	Parsec.spaces
	listTail <- parser
	Parsec.char ')'
	return $ Types.DottedList listHead listTail

parseExpr :: String -> Types.ThrowsError Types.LispVal
parseExpr str = case Parsec.parse parser "scheme" str of
	Left err -> Error.throwError $ Types.Parser err
	Right val -> return val

parser :: Parsec.Parser Types.LispVal
parser = Parsec.choice $ map Parsec.try [
	parseChar, parseString, parseFloat, parseNumber, parseAtom, parseList,
	parseDottedList]

parseChar :: Parsec.Parser Types.LispVal
parseChar = do
	_ <- Parsec.string "#\\"
	char <-
		(Parsec.try $ Parsec.string "newline" >> return '\n') <|>
		(Parsec.try $ Parsec.string "space" >> return ' ') <|>
		Parsec.anyChar
	return $ Types.Char char

parseString :: Parsec.Parser Types.LispVal
parseString = do
	_ <- Parsec.char '"'
	strContents <- Parsec.many $ parseCharSequence
	_ <- Parsec.char '"'
	return $ Types.String strContents
	where
		escapableChars = [
			('n', '\n'),
			('r', '\r'),
			('t', '\t'),
			('\\', '\\')]
		parseEscapableChars = Parsec.char '\\' >> (Parsec.choice $
			map (\ (escapedChar, replacement) ->
				Parsec.char escapedChar >> return replacement) escapableChars)

		parseCharSequence = parseEscapableChars <|> Parsec.noneOf "\""

parseAtom :: Parsec.Parser Types.LispVal
parseAtom = do
	first <- Parsec.letter <|> symbol
	rest <- Parsec.many (Parsec.letter <|> Parsec.digit <|> symbol)
	let atom = first : rest
	return $ case atom of
		"#t" -> Types.Bool True
		"#f" -> Types.Bool False
		_ -> Types.Atom atom

parseNumber :: Parsec.Parser Types.LispVal
parseNumber = Parsec.choice [
	parseNumberInBase "#o" Parsec.octDigit octToInt,
	parseNumberInBase "#x" Parsec.hexDigit hexToInt,
	parseNumberInBase "#b" (Parsec.oneOf "01") binToInt,
	(Parsec.optional (Parsec.string "#d") >> (Parsec.many1 Parsec.digit) >>=
		(return . Types.Number . read))]
	where
		{-
		 - A convenience function for interpreting a string with a certain
		 - prefix as a number in a corresponding base. (eg, "#b" -> binary)
		 -}
		parseNumberInBase strPrefix digitParser toIntDecoder = do
			numStr <- (Parsec.try $
				(Parsec.string strPrefix >> (Parsec.many1 digitParser)))
			return $ Types.Number $ case toIntDecoder numStr of
				Just num -> num
				Nothing -> error $
					"Implementation error: the parser passed a string (" ++
					numStr ++
					") containing an invalid digit to an integer decoder."

		{-
		 - The following functions concern converting strings representing
		 - numbers in a certain base to Integers.
		 -}
		strToInt :: Int -> (Char -> Maybe Int) -> String -> Maybe Integer
		strToInt base digitToInt =
			fmap digitsToInt . sequence . map digitToInt
			where
				digitsToInt :: [Int] -> Integer
				digitsToInt digits = sum $ zipWith
					(\ digit place -> (fromIntegral digit) *
						(fromIntegral base) ^ place)
					(reverse digits) [0..]

		binToInt :: String -> Maybe Integer
		binToInt = strToInt 2 binDigitToInt
			where
				binDigitToInt '0' = Just 0
				binDigitToInt '1' = Just 1
				binDigitToInt _ = Nothing

		hexToInt :: String -> Maybe Integer
		hexToInt = strToInt 16
			(\ digit -> List.elemIndex digit $ ['0'..'9'] ++ ['a'..'f'])

		octToInt :: String -> Maybe Integer
		octToInt = strToInt 8 (\ digit -> List.elemIndex digit ['0'..'7'])

(<++>) :: Applicative.Applicative a => a String -> a String -> a String
(<++>) = Applicative.liftA2 (++)

parseQuoted :: Parsec.Parser Types.LispVal
parseQuoted = do
	str <- Parsec.char '\'' *> parser
	return $ Types.List [Types.Atom "quote", str]

parseFloat :: Parsec.Parser Types.LispVal
parseFloat =
	Parsec.many1 Parsec.digit <++>
	(fmap (:"") $ Parsec.char '.') <++>
	Parsec.many1 Parsec.digit >>=
	(return . Types.Float . read)
