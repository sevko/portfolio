module Main where

import qualified System.Environment as Environment
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Data.List as List
import Text.ParserCombinators.Parsec ((<|>))

data LispVal =
	Atom String |
	List [LispVal] |
	DottedList [LispVal] LispVal |
	Number Integer |
	String String |
	Bool Bool deriving (Show)

symbol :: Parsec.Parser Char
symbol = Parsec.oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parsec.Parser ()
spaces = Parsec.skipMany1 Parsec.space

readExpr :: String -> String
readExpr input = case Parsec.parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> case val of
		String str -> "Found string: " ++ str
		_ -> "Found: " ++ show val

parseExpr :: Parsec.Parser LispVal
parseExpr = Parsec.choice [parseString, parseNumber, parseAtom]

parseString :: Parsec.Parser LispVal
parseString = do
	_ <- Parsec.char '"'
	strContents <- Parsec.many $ parseCharSequence
	_ <- Parsec.char '"'
	return $ String strContents
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

parseAtom :: Parsec.Parser LispVal
parseAtom = do
	first <- Parsec.letter <|> symbol
	rest <- Parsec.many (Parsec.letter <|> Parsec.digit <|> symbol)
	let atom = first : rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_ -> Atom atom

parseNumber :: Parsec.Parser LispVal
parseNumber = Parsec.choice [
	parseNumberInBase "#o" Parsec.octDigit octToInt,
	parseNumberInBase "#x" Parsec.hexDigit hexToInt,
	parseNumberInBase "#b" (Parsec.oneOf "01") binToInt,
	(Parsec.optional (Parsec.string "#d") >> (Parsec.many1 Parsec.digit) >>=
		(return . Number . read))]
	where parseNumberInBase strPrefix digitParser toIntDecoder = do
		numStr <- (Parsec.try $ (Parsec.string strPrefix >> (Parsec.many1 digitParser)))
		return $ Number $ case toIntDecoder numStr of
			Just num -> num
			Nothing -> error $
				"Implementation error: the parser passed a string (" ++
				numStr ++
				") containing an invalid digit to an integer decoder."

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

main :: IO ()
main = do
	args <- Environment.getArgs
	case args of
		[expr] -> putStrLn $ readExpr expr
		_ -> error "Usage: ./parser SCHEME_EXPR"
	print $ binToInt "100"
	print $ hexToInt "7b"
	print $ octToInt "131"
