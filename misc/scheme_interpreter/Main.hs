module Main where

import qualified System.Environment as Environment
import qualified Text.ParserCombinators.Parsec as Parsec
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
parseExpr = Parsec.choice [parseAtom, parseString, parseNumber]

parseString :: Parsec.Parser LispVal
parseString = do
	Parsec.char '"'
	strContents <- Parsec.many $ parseCharSequence
	Parsec.char '"'
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
parseNumber = Parsec.many1 Parsec.digit >>= (return . Number . read)

main :: IO ()
main = do
	args <- Environment.getArgs
	case args of
		[expr] -> putStrLn $ readExpr expr
		_ -> error "Usage: ./parser SCHEME_EXPR"
