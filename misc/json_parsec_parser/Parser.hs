import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec.String
import Text.Parsec ((<|>))
import Control.Applicative ((<$), (*>), (<*))
import qualified Control.Applicative as Applicative
import qualified Data.List as List
import qualified System.Environment as Environment
import qualified Data.Char as Char
import qualified Text.Printf as Printf

data JsonVal =
	JsonString String |
	JsonNumber Int |
	JsonObject [(String, JsonVal)] |
	JsonArray [JsonVal] |
	JsonBool Bool |
	JsonNull
	deriving (Show)

parseValue :: Parsec.String.Parser JsonVal
parseValue = Parsec.choice [
	parseString,
	parseNumber,
	parseObject,
	parseArray,
	parseBool,
	parseNull
	]

parseString :: Parsec.String.Parser JsonVal
parseString = fmap JsonString $
	Parsec.char '"' *> Parsec.many charParser <* Parsec.char '"'
	where
		escapedCharReplacements = [
			('"', '"'),
			('\\', '\\'),
			('/', '/'),
			('b', '\b'),
			('f', '\f'),
			('n', '\n'),
			('r', '\r'),
			('t', '\t')]

		replaceEscapedChar escapedChar =
			if escapedChar == 'x'
				then fmap (Char.chr . read . (++) "0x") $
					Parsec.count 4 Parsec.hexDigit
				else let replacement = List.find
						(\ (chr, _) -> chr == escapedChar)
						escapedCharReplacements
					in case replacement of
						Just (_, chr) -> return chr
						Nothing -> fail $ Printf.printf
							"`%c` is not an escapable character" escapedChar

		charParser =
			(Parsec.char '\\' >> Parsec.anyChar >>= replaceEscapedChar) <|>
			Parsec.noneOf "\""

parseNumber :: Parsec.String.Parser JsonVal
parseNumber = fmap (JsonNumber . read) $ Parsec.many1 Parsec.digit

parseObject :: Parsec.String.Parser JsonVal
parseObject = do
	Parsec.char '{'
	Parsec.spaces
	vals <- Parsec.sepBy
		(Applicative.liftA2 (,) parseJsonString $
			Parsec.spaces *> Parsec.char ':' *> Parsec.spaces *> parseValue)
		(Parsec.spaces >> Parsec.char ',' >> Parsec.spaces)
	Parsec.spaces
	Parsec.char '}'
	return $ JsonObject vals
	where
		parseJsonString = fmap
			(\ jsonVal -> case jsonVal of
				JsonString str -> str
				_ -> error "Programmer error: failed to extract JsonString\
					\from parseString.")
			parseString

parseArray :: Parsec.String.Parser JsonVal
parseArray = do
	Parsec.char '['
	Parsec.spaces
	values <- Parsec.sepBy parseValue $
		Parsec.spaces *> Parsec.char ',' *> Parsec.spaces
	Parsec.spaces
	Parsec.char ']'
	return $ JsonArray values

parseBool :: Parsec.String.Parser JsonVal
parseBool = fmap JsonBool $
	False <$ Parsec.string "false" <|>
	True <$ Parsec.string "true"

parseNull :: Parsec.String.Parser JsonVal
parseNull = JsonNull <$ Parsec.string "null"

main :: IO ()
main = do
	{- let [(a, _)] = Char.readLitChar "\\0394" -}
	{- putStrLn $ a : "" -}
	{- putStrLn $ (Char.chr 394) : "" -}
	{-  "-" -}
	[expr] <- Environment.getArgs
	print $ Parsec.parse parseValue "" expr
