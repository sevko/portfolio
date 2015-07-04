import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec.String
import Text.Parsec ((<|>))
import Control.Applicative ((<$), (*>), (<*))
import qualified Control.Applicative as Applicative
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Text.Printf as Printf

data JsonVal =
	JsonString String |
	JsonInt Int |
	JsonFloat Float |
	JsonObject [(String, JsonVal)] |
	JsonArray [JsonVal] |
	JsonBool Bool |
	JsonNull
	deriving (Show)

indent :: String -> String
indent = List.intercalate "\n" . map ('\t' :) . lines

joinComma :: [String] -> String
joinComma = List.intercalate ",\n"

prettyPrint :: JsonVal -> String
prettyPrint (JsonObject []) = "{}"
prettyPrint (JsonObject keyValPairs) =
	Printf.printf "{\n%s\n}" $ indent $ joinComma $ map pairToStr keyValPairs
	where pairToStr (key, val) = Printf.printf "\"%s\": %s" key $
		prettyPrint val
prettyPrint (JsonArray []) = "[]"
prettyPrint (JsonArray vals) = Printf.printf "[\n%s\n]" $
	indent $ joinComma $ map prettyPrint vals
prettyPrint val = show val

parseValue :: Parsec.String.Parser JsonVal
parseValue = Parsec.choice [
	parseString,
	parseNumber,
	parseObject,
	parseArray,
	parseBool,
	parseNull]

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
			(Parsec.char '\\' *> Parsec.anyChar >>= replaceEscapedChar) <|>
			Parsec.noneOf "\""

parseNumber :: Parsec.String.Parser JsonVal
parseNumber = do
	baseNumStr <- Applicative.liftA2 (++)
		(Parsec.option "" $ Parsec.string "-") parseDigits
	decimalStr <- Parsec.optionMaybe $ Parsec.char '.' *> parseDigits
	expStr <- Parsec.optionMaybe $ Parsec.oneOf "eE" *>
		((Applicative.liftA2 (:) (Parsec.char '-') parseDigits) <|>
		((Parsec.optional $ Parsec.char '+') *> parseDigits))
	let
		expNumMaybe = fmap read expStr
		applyExponent num = case expNumMaybe of
			Just expNum -> num * 10 ^ expNum
			Nothing -> num
	return $ case decimalStr of
		Just str -> JsonFloat $ applyExponent $ read $
			baseNumStr ++ ('.' : str)
		Nothing -> JsonInt $ applyExponent $ read baseNumStr
	where
		parseDigits = Parsec.many1 Parsec.digit

parseObject :: Parsec.String.Parser JsonVal
parseObject = do
	Parsec.char '{'
	Parsec.spaces
	vals <- Parsec.sepBy
		(Applicative.liftA2 (,) parseJsonString $
			Parsec.spaces *> Parsec.char ':' *> Parsec.spaces *> parseValue)
		(Parsec.try $ Parsec.spaces *> Parsec.char ',' *> Parsec.spaces)
	Parsec.spaces
	Parsec.char '}'
	return $ JsonObject vals
	where
		parseJsonString = fmap
			(\ jsonVal -> case jsonVal of
				JsonString str -> str
				_ -> error "Programmer error: failed to extract JsonString \
					\from parseString.")
			parseString

parseArray :: Parsec.String.Parser JsonVal
parseArray = do
	Parsec.char '['
	Parsec.spaces
	values <- Parsec.sepBy parseValue $
		Parsec.try $ Parsec.spaces *> Parsec.char ',' *> Parsec.spaces
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
	input <- getContents
	putStrLn $ case Parsec.parse parseValue "stdin" input of
		Right parsed -> prettyPrint parsed
		Left err -> show err
