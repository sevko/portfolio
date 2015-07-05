import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec.Error
import Text.Parsec ((<|>), (<?>))
import Control.Applicative ((<$), (*>), (<*))
import qualified Control.Applicative as Applicative
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Text.Printf as Printf

type Parser = Parsec.Parsec String ()
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
prettyPrint (JsonString str) = show str
prettyPrint (JsonInt int) = show int
prettyPrint (JsonFloat float) = show float
prettyPrint (JsonBool True) = "true"
prettyPrint (JsonBool False) = "false"
prettyPrint JsonNull = "null"

parseValue :: Parser JsonVal
parseValue = Parsec.choice [
	parseString,
	parseNumber,
	parseObject,
	parseArray,
	parseBool,
	parseNull] <?> "JSON value (string, number, \
		\dictionary, array, boolean, or null)"

parseString :: Parser JsonVal
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

parseNumber :: Parser JsonVal
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

parseObject :: Parser JsonVal
parseObject = fmap JsonObject $ Parsec.char '{' *> Parsec.spaces *>
	parsePairs <* Parsec.spaces <* Parsec.char '}'
	where
		parsePairs = Parsec.sepBy
			(Applicative.liftA2 (,) parseKey $
				Parsec.spaces *> Parsec.char ':' *> Parsec.spaces *>
				parseValue)
			(Parsec.try $ Parsec.spaces *> Parsec.char ',' *> Parsec.spaces)

		parseKey = fmap
			(\ jsonVal -> case jsonVal of
				JsonString str -> str
				_ -> error "Programmer error: failed to extract JsonString \
					\from parseString.")
			parseString

parseArray :: Parser JsonVal
parseArray = fmap JsonArray $ Parsec.char '[' *> Parsec.spaces *>
	parseValues <* Parsec.spaces <* Parsec.char ']'
	where parseValues = Parsec.sepBy parseValue $
		Parsec.try $ Parsec.spaces *> Parsec.char ',' *> Parsec.spaces

parseBool :: Parser JsonVal
parseBool = fmap JsonBool $
	False <$ Parsec.string "false" <|>
	True <$ Parsec.string "true"

parseNull :: Parser JsonVal
parseNull = JsonNull <$ Parsec.string "null"

formatError :: String -> Parsec.ParseError -> String
formatError inputStr err = let
	sourcePos = Parsec.errorPos err
	lineNum = Parsec.sourceLine sourcePos
	colNum = Parsec.sourceColumn sourcePos
	numContextLines = 5
	contextLines = unlines $ map ('\t':) $ take (min numContextLines lineNum) $
		drop (lineNum - numContextLines) $ lines inputStr
	colPointerLine = '\t' : (replicate (colNum - 1) ' ') ++ "^"
	errMsg = Parsec.Error.showErrorMessages
		"or" "unknown parse error" "expecting" "unexpected"
		"end of input" $ Parsec.Error.errorMessages err
	in Printf.printf
		"Parser error on line %d, column %d:\n\n%s%s\nError:%s"
		lineNum colNum contextLines colPointerLine errMsg

main :: IO ()
main = do
	input <- getContents
	putStrLn $ case Parsec.parse parseValue "stdin" input of
		Right parsed -> prettyPrint parsed
		Left err -> formatError input err
