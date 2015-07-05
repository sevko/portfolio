{-
 - A JSON parser written with Parsec.
 -}

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec.Error
import Text.Parsec ((<|>), (<?>))
import Control.Applicative ((<$), (*>), (<*))
import qualified Control.Applicative as Applicative
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Text.Printf as Printf
import qualified System.Environment as Environment

type Parser = Parsec.Parsec String ()

-- A type to represent all of the basic JSON types.
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
parseValue = Parsec.spaces *>
	Parsec.choice [
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

{-
 - Converts a Parsec error into a fairly pretty string, which contains the
 - useful diagnostic information that Parsec provides and also a visual
 - representation of the location of the error (that is, the line on which the
 - error occurred as well as several lines before it to provide context), and
 - other goodies.
 -}
formatError :: String -> Parsec.ParseError -> String
formatError inputStr err = let
	sourcePos = Parsec.errorPos err
	lineNum = Parsec.sourceLine sourcePos
	colNum = Parsec.sourceColumn sourcePos

	-- Grab the line on which the error occurred, as well as several lines
	-- preceding it to provide sufficient context for the user.
	numContextLines = 5
	contextLines = take (min numContextLines lineNum) $
		drop (lineNum - numContextLines) $ lines inputStr

	-- Prepend each line with its line number.
	lineNumLen = length $ show lineNum
	lnFormatStr = "\t%" ++ show lineNumLen ++ "d |%s"
	lnNumbers = [lineNum - length contextLines + 1..]

	-- Format the context lines, replacing all tabs used for indentation with
	-- spaces for the reasons listed in the documentation comment for
	-- `replaceTabsWithSpaces`.
	contextLinesStr = unlines $
		zipWith (Printf.printf lnFormatStr) lnNumbers $
		map replaceTabsWithSpaces contextLines

	-- Recompute the column number, since we've replaced tabs with spaces.
	newColNum = (getNewColNum (last contextLines) 0 0 colNum) + lineNumLen + 2
	colPointerLine = '\t' : (replicate (newColNum - 1) ' ') ++ "^"

	-- The following was taken straight from Parsec's source code.
	errMsg = Parsec.Error.showErrorMessages
		"or" "unknown parse error" "expecting" "unexpected"
		"end of input" $ Parsec.Error.errorMessages err
	in Printf.printf
		"Parser error on line %d, column %d:\n\n%s%s\nError:%s"
		lineNum colNum contextLinesStr colPointerLine errMsg
	where
		tabSpaces = "    "
		tabSpacesLength = length tabSpaces

		{-
		 - Since the context lines are both indented and prefixed with
		 - the line number, any tabs in one such line might not take up a full
		 - 8 characters when printed. Thus, for proper formatting, we convert
		 - tabs to spaces.
		 -}
		replaceTabsWithSpaces = concat .
			map (\ chr -> if chr == '\t' then tabSpaces else chr : "")

		{-
		 - Since we convert tabs to spaces, we need to recompute the character
		 - column where the parse error occurred (this is a bit tricky because
		 - Parsec counts tabs as 8 characters).
		 -}
		getNewColNum "" a b c = error $ Printf.printf "%d %d %d" a b c
		getNewColNum (char:restOfLn) newColAccum parsecColAccum parsecCol =
			let
				isTab = char == '\t'
				parsecColAccum' = parsecColAccum +
					(if isTab then 8 - (mod parsecColAccum 8) else 1)
				newColAccum' = newColAccum +
					(if isTab then tabSpacesLength else 1)
			in if parsecColAccum' == parsecCol
				then newColAccum'
				else getNewColNum restOfLn newColAccum' parsecColAccum' parsecCol

main :: IO ()
main = do
	let usageMsg = "`json_parser file.json` or `json_parser < file.json`."
	args <- Environment.getArgs
	case args of
		[] -> putStrLn "Enter JSON below:" >> getContents >>= parseJsonString
		["--help"] -> putStrLn usageMsg
		[filePath] -> readFile filePath >>= parseJsonString
		_ -> error $ "Invalid args! " ++ usageMsg
	where parseJsonString str = putStrLn $
		case Parsec.parse parseValue "stdin" str of
			Right parsed -> "Successfully parsed: " ++ prettyPrint parsed
			Left err -> formatError str err
