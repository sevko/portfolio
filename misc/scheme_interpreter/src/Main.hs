import qualified SchemeInterpreter.Parser as Parser
import qualified System.Environment as Environment

main :: IO ()
main = do
	args <- Environment.getArgs
	case args of
		[str] -> print $ Parser.parseExpr str
		_ -> error "Usage: ./parser SCHEME_EXPR"
	{- print $ Parsec.parse Parser.parseNumber "" "#o100" -}
