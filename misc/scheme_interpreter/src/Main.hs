import qualified SchemeInterpreter.Parser as Parser
import qualified SchemeInterpreter.Interpreter as Interpreter
import qualified System.IO as IO
import qualified Control.Monad as Monad

repl :: IO ()
repl = do
	IO.hSetBuffering IO.stdout IO.NoBuffering
	Monad.forever $ do
		putStr "> "
		input <- getLine
		either print print $ Parser.parseExpr input >>= Interpreter.eval

main :: IO ()
main = repl
