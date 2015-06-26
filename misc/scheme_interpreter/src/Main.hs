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
		putStrLn $ case Parser.parseExpr input of
			Right expr -> show $ Interpreter.eval expr
			Left err -> show $ err

main :: IO ()
main = repl
