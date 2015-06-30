import qualified SchemeInterpreter.Parser as Parser
import qualified SchemeInterpreter.Interpreter as Interpreter
import qualified SchemeInterpreter.State as State

import qualified System.IO as IO
import qualified Control.Monad as Monad

runRepl :: IO ()
runRepl = State.nullEnv >>= repl
	where
		repl env = do
			putStr "> "
			IO.hFlush IO.stdout
			input <- getLine
			if input == "quit"
				then return ()
				else do
					evalString env input >>= print
					repl env

		evalString env expr = State.runIOThrows $ Monad.liftM show $
			(State.liftThrows $ Parser.parseExpr expr) >>= Interpreter.eval env

main :: IO ()
main = runRepl
