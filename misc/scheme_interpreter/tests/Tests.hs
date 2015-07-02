import qualified Test.HUnit as HUnit
import qualified Tests.Parser as Parser
import qualified Tests.Types as Types
import qualified Tests.Interpreter as Interpreter
import qualified System.Exit as Exit

main :: IO ()
main = do
	let tests = HUnit.TestList $ concat [
		Parser.tests, Types.tests, Interpreter.tests]
	count <- HUnit.runTestTT tests

	-- Exit with a failing exit code when tests failed (otherwise, `cabal test`
	-- won't know).
	if HUnit.failures count > 0 then Exit.exitFailure else return ()
