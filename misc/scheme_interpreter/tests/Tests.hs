import qualified Test.HUnit as HUnit
import qualified Tests.Parser as Parser
import qualified System.Exit as Exit

main :: IO ()
main = do
	count <- HUnit.runTestTT Parser.tests

	-- Exit with a failing exit code when tests failed (otherwise, `cabal test`
	-- won't know).
	if HUnit.failures count > 0 then Exit.exitFailure else return ()
