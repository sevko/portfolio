{-# LANGUAGE NoMonomorphismRestriction #-}

{-
 - Types the interpreter uses to signify parsing/evaluation errors.
 -}

module SchemeInterpreter.Error where

import qualified Control.Monad.Error as Error
import qualified SchemeInterpreter.Types as Types
import qualified Text.Format as Format
import qualified Text.ParserCombinators.Parsec as Parsec

data LispError =
	NumArgs Integer [Types.LispVal] |
	TypeMismatch String Types.LispVal |
	Parser Parsec.ParseError |
	BadSpecialForm String Types.LispVal |
	NotFunction String String |
	UnboundVar String String |
	ProgramStructure String |
	Default String deriving (Eq)

type ThrowsError = Either LispError

instance Show LispError where
	show (UnboundVar msg varName) = msg ++ ": " ++ varName
	show (BadSpecialForm msg form) = msg ++ ": " ++ show form
	show (NotFunction msg funcName) = msg ++ ": " ++ funcName
	show (NumArgs numExpected args) = Format.format
		"Expected {0} args, but got {1} ({2})" [
			show numExpected,
			show $ length args,
			show args
		]
	show (TypeMismatch expected got) = Format.format
		"Expecting type {0}, but got {1}." [expected, show got]
	show (Parser parseErr) = "Parse error: " ++ show parseErr
	show (ProgramStructure err) = "Program structure error: " ++ err

instance Error.Error LispError where
	noMsg = Default "An error has occurred"
	strMsg = Default

throwError = Error.throwError
catchError = Error.catchError
