{-# LANGUAGE NoMonomorphismRestriction #-}

{-
 - Definitions of types to represent Scheme types.
 -}

module SchemeInterpreter.Types where

import qualified Text.Format as Format
import qualified Control.Monad.Error as MonadError
import qualified Data.IORef as IORef
import qualified Text.ParserCombinators.Parsec as Parsec

data LispVal =
	Atom String |
	Number Integer |
	Float Float |
	String String |
	Char Char |
	Bool Bool |
	List [LispVal] |
	DottedList [LispVal] LispVal |
	PrimitiveFunc ([LispVal] -> ThrowsError LispVal) |
	Func {
		params :: [String],
		varargs :: Maybe String,
		body :: [LispVal],
		closure :: Env
	}

instance Eq LispVal where
	(Atom a) == (Atom b) = a == b
	(Number a) == (Number b) = a == b
	(Float a) == (Float b) = a == b
	(String a) == (String b) = a == b
	(Char a) == (Char b) = a == b
	(Bool a) == (Bool b) = a == b
	(List a) == (List b) = a == b
	(DottedList a b) == (DottedList c d) = a == c && b == d
	_ == _ = False

instance Show LispVal where
	show (Atom atom) = atom
	show (Number number) = show number
	show (Float float) = show float
	show (String string) = '"' : string ++ "\""
	show (Char char) = show char
	show (Bool True) = "#t"
	show (Bool False) = "#f"
	show (List list) = '(' : (unwords $ map show list) ++ ")"
	show (DottedList head' tail') = Format.format "({0} . {1})"
		[unwords $ map show head', show tail']

data LispError =
	NumArgs Integer [LispVal] |
	TypeMismatch String LispVal |
	Parser Parsec.ParseError |
	BadSpecialForm String LispVal |
	NotFunction String String |
	UnboundVar String String |
	ProgramStructure String |
	Default String deriving (Eq)

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
	show (Default err) = "Program structure error: " ++ err

instance MonadError.Error LispError where
	noMsg = Default "An error has occurred"
	strMsg = Default

type ThrowsError = Either LispError
type Env = IORef.IORef [(String, IORef.IORef LispVal)]
type IOThrowsError = MonadError.ErrorT LispError IO

throwError = MonadError.throwError
catchError = MonadError.catchError
