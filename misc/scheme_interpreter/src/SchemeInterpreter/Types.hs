module SchemeInterpreter.Types where

import qualified Text.Format as Format

data LispVal =
	Atom String |
	Number Integer |
	Float Float |
	String String |
	Char Char |
	Bool Bool |
	List [LispVal] |
	DottedList [LispVal] LispVal
	deriving (Eq)

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
