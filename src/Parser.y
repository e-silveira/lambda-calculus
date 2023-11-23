{
module Parser where
import Data.Char
import Lambda
import Library
}

%name parserlamb
%tokentype { Token }
%error { parseError }

%token
	lambda { TokenLam }
	true   { TokenTrue }
	false  { TokenFalse }
	and    { TokenAnd }
	or     { TokenOr }
	var    { TokenVar $$ }
	'.'    { TokenPoint }
	'('    { TokenOB }
	')'    { TokenCB }

%right '.'
%nonassoc lambda var '(' ')' true false and or
%left APP
%%

Term 
	: lambda var '.' Term   { LamAbs $2 $4}
	| Term Term %prec APP   { LamApp $1 $2 }
	| '(' Term ')'          { $2 }
	| true                  { true }
	| false                 { false }
	| and                   { Library.and }
	| or                    { Library.or }
	| var                   { LamVar $1 }
{

parseError :: [Token] -> a
parseError b = error "Parse Error"

-- data Term
--   = LamVar Char
--   | LamAbs Char Term
--   | LamApp Term Term
--   deriving (Show, Eq)

data Token 
	= TokenVar Char
	| TokenPoint
	| TokenOB
	| TokenCB
	| TokenLam 
	| TokenTrue
	| TokenFalse
	| TokenAnd
	| TokenOr
	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c == '.'  = TokenPoint : lexer cs
    | c == '('  = TokenOB : lexer cs
    | c == ')'  = TokenCB : lexer cs
    | isAlpha c = lexWord (c:cs)

lexWord :: String -> [Token]
lexWord (c:cs)
	| a == "lambda" = TokenLam : lexer rest
	| a == "true"   = TokenTrue : lexer rest
	| a == "false"  = TokenFalse : lexer rest
	| a == "and"    = TokenAnd : lexer rest
	| a == "or"     = TokenOr : lexer rest
	| otherwise     = TokenVar c : lexer rest
	where (a, rest) = span isAlpha (c:cs)
-- lexer :: String -> [Token]
-- lexer [] = []
-- lexer (c:cs)
--     | isSpace c = lexer cs
--     | c == '.'  = TokenPoint : lexer cs
--     | c == '('  = TokenOB : lexer cs
--     | c == ')'  = TokenCB : lexer cs
--     | isAlpha c = 
-- 		if (a == "lambda") then TokenLam : lexer rest else (TokenVar c) : lexer rest
-- 		where (a, rest) = span isAlpha (c:cs)
}