{
module Parser where
import Data.Char
import Lambda
}

%name parserlamb
%tokentype { Token }
%error { parseError }

%token
	lambda { TokenLam }
	var    { TokenVar $$ }
	'.'    { TokenPoint }
	'('    { TokenOB }
	')'    { TokenCB }

%right '.'
%nonassoc lambda var '(' ')'
%left APP
%%

Term 
	: lambda var '.' Term   { Abs $2 $4}
	| Term Term %prec APP   { App $1 $2 }
	| '(' Term ')'          { $2 }
	| var                   { Var $1 }
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