{
module Parser where 

import Lexer
}

%name parser 
%tokentype { Token }
%error { parseError }

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenAdd }
    "&&"            { TokenAnd }
    if              { TokenIf }
    then            { TokenThen }
    else            { TokenElse }

%nonassoc if then else 
%left '+' 
%left "&&"

%% 

Exp     : num                       { Num $1 }
        | true                      { BTrue }
        | false                     { BFalse }
        | Exp '+' Exp               { Add $1 $3 }
        | Exp "&&" Exp              { And $1 $3 }
        | if Exp then Exp else Exp  { If $2 $4 $6 }

{ 

parseError :: [Token] -> a 
parseError _ = error "Erro sintático!"

}