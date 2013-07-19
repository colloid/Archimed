{
module Parser where
import Lexer
import Expression
import Statement
}

%name parse
%tokentype { Token }
%error { (\x -> error "Parse error") }

%token
    skip                { TkSkip }
    if                  { TkIf }
    then                { TkThen }
    else                { TkElse }
    begin               { TkBegin }
    end                 { TkEnd }
    while               { TkWhile }
    do                  { TkDo }

    id                  { TkId $$ }
    num                 { TkLiteral $$ }
    
    read                { TkRead }
    write               { TkWrite }

    "("                 { TkOP }
    ")"                 { TkCP }

    ":="                { TkAssign }
    "+"                 { TkPlus }
    "-"                 { TkMinus }
    "*"                 { TkTimes }
    "/"                 { TkDiv }
    "%"                 { TkMod }
    "<"                 { TkLess }
    "!"                 { TkNeg }


%nonassoc "<"
%left "+" "-"
%left "*" "/"
%left "%"
%left "!"

%%

Program
    : Block             { $1 }

StmtList
    : Stmt              { [ $1 ] }
    | Stmt StmtList     { $1 : $2 }

Stmt
    : Skip              { $1 }
    | IfThen            { $1 }
    | WhileDo           { $1 }
    | Block             { $1 }
    | Assign            { $1 }
    | Read              { $1 }
    | Write             { $1 }

Read
    : read id           { Read $2 }

Write
    : write id          { Write $2 }

Block
    : begin StmtList end    { Block $2 }

Skip
    : skip              { Skip }

IfThen
    : if Expr then StmtList else StmtList end
                        { IfThen $2 $4 $6 }

Assign
    : id ":=" Expr      { Assign $1 $3 }

WhileDo
    : while Expr do StmtList end 
                        { WhileDo $2 $4 }

Expr
    : id                { Variable $1 }
    | num               { Literal $1 }
    | "(" Expr ")"      { $2 }
    | Expr "+" Expr     { Plus $1 $3 }
    | Expr "-" Expr     { Minus $1 $3 }
    | Expr "*" Expr     { Times $1 $3 }
    | Expr "/" Expr     { Divide $1 $3 }
    | Expr "%" Expr     { Modulo $1 $3 }
    | Expr "<" Expr     { Less $1 $3 }
    | "!" Expr          { Negate $2 }
 
