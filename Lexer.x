{
module Lexer (scan, Token(..)) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$space = [\ \t\n]

tokens :-

    $space          ;
    $digit+         { \s -> TkLiteral (read s) } 
    "+"             { \s -> TkPlus }
    "-"             { \s -> TkMinus }
    "*"             { \s -> TkTimes }
    "/"             { \s -> TkDiv }
    "%"             { \s -> TkMod }
    "("             { \s -> TkOP }
    ")"             { \s -> TkCP }
    "<"             { \s -> TkLess }
    ":="            { \s -> TkAssign }
    "!"             { \s -> TkNeg }

    "if"            { \s -> TkIf }
    "then"          { \s -> TkThen }
    "else"          { \s -> TkElse }
    "while"         { \s -> TkWhile }
    "do"            { \s -> TkDo }
    "begin"         { \s -> TkBegin }
    "end"           { \s -> TkEnd }

    "write"         { \s -> TkWrite }
    "read"          { \s -> TkRead }
    "skip"          { \s -> TkSkip }

    $alpha [$alpha $digit \_]*  { \s -> TkId s }

{
data Token =
      TkLiteral Int
    | TkPlus
    | TkMinus
    | TkTimes
    | TkDiv
    | TkMod
    | TkOP
    | TkCP
    | TkLess
    | TkAssign
    | TkNeg
    | TkId String
    | TkIf
    | TkThen
    | TkElse
    | TkWhile
    | TkDo
    | TkBegin
    | TkEnd
    | TkWrite
    | TkRead
    | TkSkip
    deriving (Eq, Show)

scan = alexScanTokens
}
