module Expression (
    Expression(..),
    evaluate
) where

import qualified Configuration as Conf

{-- type definition --}
data Expression =
      Literal  Int
    | Variable String

    | Plus              Expression Expression
    | Minus             Expression Expression
    | Times             Expression Expression
    | Divide            Expression Expression
    | Modulo            Expression Expression

{--
    | Greater           Expression Expression
    | GreaterEq         Expression Expression
    | LessEq            Expression Expression
    | Equal             Expression Expression
    | NotEqual          Expression Expression
--}
    | Less              Expression Expression
    | Negate            Expression
    deriving (Eq, Read)
    
{-- evaluate --}
fromBool True  = 1
fromBool False = 0

evaluate (Literal  l) conf = l
evaluate (Variable v) conf = Conf.state conf v

evaluate (Plus           el er) conf    = (evaluate el conf) + (evaluate er conf)
evaluate (Minus          el er) conf    = (evaluate el conf) - (evaluate er conf)
evaluate (Times          el er) conf    = (evaluate el conf) * (evaluate er conf)
evaluate (Divide         el er) conf    = (evaluate el conf) `div` (evaluate er conf)
evaluate (Modulo         el er) conf    = (evaluate el conf) `mod` (evaluate er conf)

{--
evaluate (Greater        el er) conf    = fromBool $ (evaluate el conf) >  (evaluate er conf)
evaluate (GreaterEq      el er) conf    = fromBool $ (evaluate el conf) >= (evaluate er conf)
evaluate (LessEq         el er) conf    = fromBool $ (evaluate el conf) <= (evaluate er conf)
evaluate (Equal          el er) conf    = fromBool $ (evaluate el conf) == (evaluate er conf)
evaluate (NotEqual       el er) conf    = fromBool $ (evaluate el conf) /= (evaluate er conf)
--}
evaluate (Less           el er) conf    = fromBool $ (evaluate el conf) <  (evaluate er conf)
evaluate (Negate         expr)  conf    = fromBool $ (evaluate expr conf) == 0

{-- instance Show --}
data Rank = RnkLess 
          | RnkMinus
          | RnkPlus 
          | RnkDiv
          | RnkTimes 
          | RnkMod 
          | RnkNeg 
          | RnkVar 
          deriving (Eq, Ord, Bounded)

rank expr = case expr of
    Literal  _      -> RnkVar
    Variable _      -> RnkVar
    Plus     _ _    -> RnkPlus
    Minus    _ _    -> RnkMinus
    Times    _ _    -> RnkTimes
    Divide   _ _    -> RnkDiv
    Modulo   _ _    -> RnkMod
    Less     _ _    -> RnkLess
    Negate   _      -> RnkNeg

instance Show Expression where
    show expr = show' minBound expr

show' r0 expr = let r1 = rank expr
                    ex = show'' r1 expr
                in if parenNeeded r0 r1 then "(" ++ ex ++ ")"
                                        else ex
                where parenNeeded RnkMinus r = RnkMinus >= r
                      parenNeeded RnkDiv   r = RnkDiv   >= r
                      parenNeeded      rl rb = rl > rb

show'' rank expr = let abbr delim lhs rhs = show' rank lhs ++ delim ++ show' rank rhs
                   in case expr of
    Literal  a          -> show a
    Variable v          -> v
    Plus     l r        -> abbr " + " l r
    Minus    l r        -> abbr " - " l r
    Times    l r        -> abbr " * " l r
    Divide   l r        -> abbr " / " l r
    Modulo   l r        -> abbr " % " l r
    Less     l r        -> abbr " < " l r
    Negate   e          -> "!" ++ show' rank e
