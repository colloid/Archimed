module Expression (
    Expression(..),
    evaluate
) where

import qualified Configuration as Conf


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
    deriving (Eq, Show, Read)
    
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
