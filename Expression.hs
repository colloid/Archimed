module Expression (
    Expression(..),
    evaluate,
    mix
) where

import qualified Configuration as Conf
type Input = Conf.Input

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

evaluate (Literal  i) conf = i
evaluate (Variable v) conf = let value = Conf.evaluateVariable v conf
                             in case value of
                                (Conf.Def i) -> i

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


{-- mix --}
mix (Literal        l) conf = Literal l
mix (Variable       v) conf = let value = Conf.evaluateVariable v conf
                              in case value of
                                  (Conf.Def i) -> Literal  i
                                  (Conf.Undef) -> Variable v

mix (Plus       el er) conf = mix' $ Plus   (mix el conf) (mix er conf)
mix (Minus      el er) conf = mix' $ Minus  (mix el conf) (mix er conf)
mix (Times      el er) conf = mix' $ Times  (mix el conf) (mix er conf)
mix (Divide     el er) conf = mix' $ Divide (mix el conf) (mix er conf)
mix (Modulo     el er) conf = mix' $ Modulo (mix el conf) (mix er conf)

mix (Less       el er) conf = mix' $ Less   (mix el conf) (mix er conf)
mix (Negate      expr) conf = mix' $ Negate $ mix expr conf


mix' (Plus      (Literal ll) (Literal lr)) = Literal (ll + lr)
mix' (Minus     (Literal ll) (Literal lr)) = Literal (ll - lr)
mix' (Times     (Literal ll) (Literal lr)) = Literal (ll * lr)
mix' (Divide    (Literal ll) (Literal lr)) = Literal (ll `div` lr)
mix' (Modulo    (Literal ll) (Literal lr)) = Literal (ll `mod` lr)

mix' (Less      (Literal ll) (Literal lr)) = Literal $ fromBool $ ll < lr
mix' (Negate    (Literal ll))              = Literal $ fromBool $ ll == 0

mix' x = x


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
