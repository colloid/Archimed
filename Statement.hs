module Statement (
    Statement(..),
    evaluate
) where

import qualified Expression as Expr
import qualified Configuration as Conf


type Expression = Expr.Expression

data Statement =
      Skip
    | Assign        String Expression
    | Read          String
    | Write         String
    | IfThen        Expression [Statement] [Statement]
    | WhileDo       Expression [Statement]
    | Block         [Statement]      
    deriving (Eq, Show, Read)

evaluate stmt conf = let evaluate' = flip evaluate 
                     in  case stmt of
    Skip            -> conf
    Assign id expr  -> Conf.assignVariable id (Expr.evaluate expr conf) conf
    Read id         -> Conf.readInput id conf
    Write id        -> Conf.write id conf
    IfThen e st sf  -> if Expr.evaluate e conf == 0 then foldl evaluate' conf sf
                                                    else foldl evaluate' conf st
    WhileDo e s     -> if Expr.evaluate e conf == 0 then conf
                                                    else evaluate stmt (foldl evaluate' conf s)
    Block b         -> foldl evaluate' conf b
