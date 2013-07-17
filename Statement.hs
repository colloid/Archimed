module Statement (
    Statement(..),
    evaluate
) where

import qualified Expression as Expr
import qualified Configuration as Conf


type Expression = Expr.Expression

{-- type definition --}
data Statement =
      Skip
    | Assign        String Expression
    | Read          String
    | Write         String
    | IfThen        Expression [Statement] [Statement]
    | WhileDo       Expression [Statement]
    | Block         [Statement]      
    deriving (Eq, Read)

{-- evaluate --}
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

{-- instance Show --}
instance Show Statement where 
    show x = show' 0 x

indentations = cycle ["    "]
gosub indent stmts = concat $ map (show' indent) stmts

show' indent stmt = let prefix = concat $ take indent indentations
                    in  case stmt of
    Skip            -> prefix ++ "skip\n"
    Assign id expr  -> prefix ++ id ++ " := " ++ show expr ++ "\n"
    Read id         -> prefix ++ "read " ++ id ++ "\n"
    Write id        -> prefix ++ "write " ++ id ++ "\n"
    IfThen e st sf  -> prefix ++ "if " ++ show e ++ " then\n" ++
                            gosub (indent + 1) st ++
                            prefix ++ "else\n" ++
                            gosub (indent + 1) sf ++
                            prefix ++ "end\n"
    WhileDo e stmt  -> prefix ++ "while " ++ show e ++ " do\n" ++
                            gosub (indent + 1) stmt ++
                            prefix ++ "end\n"
    Block stmt      -> prefix ++ "begin\n" ++
                            gosub (indent + 1) stmt ++
                            prefix ++ "end\n"
