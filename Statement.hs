module Statement (
    Statement(..),
    evaluate,
    mix
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
    Assign id expr  -> Conf.assignVariable id (Conf.Def $ Expr.evaluate expr conf) conf
    Read id         -> Conf.readVariable id conf
    Write id        -> Conf.writeVariable id conf
    IfThen e st sf  -> if Expr.evaluate e conf == 0 then foldl evaluate' conf sf
                                                    else foldl evaluate' conf st
    WhileDo e s     -> if Expr.evaluate e conf == 0 then conf
                                                    else evaluate stmt (foldl evaluate' conf s)
    Block b         -> foldl evaluate' conf b

{-- mix --}
mix_list stmts conf = 
    foldl fun ([], conf) stmts
    where fun (list, cfg) stmt = let tmp = mix stmt cfg
                                     sss = fst tmp
                                     ccc = snd tmp
                                 in (list ++ [sss], ccc)

mix stmt@(Assign id expr) conf = 
    let val = Expr.mix expr conf
    in case val of
        (Expr.Literal l) -> (Skip, Conf.assignVariable id (Conf.Def l) conf)
        _                -> (Assign id val, Conf.assignVariable id Conf.Undef conf)

mix stmt@(Read id) conf = let tmp = Conf.readValue conf
                              val = fst tmp
                              ccc = snd tmp
                              cnf = Conf.assignVariable id val ccc
                          in if Conf.Undef == val then (stmt, cnf) 
                                                  else (Skip, cnf)

mix stmt@(Write id) conf = 
    let val = Conf.evaluateVariable id conf
    in case val of
        (Conf.Def i) -> let ass = Assign id (Expr.Literal i)
                            block = Block [ass, stmt]
                        in (block, conf)
        _            -> (stmt, conf)

mix stmt@(IfThen expr st sf) conf =
    let val = Expr.mix expr conf
    in case val of 
        (Expr.Literal l) -> if l == 0 then mix (Block sf) conf
                                      else mix (Block st) conf
        _                -> let st_pair = mix_list st conf
                                sf_pair = mix_list sf conf
                                st_cfg = snd st_pair
                                sf_cfg = snd sf_pair
                                st' = fst st_pair
                                sf' = fst sf_pair
                            in (IfThen val st' sf', Conf.merge st_cfg sf_cfg)

mix stmt@(WhileDo expr stmts) conf =
    let val = Expr.mix expr conf
    in case val of
        (Expr.Literal l) -> if l == 0 then (Skip, conf)
                                      else let block = Block $ stmts ++ [stmt]
                                           in mix block conf
        _                -> let tmp  = mix_list stmts conf
                            in (WhileDo val $ fst tmp, snd tmp)
                            {-- TODO если в цикле k := k + 1, а перед циклом k := 1, то
                             -  в цикле получаем k := 1 + 1, что неверно --}

mix stmt@(Block stmts) conf =
    let tmp = mix_list stmts conf
        sss = fst tmp
        ccc = snd tmp
    in (Block sss, ccc)

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
