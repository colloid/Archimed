module Configuration
    ( Configuration
    , Input(..)
    , new
    , readValue
    , readVariable
    , writeValue
    , writeVariable
    , evaluateVariable
    , assignVariable
    , input
    , output
    , merge
    ) where

import qualified Data.Map as Map

data Input = Undef | Def Int deriving (Eq)

readInput :: String -> [(Input, String)]
readInput "_" = [(Undef, "")]
readInput x = let readInt = readsPrec 5 :: ReadS Int
                  result  = readInt x
                  remainder = snd $ head result
                  value = fst $ head result
              in if remainder == "" then [(Def value, "")]
                                    else error $ "Invalid input '" ++ x ++ "'"

instance Read Input where
    readsPrec _ = readInput

data Configuration = Configuration {
    state   :: Map.Map String Input,
    input   :: [Input],
    output  :: [Int]
}

new vector = Configuration { state = Map.empty, input = vector, output = [] }

readValue conf = let in_vec = input conf
                     conf' = Configuration { state = state conf,
                                             input = tail' in_vec,
                                             output = output conf
                                           } 
                 in (head' in_vec, conf') where
                 head' [] = error "not enough input data"
                 head' (x:xs) = x
                 tail' [] = head' []
                 tail' (x:xs) = xs

readVariable id conf = let tmp = readValue conf
                           val = fst tmp
                           cnf = snd tmp
                       in assignVariable id val cnf

evaluateVariable id conf = eval $ Map.lookup id $ state conf
    where eval (Just x) = x
          eval  Nothing = error $ "Undefined variable: " ++ id

assignVariable id value conf = Configuration { state = new_state,
                                               input = input conf,
                                               output = output conf
                                             } where
                new_state = Map.insert id value $ state conf

writeValue (Def value) conf = Configuration { state = state conf,
                                        input = input conf,
                                        output = output conf ++ [value]
                                      }

writeVariable id conf = let value = evaluateVariable id conf
                        in writeValue value conf

merge lhs rhs = 
    let lstate = state lhs
        rstate = state rhs
        linput = input lhs
        rinput = input rhs
        output = []
        mstate = Map.unionWith fun lstate rstate
        minput = zipWith fun linput rinput
    in Configuration { state = mstate,
                       input = minput,
                       output = output
                     } where
    fun lv rv = if (lv == rv) then lv else Undef
