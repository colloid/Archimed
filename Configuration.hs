module Configuration
    ( Configuration
    , new
    , readInput
    , write
    , evaluateVariable
    , assignVariable
    , input
    , output
    ) where

import qualified Data.Map as Map

data Configuration = Configuration {
    state   :: Map.Map String Int,
    input   :: [Int],
    output  :: [Int]
}

new vector = Configuration { state = Map.empty, input = vector, output = [] }

readInput id conf = Configuration { state = new_state, 
                                    input = tail' in_vec,
                                    output = output conf
                                  } where
                in_vec = input conf
                new_state = Map.insert id (head' in_vec) $ state conf
                head' [] = error "not enough input data"
                head' (x:xs) = x
                tail' [] = head' []
                tail' (x:xs) = xs

evaluateVariable id conf = eval $ Map.lookup id $ state conf
    where eval (Just x) = x
          eval  Nothing = error $ "Undefined variable: " ++ id

assignVariable id value conf = Configuration { state = new_state,
                                               input = input conf,
                                               output = output conf
                                             } where
                new_state = Map.insert id value $ state conf

write id conf = Configuration { state = state conf,
                                input = input conf,
                                output = output conf ++ [ evaluateVariable id conf ]
                              }
