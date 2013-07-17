module Configuration
    ( Configuration
    , new
    , readInput
    , write
    , evaluateVariable
    , assignVariable
    , state
    , input
    , output
    ) where

data Configuration = Configuration {
    state   :: String -> Int,
    input   :: [Int],
    output  :: [Int]
}

undef var = error $ "Undefined variable " ++ var

new vector = Configuration { state = undef, input = vector, output = [] }

readInput id conf = Configuration { state = new_state, 
                                    input = tail in_vec,
                                    output = output conf
                                  } where
                in_vec = input conf
                new_state x = if x == id then head in_vec else state conf x

evaluateVariable id conf = state conf id

assignVariable id value conf = Configuration { state = new_state,
                                               input = input conf,
                                               output = output conf
                                             } where
                new_state x = if x == id then value else state conf x

write id conf = Configuration { state = state conf,
                                input = input conf,
                                output = output conf ++ [ state conf id ]
                              }
