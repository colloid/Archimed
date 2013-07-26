module Ast (normalize) where

import Statement
import Expression

transformations = 
    [ linearize
    , skipSkip
    ]

normalize ast = foldl (\x f -> f x) ast transformations


{-- linearize --}
linearize (Block list) = Block $ linearize_l list
    where linearize_l = concat . map linearize'
          linearize' stmt = case stmt of
            (IfThen  e l r) -> [ IfThen e (linearize_l l) (linearize_l r) ]
            (WhileDo e s  ) -> [ WhileDo e (linearize_l s) ]
            (Block   list ) -> linearize_l list
            x               -> [ x ]

linearize _ = error "Bad AST"


{-- skipSkip --}
skipSkip stmt =
    case stmt of
        (Block list)    -> fillIfEmpty $ Block $ transmutate list
        (IfThen e t f)  -> fillIfEmpty $ IfThen e (transmutate t) (transmutate f)
        (WhileDo e s)   -> fillIfEmpty $ WhileDo e $ transmutate s
        x               -> x
    where
        nonSkip = filter (\x -> x /= Skip)
        transmutate = (map skipSkip) . nonSkip
        fillIfEmpty (Block []) = Block [Skip]
        fillIfEmpty (IfThen e [] f) = fillIfEmpty $ IfThen e [Skip] f
        fillIfEmpty (IfThen e t []) = fillIfEmpty $ IfThen e t [Skip]
        fillIfEmpty (WhileDo e []) = WhileDo e [Skip]
        fillIfEmpty x = x


