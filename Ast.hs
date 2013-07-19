module Ast (normalize) where

import Statement
import Expression

transformations = 
    [ linearize
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

