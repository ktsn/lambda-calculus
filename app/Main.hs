module Main where

import Data.Term

info = Info 10

tree :: Term
tree
  = Abs info "x"
    $ App info
      ( Abs info "x"
        $ Var info 0 2 )
      ( Var info 0 1 )

main :: IO ()
main = print tree
