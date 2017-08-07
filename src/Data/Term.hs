module Data.Term where

import Data.List (find, (!!))
import Data.Maybe (isJust)

newtype Info = Info Int

data Term
  = Var Info Int Int
  | Abs Info String Term
  | App Info Term Term

data Binding = NameBind

type Context = [(String, Binding)]

printTerm :: Context -> Term -> String
printTerm ctx t = case t of
  Abs info x t1 ->
    let (ctx', x') = pickFreshName ctx x in
    "(λ" ++ x' ++ ". " ++ printTerm ctx' t1 ++ ")"
  App info t1 t2 ->
    "(" ++ printTerm ctx t1 ++ " " ++ printTerm ctx t2 ++ ")"
  Var info x n ->
    if length ctx == n then
      indexToName info ctx x
    else
      "[bad index]"

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x
  | hasName ctx x =
    pickFreshName ctx $ x ++ "'"
  | otherwise =
    let ctx' = (x, NameBind) : ctx in
    (ctx', x)

hasName :: Context -> String -> Bool
hasName ctx x = isJust $ find (\c -> fst c == x) ctx

indexToName :: Info -> Context -> Int -> String
indexToName info ctx x = fst $ ctx !! x
