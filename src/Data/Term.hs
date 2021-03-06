module Data.Term where

import Data.List (find, (!!))
import Data.Maybe (isJust)
import Data.Either (Either(..))

newtype Info = Info Int

data Binding = NameBind

type Context = [(String, Binding)]

data EvalError = NoRuleApplies

data Term
  = Var Info Int Int
  | Abs Info String Term
  | App Info Term Term

instance Show Term where
  show = printTerm []

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

-- Shift the variables in the given term by `d`
termShift :: Int -> Term -> Term
termShift d = walk 0
  where
    walk :: Int -> Term -> Term
    walk c t = case t of
      Var info x n
        | x < c -> Var info x (n + d)
        | otherwise -> Var info (x + d) (n + d)
      App info t1 t2 ->
        App info (walk c t1) (walk c t2)
      Abs info x t1 ->
        Abs info x $ walk (c + 1) t1

-- Substitute the `j`th variable to term `s` in term `t`
termSubstitute :: Int -> Term -> Term -> Term
termSubstitute j s = walk 0
  where
    walk :: Int -> Term -> Term
    walk c t = case t of
      Var info x n
        | x == j + c -> termShift c s
        | otherwise -> t
      App info t1 t2 ->
        App info (walk c t1) (walk c t2)
      Abs info x t1 ->
        Abs info x $ walk (c + 1) t1

-- Apply term `s` to term `t`
termSubstituteTop :: Term -> Term -> Term
termSubstituteTop s t =
  termShift (-1) $ termSubstitute 0 (termShift 1 s) t

isVal :: Context -> Term -> Bool
isVal _ Abs{} = True
isVal _ _ = False

-- One step evaluation
eval1 :: Context -> Term -> Either EvalError Term
eval1 ctx t = case t of
  App info (Abs _ x t12) v2 | isVal ctx v2 ->
    Right $ termSubstituteTop v2 t12 -- beta reduction
  App info v1 t2 | isVal ctx v1 -> do
    t2' <- eval1 ctx t2
    Right $ App info v1 t2'
  App info t1 t2 -> do
    t1' <- eval1 ctx t1
    Right $ App info t1' t2
  _ ->
    Left NoRuleApplies

-- Multi-step evaluation
eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Right t' -> eval ctx t'
  Left _ -> t
