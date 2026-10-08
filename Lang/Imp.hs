{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Lang.Imp where

import qualified Map.TreeMap as M

todo :: a
todo = error "TODO"

-- Type alias for variable names
type Name = String

-- Type alias for mappings from variable names to their values
-- E.g. fromList [("x", 2), ("y", 6)]
type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty

data Expr a where
    -- Boolean expressions
    BNot :: Expr Bool -> Expr Bool -- Boolean inverse (not)
    BAnd :: Expr Bool -> Expr Bool -> Expr Bool -- Boolean And (&&)
    BOr  :: Expr Bool -> Expr Bool -> Expr Bool -- Boolean Or (||)

    -- Relational expressions
    REq :: Expr Integer -> Expr Integer -> Expr Bool -- Equality (==)
    RLt :: Expr Integer -> Expr Integer -> Expr Bool -- Less than (<)

    -- Arithmetic expressions
    AConst :: Integer -> Expr Integer -- Constant, e.g. `AConst 42`
    AVar   :: Name -> Expr Integer -- Variable, e.g. `AVar "x"`
    APlus  :: Expr Integer -> Expr Integer -> Expr Integer -- Integer addition (+)
    AMinus :: Expr Integer -> Expr Integer -> Expr Integer -- Integer subtraction (-)
    AMul   :: Expr Integer -> Expr Integer -> Expr Integer -- Integer multiplication (*)
    ADiv   :: Expr Integer -> Expr Integer -> Expr Integer -- Integer division `div`
    AMod   :: Expr Integer -> Expr Integer -> Expr Integer -- Integer modulus `mod`

deriving instance Eq (Expr a) 
deriving instance Show (Expr a)

data Cmd = 
    CSkip -- No operation, has no effect
  | CAssign Name (Expr Integer) -- Assigns the result of evaluating (`Expr Integer`) to the variable `Name`
  | CIfThEl (Expr Bool) Cmd Cmd -- If then else: evaluates the condition and, depending on the result, executes the first Cmd (then branch) or the second (else branch)
  | CWhile (Expr Bool) Cmd -- While loop
  | CSeq [Cmd] -- Sequence of commands, i.e. the program code

-- Computes the result from an expression and state
eval :: Expr a -> State -> a
eval (BNot b) st      = not (eval b st)
eval (BAnd l r) st    = (eval l st) && (eval r st)
eval (BOr l r) st     = (eval l st) || (eval r st)
eval (REq l r) st     = (eval l st) == (eval r st)
eval (RLt l r) st     = (eval l st) < (eval r st)
eval (AConst x) _     = x
eval (AVar name) st   = maybe (error "Variable not found") id (M.lookup name st)
eval (APlus l r) st   = (eval l st) + (eval r st)
eval (AMinus l r) st  = (eval l st) - (eval r st)
eval (AMul l r) st    = (eval l st) * (eval r st)
eval (ADiv l r) st    = (eval l st) `div` (eval r st)
eval (AMod l r) st    = (eval l st) `mod` (eval r st)

exec :: Cmd -> State -> State
exec CSkip st                 = st
exec (CAssign name expr) st   = M.insert name (eval expr st) st 
exec (CIfThEl expr th el) st  = if (eval expr st) then (exec th st) else (exec el st)
exec (CWhile cond cmd) st     = if (eval cond st) then exec (CWhile cond cmd) (exec cmd st) else st
-- Execute all cmd's from the left sequentially. Updated state is stored in st', currend command in cmd
exec (CSeq xs) st             = foldl (\st' cmd -> exec cmd st') st xs

