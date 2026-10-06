-- |
-- Module      : Expr
-- Course      : EC 8206 - Functional Programming (University of Ruhuna)
-- Description : A small, type-safe arithmetic expression interpreter.
--
-- Part A: algebraic data types  (Expr, BExpr, Env)
-- Part B: eval                  (recursion + pattern matching, Either errors)
-- Part C: simplify, evalBatch, batchReport, evalAll (higher-order functions
--         and currying)
module Expr
  ( Expr(..)
  , BExpr(..)
  , Env
  , eval
  , evalB
  , simplify
  , simplifyB
  , evalBatch
  , evalAll
  , batchReport
  , pretty
  ) where

import Data.Either (rights)

--------------------------------------------------------------------------
-- Part A - Language design
--------------------------------------------------------------------------

-- | Arithmetic expressions.  The first six constructors are the required
-- ones; 'Let' and 'If' are the extension constructors that make 'Expr' a
-- genuine sum type with a second "shape" (binding and branching).
data Expr
  = Lit Double              -- ^ numeric literal, e.g. @Lit 3.5@
  | Var String              -- ^ variable reference, e.g. @Var "x"@
  | Add Expr Expr           -- ^ e1 + e2
  | Sub Expr Expr           -- ^ e1 - e2
  | Mul Expr Expr           -- ^ e1 * e2
  | Div Expr Expr           -- ^ e1 / e2
  | Let String Expr Expr    -- ^ @Let x e1 e2@  =  let x = e1 in e2
  | If BExpr Expr Expr      -- ^ @If c t e@     =  if c then t else e
  deriving (Show, Eq)

-- | A small boolean sub-language used as the condition of 'If'.
-- Because conditions are a *different type* from numeric expressions,
-- something like @If (Lit 1) a b@ is rejected by the compiler.
data BExpr
  = BLit Bool               -- ^ True / False
  | Less Expr Expr          -- ^ e1 < e2
  | Equal Expr Expr         -- ^ e1 == e2
  | Not BExpr               -- ^ not b
  | And BExpr BExpr         -- ^ b1 && b2
  deriving (Show, Eq)

-- | Variable bindings: an association list from names to values.
type Env = [(String, Double)]

--------------------------------------------------------------------------
-- Part B - Evaluation
--------------------------------------------------------------------------

-- | Evaluate an expression in an environment.
-- Failures are values (@Left msg@), never runtime exceptions:
--
--   * reference to an undefined variable
--   * division by zero
--
-- The @do@ blocks use the 'Either' monad, so the first error in a
-- sub-expression short-circuits the rest of the computation.
eval :: Env -> Expr -> Either String Double
eval _   (Lit n)       = Right n
eval env (Var x)       = case lookup x env of
                           Just v  -> Right v
                           Nothing -> Left ("undefined variable: " ++ x)
eval env (Add a b)     = do { x <- eval env a; y <- eval env b; Right (x + y) }
eval env (Sub a b)     = do { x <- eval env a; y <- eval env b; Right (x - y) }
eval env (Mul a b)     = do { x <- eval env a; y <- eval env b; Right (x * y) }
eval env (Div a b)     = do
  x <- eval env a
  y <- eval env b
  if y == 0 then Left "division by zero" else Right (x / y)
eval env (Let x e1 e2) = do
  v <- eval env e1
  eval ((x, v) : env) e2      -- new binding shadows any older one; env itself is untouched
eval env (If c t e)    = do
  b <- evalB env c
  if b then eval env t else eval env e   -- only the chosen branch is evaluated

-- | Evaluate a boolean expression (helper for 'If').
evalB :: Env -> BExpr -> Either String Bool
evalB _   (BLit b)    = Right b
evalB env (Less a b)  = do { x <- eval env a; y <- eval env b; Right (x < y) }
evalB env (Equal a b) = do { x <- eval env a; y <- eval env b; Right (x == y) }
evalB env (Not b)     = not <$> evalB env b
evalB env (And a b)   = do
  p <- evalB env a
  if p then evalB env b else Right False   -- short-circuit

--------------------------------------------------------------------------
-- Part C - Simplification and higher-order functions
--------------------------------------------------------------------------

-- | Bottom-up algebraic simplification.  Children are simplified first,
-- then the rewrite rules for the current node are tried.  Rules:
--
-- >  x + 0 -> x      0 + x -> x
-- >  x - 0 -> x
-- >  x * 1 -> x      1 * x -> x
-- >  x * 0 -> 0      0 * x -> 0
-- >  x / 1 -> x
-- >  literal (op) literal -> folded literal   (never folds division by 0)
-- >  if True/False then a else b -> a / b
--
-- Note: @x * 0 -> 0@ can turn an expression that would fail (e.g. @x@
-- undefined) into a successful 0; for every expression whose original
-- evaluation succeeds, the simplified one gives the same result.
simplify :: Expr -> Expr
simplify (Add a b) = addRule (simplify a) (simplify b)
simplify (Sub a b) = subRule (simplify a) (simplify b)
simplify (Mul a b) = mulRule (simplify a) (simplify b)
simplify (Div a b) = divRule (simplify a) (simplify b)
simplify (Let x e1 e2) = Let x (simplify e1) (simplify e2)
simplify (If c t e) =
  case simplifyB c of
    BLit True  -> simplify t
    BLit False -> simplify e
    c'         -> If c' (simplify t) (simplify e)
simplify e = e                       -- Lit and Var are already simple

-- | Simplify the expressions inside a boolean expression, folding
-- literal comparisons and double negation.
simplifyB :: BExpr -> BExpr
simplifyB (Less a b)  = case (simplify a, simplify b) of
                          (Lit x, Lit y) -> BLit (x < y)
                          (a', b')       -> Less a' b'
simplifyB (Equal a b) = case (simplify a, simplify b) of
                          (Lit x, Lit y) -> BLit (x == y)
                          (a', b')       -> Equal a' b'
simplifyB (Not b)     = case simplifyB b of
                          BLit v -> BLit (not v)
                          Not b' -> b'
                          b'     -> Not b'
simplifyB (And a b)   = case (simplifyB a, simplifyB b) of
                          (BLit False, _)     -> BLit False
                          (BLit True, b')     -> b'
                          (a', BLit True)     -> a'
                          (a', b')            -> And a' b'
simplifyB b           = b

-- One rule function per operator; each receives already-simplified children.
addRule, subRule, mulRule, divRule :: Expr -> Expr -> Expr
addRule (Lit x) (Lit y) = Lit (x + y)
addRule (Lit 0) e       = e
addRule e       (Lit 0) = e
addRule a       b       = Add a b

subRule (Lit x) (Lit y) = Lit (x - y)
subRule e       (Lit 0) = e
subRule a       b       = Sub a b

mulRule (Lit x) (Lit y) = Lit (x * y)
mulRule (Lit 0) _       = Lit 0
mulRule _       (Lit 0) = Lit 0
mulRule (Lit 1) e       = e
mulRule e       (Lit 1) = e
mulRule a       b       = Mul a b

divRule (Lit x) (Lit y) | y /= 0 = Lit (x / y)
divRule e       (Lit 1)          = e
divRule a       b                = Div a b

-- | Evaluate a batch of expressions and keep only the successful results.
-- Uses currying: @eval env@ is a partially applied function that is
-- mapped over the list.  ('rights' discards every 'Left'.)
evalBatch :: Env -> [Expr] -> [Double]
evalBatch env = rights . map (eval env)

-- | Evaluate every expression, keeping both successes and failures.
-- Partial application of 'eval' to a shared environment.
evalAll :: Env -> [Expr] -> [Either String Double]
evalAll env = map (eval env)

-- | Report @(succeeded, failed)@ for a batch, using 'foldr'.
batchReport :: Env -> [Expr] -> (Int, Int)
batchReport env = foldr tally (0, 0) . evalAll env
  where
    tally (Right _) (ok, bad) = (ok + 1, bad)
    tally (Left  _) (ok, bad) = (ok, bad + 1)

--------------------------------------------------------------------------
-- Pretty printing (used by the demo program)
--------------------------------------------------------------------------

-- | Render an expression in conventional, fully parenthesised infix syntax.
pretty :: Expr -> String
pretty (Lit n)       = showNum n
pretty (Var x)       = x
pretty (Add a b)     = bin "+" a b
pretty (Sub a b)     = bin "-" a b
pretty (Mul a b)     = bin "*" a b
pretty (Div a b)     = bin "/" a b
pretty (Let x a b)   = "let " ++ x ++ " = " ++ pretty a ++ " in " ++ pretty b
pretty (If c t e)    = "if " ++ prettyB c ++ " then " ++ pretty t ++ " else " ++ pretty e

prettyB :: BExpr -> String
prettyB (BLit b)    = show b
prettyB (Less a b)  = pretty a ++ " < " ++ pretty b
prettyB (Equal a b) = pretty a ++ " == " ++ pretty b
prettyB (Not b)     = "not (" ++ prettyB b ++ ")"
prettyB (And a b)   = "(" ++ prettyB a ++ " && " ++ prettyB b ++ ")"

bin :: String -> Expr -> Expr -> String
bin op a b = "(" ++ pretty a ++ " " ++ op ++ " " ++ pretty b ++ ")"

-- Show whole numbers without a trailing ".0".
showNum :: Double -> String
showNum n | n == fromIntegral r = show r
          | otherwise           = show n
  where r = round n :: Integer
