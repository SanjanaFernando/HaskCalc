-- | Demo and test driver for the Expr interpreter.
-- Run with:  runghc Main.hs      (or: ghc -Wall Main.hs && ./Main)
module Main (main) where

import Expr

env0 :: Env
env0 = [("x", 5), ("y", 2)]

-- | (label, environment, expression, expected result)
type Case = (String, Expr, Either String Double)

evalCases :: [Case]
evalCases =
  [ ("1  arithmetic",        Add (Lit 2) (Mul (Var "x") (Lit 3)),                    Right 17)
  , ("2  division",          Div (Var "x") (Var "y"),                                Right 2.5)
  , ("3  division by zero",  Div (Lit 1) (Sub (Var "x") (Lit 5)),                    Left "division by zero")
  , ("4  undefined variable",Add (Var "x") (Var "z"),                                Left "undefined variable: z")
  , ("5  let binding",       Let "a" (Add (Var "x") (Lit 1)) (Mul (Var "a") (Var "a")), Right 36)
  , ("6  let shadowing",     Let "x" (Lit 10) (Sub (Var "x") (Var "y")),             Right 8)
  , ("7  if (true branch)",  If (Less (Var "y") (Var "x")) (Lit 100) (Lit 200),      Right 100)
  , ("8  if (lazy branch)",  If (BLit True) (Lit 1) (Div (Lit 1) (Lit 0)),           Right 1)
  , ("9  error in Let rhs",  Let "a" (Div (Lit 1) (Lit 0)) (Lit 7),                  Left "division by zero")
  , ("10 nested bool",       If (And (Not (Equal (Var "x") (Lit 0))) (Less (Lit 1) (Var "y")))
                                (Lit 1) (Lit 0),                                     Right 1)
  ]

simplifyCases :: [(String, Expr, Expr)]
simplifyCases =
  [ ("x + 0",            Add (Var "x") (Lit 0),                   Var "x")
  , ("0 + x",            Add (Lit 0) (Var "x"),                   Var "x")
  , ("x * 1",            Mul (Var "x") (Lit 1),                   Var "x")
  , ("x * 0",            Mul (Var "x") (Lit 0),                   Lit 0)
  , ("x - 0",            Sub (Var "x") (Lit 0),                   Var "x")
  , ("x / 1",            Div (Var "x") (Lit 1),                   Var "x")
  , ("nested",           Add (Mul (Var "x") (Lit 1)) (Mul (Lit 0) (Var "y")), Var "x")
  , ("constant folding", Add (Lit 2) (Mul (Lit 3) (Lit 4)),       Lit 14)
  , ("keeps 1/0",        Div (Lit 1) (Lit 0),                     Div (Lit 1) (Lit 0))
  , ("static if",        If (Less (Lit 1) (Lit 2)) (Var "x") (Var "y"), Var "x")
  ]

check :: (Eq a, Show a) => String -> a -> a -> IO Bool
check label expected actual = do
  let ok = expected == actual
  putStrLn $ (if ok then "  [PASS] " else "  [FAIL] ") ++ label
  if ok then return () else putStrLn ("         expected " ++ show expected ++ "\n         actual   " ++ show actual)
  return ok

main :: IO ()
main = do
  putStrLn ("Environment: " ++ show env0 ++ "\n")

  putStrLn "== eval =="
  r1 <- mapM (\(l, e, want) -> do
                putStrLn ("  " ++ pretty e)
                putStrLn ("    expected: " ++ show want ++ "   actual: " ++ show (eval env0 e))
                check l want (eval env0 e)) evalCases

  putStrLn "\n== simplify =="
  r2 <- mapM (\(l, e, want) -> do
                putStrLn ("  " ++ pretty e ++ "   ==>   " ++ pretty (simplify e))
                check l want (simplify e)) simplifyCases

  putStrLn "\n== simplify preserves value (where eval succeeds) =="
  let sameValue e = eval env0 e == eval env0 (simplify e)
  r3 <- mapM (\(l, e, _) -> check l True (sameValue e))
             [c | c@(_, e, _) <- evalCases, either (const False) (const True) (eval env0 e)]

  putStrLn "\n== batch (currying / map / filter / foldr) =="
  let batch = [e | (_, e, _) <- evalCases]
  putStrLn ("  evalBatch  (successful values): " ++ show (evalBatch env0 batch))
  putStrLn ("  batchReport (ok, failed)      : " ++ show (batchReport env0 batch))
  r4 <- check "batchReport" (7, 3) (batchReport env0 batch)
  r5 <- check "evalBatch"   [17,2.5,36,8,100,1,1] (evalBatch env0 batch)

  let results = concat [r1, r2, r3, [r4, r5]]
  putStrLn ("\n" ++ show (length (filter id results)) ++ "/" ++ show (length results) ++ " checks passed")
