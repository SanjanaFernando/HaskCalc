# Expression Interpreter

A small Haskell project for the EC 8206 Functional Programming course at the
University of Ruhuna. It defines a typed expression language, evaluates
expressions safely, simplifies them, and demonstrates higher-order functions.

## Features

- Arithmetic expressions: literals, variables, addition, subtraction,
  multiplication, and division
- `let` bindings with lexical scoping and variable shadowing
- Boolean expressions and conditional expressions
- Safe evaluation using `Either String Double`
- Algebraic simplification and constant folding
- Batch evaluation with `map`, `filter`, currying, and `foldr`
- A demo program with checks for the main behaviors

## Files

- `Expr.hs` - The reusable expression language and interpreter module
- `Main.hs` - Demo program and executable checks

## Requirements

- GHC (the Glasgow Haskell Compiler)

## Run without building files

```text
runghc Main.hs
```

## Compile and run

```text
ghc -Wall Main.hs -o demo
demo.exe
```

The program prints evaluation results, simplification results, and the number
of checks that passed.

## Example

```haskell
eval [("x", 5)] (Add (Var "x") (Lit 3))
-- Right 8.0
```
