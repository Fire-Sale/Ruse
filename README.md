# Ruse Language

This is a toy programming language inplemented in Haskell and is written while taking CS 538 Theory and Design of Programming Languages. Ruse is a simple Lisp-like language that support untyped lambda calculus with useful extensions.



## Functionality

- Type supported: `bool`, `int`, `char`, `string`, `list`, `function`.
- Operations supported: Common boolean/arithmatic operations that support arguments with arbitrary length, branching (`if-then-else`, `condition`), list operations (`init`, `concat`, `head\tail` etc.), function operations (declaration, application). Value binding and recursive functions are also supported. 
- For full specification details, please refer to `ruse-spec.pdf`. It includes syntax and big-step operational semnatics description. 



## Main Components

- Syntax (`src/Ruse/Syntax.hs`): Describe the abstract syntax tree for Ruse. 
- Parser (`src/Ruse/Parser.hs`): Parse the code string literal into AST.
- Big-step evaluator (`src/Ruse/Eval.hs`): Eval the value given the AST.
- REPL (`app/Main.hs`): Entrance. 
- Testing (`app/Tests.hs`)



## Usage

- Run: `cabal v2-run`
- Testing: `cabal v2-repl` -> `:l Ruse.Tests`
- Grammar: Please refer to `ruse-spec.pdf`