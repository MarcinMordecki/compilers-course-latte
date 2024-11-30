# Project description:
Latte (C/Java-like language) compiler was created using Haskell + BNFC + Alex + Happy.
You can find out more about it's features in https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2023/Latte/ (unfortunately, polish only).
For the most part, it's similar to "Java without classes".
My compiler reads user-input code and translates it to LLVM.
I've decided to implement the following optimizations:
  - usage of registers and Phi - instead of alloc;
  - GCSE (global common subexpression elimination)
  - loop optimization - power reduction
  - constants folding
# Project's structure
The lib folder contains C-implementation of functions like printInt and similar, which weren't the core functionality of the task.
The src folder contains 
- latte.cf - BNFC grammar of the Latte language, provided alongside the task's description
- Main.hs - entrypoint of the app 
- Frontend.hs - compile-time checks (like datatype validation etc.)
- Compiler.hs - responsible for creating LLVM intermediate representation
- IR.hs - definitions of LLVM IR
- CFG.hs - translates AST components to IR and creates control-flow blocks
- GCSE.hs, FoldConstants.hs, IndVar.hs - respective optimizations
- PropagateConstants.hs - replaces knows expressions with constants, whenever possible
- RemoveJumpingBlocks.hs - removes blocks containing solely a jump statement
# Final remarks
The code was written for an assignment under heavy time pressure, so it's not clean (lacking documentation, poor variable names etc).
It was meant to be written once and never read again - if I were to write it now, I'd spend more effort to organize it more carefully, avoid copying, etc.
