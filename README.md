# Cm-Compiler
## Intro
A compiler for a syntactically C-like imperative programming language, supporting (currently) one-dimentional array and struct. The front end is not very well-designed, while the mid end focuses on data flow analysis (with generic interface). The back end is trash (compiling to lua vm).

## Programming Paradigm
* Generic. This compiler has a generic interface to data flow problem using OCaml module system. It is easy to extend.
* Visitor Pattern. In the intermediate representation and control flow graph, this compiler uses visitor pattern to traverse abstract syntax tree, graph, etc. This part makes use of OCaml object system. Using object to represent visitor makes it easy to record state while traversing.

## Workflow
* Lexing and Parsing
* Sementic Checking (type checking, early live analysis)
* Translating to IR
* Optimizing (partial redundancy elimination, propagation + dead code elimination)
* Translating to lua bytecode

## Semantics Checking
The `semant.ml` file checks def-before-use, variable initialization (not include field initialization), type and proper return. For a sequence of statements, when checking proper return, if some statement already returns, the statements behind it can have arbitrary return semantics (since we really don't care about it).

## Mimple
Mimple is an simplified version of Jimple (SOOT Java optimization framework). In this toy language, global vars are treated as static field reference.

## Data Flow Analysis
The core data flow analysis engine is written in the `dataflow.ml` file. It uses a worklist algorithm. Currently supported domains: finite set domain (live variables, available expressions, etc.), finite map domain (constant propagation, copy propagation, etc.).
Partial redundancy elimination is done in a separate file `pre.ml`. It is the most challenging task in this project. This part is done according to the book "Compilers: Principles, Techniques, and Tools".

## To Use
```bash
dune build ./bin/main.exe
dune exec ./bin/main.exe ./test/basic.cm
```
File "basic.cm" is written in Cm. A translated version will be printed to the stdout channel.
