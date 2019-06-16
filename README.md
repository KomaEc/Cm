# Cm-Compiler
## Intro
A compiler for a syntactically C-like imperative programming language (its semantics are closer to Java. For example, there's no pointer. The types are divided into two groups, primitive type and object type. Values of object types are passed by reference, etc.), supporting (currently) one-dimentional array and struct. (Under construction)

## Goals
A compiler for an imperative programming language, with a middle end (Mimple) supporting pointer analysis. The final objective is to compile the intermmediate representation to Lua bytecode.

## Currently
With an input C file, the compiler transforms the source file into an AST, and performs sementic checks. After the semantics checking, the compiler will transform the original program into an intermediate representation Mimple. The result will be printed to the standard output channel.

## Semantics Checking
The `semant.ml` file checks def-before-use, variable initialization (not include field initialization), type and proper return. For a sequence of statements, when checking proper return, if some statement already returns, the statements behind it can have arbitrary return semantics (since we really don't care about it).

## Mimple
Mimple is an simplified version of Jimple (SOOT Java optimization framework). In this toy language, global vars are treated as static field reference.

## Data Flow Analysis
The core data flow analysis engine is written in the `dfa.ml` file. It uses a worklist algorithm. Currently supported domains: finite set domain (live variables, available expressions, etc.), finite map domain (constant propagation, copy propagation, etc.).

## To Use
```bash
dune build ./bin/main.exe
dune exec ./bin/main.exe ./test/basic.cm
```
File "basic.cm" is written in Cm. A translated version will be printed to the stdout channel.
