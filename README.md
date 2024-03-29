# Cm
## Intro
A compiler for a syntactically C-like imperative programming language, supporting (currently) array and struct. The front end is not very well-designed, while the mid end focuses on data flow analysis (with generic interface). The back end is trash (compiling to lua vm).

## Programming Paradigm
* Generic. This compiler has a generic interface to data flow problem using OCaml module system. The interface is designed to be extensible. All of my implementations of data flow analysis make use of this generic interface.
* Visitor Pattern. In the intermediate representation and control flow graph, this compiler uses visitor pattern to traverse abstract syntax tree, graph, etc. This part makes use of OCaml object system. Using object to represent visitor makes it easy to record state while traversing.

## Workflow
* Lexing and Parsing
* Sementic Checking (type checking, early liveness analysis)
* Translating to IR
* Optimizing (partial redundancy elimination, propagation + dead code elimination)
* Translating to lua bytecode
* (Auxilary) Producing `.dot` file for each procedure

## Semantics Checking
The `semant.ml` file checks def-before-use, variable initialization (not include field initialization), type and proper return. For a sequence of statements, when checking proper return, if some statement already returns, the statements behind it can have arbitrary return semantics (since we really don't care about it).

## Mimple
Mimple is an simplified version of Jimple (SOOT Java optimization framework). In this toy language, global vars are treated as static field reference.

## Data Flow Analysis
The core data flow analysis engine is written in the `dataflow.ml` file. It uses a worklist algorithm. Currently supported domains: finite set domain (live variables, available expressions, etc.), finite map domain (constant propagation, copy propagation, etc.).

Partial redundancy elimination is done in a separate file `pre.ml`. It is the most challenging task in this project. This part is done according to the book "Compilers: Principles, Techniques, and Tools".

## Requirement
This project requires 
* OCaml version >= 4.07.0
* opam version >= 2.0.4
* dune version >= 1.10.0
* menhir

To install on MacOS, type
```bash
brew install ocaml
brew install opam
opam init
eval `opam env`
opam switch create 4.07.0
eval `opam env`
opam install dune
opam install menhir
```

## To Install and Uninstall
```bash
make PLAT=macosx
make clean
```
Only macosx is supported:) 

## To Use
```bash
cd Cm
python run.py --file <path-to-file>
```
For example, to run the test program, type `python run.py --file ./test/pre.cm`, and the program will be executed. The binary file will be recorded into `cm_out_bin.txt`. 

## Possible Improvement
* Make instruction data type more heavy-weight. Currently, the instruction data type contains pure instruction. For transformation purpose, we can augment it with reference to its predecessor and successor. 
