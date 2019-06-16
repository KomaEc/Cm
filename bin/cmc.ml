open Cm_core.Ast.Util
module Parser = Cm_core.Parser 
open Printf 
open Lexing

module Lexer = Cm_core.Lexer
module Mimple = Cm_core.Mimple
module Basic_block = Cm_core.Basic_block
open Cm_core.Semant
open Cm_core.Support.Error
module Dfa = Cm_core.Dfa
module T = Cm_core.Translate
module Proc = Cm_core.Procdesc
module Dataflow = Cm_core.Dataflow
module Pre = Cm_core.Pre
module Binary = Cm_backend.Binary

let print_position outx lexbuf = 
  let pos = lexbuf.lex_curr_p in 
  fprintf outx "%s : lines %d : offset %d\n"
  pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf = 
  try Parser.prog Lexer.read lexbuf with 
  | Lexer.Syntax_Error msg -> 
    fprintf stderr "%a: %s" print_position lexbuf msg;
    exit(0)
  | Parser.Error -> 
    fprintf stderr "%a" print_position lexbuf;
    exit(0)
;;

let check_with_error s = 
  try check s with 
  | Duplicated_Definition i -> 
    fprintf stderr "%a %s" printInfo i "Duplicated definition\n"; exit(0)
  | Lack_Definition i -> 
    fprintf stderr "%a %s" printInfo i "LackDefinition\n"; exit(0)
  | No_Initialization i -> 
    fprintf stderr "%a %s" printInfo i "No Initialization\n"; exit(0)
  | Ill_Typed i -> 
    fprintf stderr "%a %s" printInfo i "Ill Typed\n"; exit(1)
  | Arity_Mismatched i -> 
    fprintf stderr "%a %s" printInfo i "Arity Mismatched\n"; exit(1)
  | Fundec_Mismatched i -> 
    fprintf stderr "%a %s" printInfo i "Function Declaration Mismatched\n"; exit(1)
  | Not_Function i -> 
    fprintf stderr "%a %s" printInfo i "Identifier Not a Function\n"; exit(1)
  | No_Fieldname i -> 
    fprintf stderr "%a %s" printInfo i "No Such Field\n"; exit(1)
  | Not_Struct i -> 
    fprintf stderr "%a %s" printInfo i "Identifier Not a Struct\n"; exit(1)
  | Alloc_Non_Struct i -> 
    fprintf stderr "%a %s" printInfo i "Allocation of Array and Struct Only\n"; exit(1)
  | Type_Var_Misuse i -> 
    fprintf stderr "%a %s" printInfo i "Variable Expected. Not Type\n"; exit(1)
  | Null_Reference i -> 
    fprintf stderr "%a %s" printInfo i "Referencing a Nullptr\n"; exit(1)
let print_helper = "What do you need?\n"

let () = 
  match Array.length Sys.argv with 
  | 1 -> fprintf stderr "%s" print_helper; exit(0)   
  | 2 -> (let fname = Sys.argv.(1) in 
          let inx = open_in fname in 
          let lexbuf = from_channel inx in 
          lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname};
          lexbuf 
          |> parse_with_error
          |> simplify
          |> check_with_error
          |> Binary.compile;
          close_in inx)
  | _ -> fprintf stderr "Too many arguments! Expected 1\n"; exit(0)