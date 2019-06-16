
open Cm_core.Ast.Util
module Parser = Cm_core.Parser 
open Printf 
open Lexing

module Lexer = Cm_core.Lexer
module Mimple = Cm_core.Mimple
module Basic_block = Cm_core.Basic_block
open Cm_core.Printer
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
          (*(match parse_with_error lexbuf with 
          | Some s -> print_stmt "" (simplify s) 
          | None -> ());*)
          (*print_stmt "" (Seq(parse_with_error lexbuf, Support.Error.dummyinfo));*)
          let s = simplify (parse_with_error lexbuf) in 
          print_endline "\nOriginal program: ";
          print_stmt "" s;
          print_newline();
          let prog = check_with_error s in
          print_endline "\nTranslating to Mimple...\n";
          Mimple.print_prog prog;
          print_newline ();

          let prog' = T.get_mimple_ori () in
          Mimple.print_prog (prog', Cm_core.Symbol.empty);
          print_newline ();
          print_endline "hello";
          
          let procs = Proc.from_prog prog in
          (*let () = List.iter Proc.insert_goto procs in *)
          print_endline (Proc.string_of_t_list procs);
          (*let () = List.iter (fun proc -> Proc.(test(recover proc))) procs in*)
          (*let () = Proc.test procs in*)
          let prog1 = T.get_mimple_after_pre () in 
          Mimple.print_prog (prog1, Cm_core.Symbol.empty);

          Binary.compile (prog1, Cm_core.Symbol.empty);

(*
          let prog1 = T.get_mimple1() in 
          Mimple.print_prog prog1;
          print_newline();
          print_string "Analysis Result : \n\n";
          Dfa.analysis_prog prog;
          print_string "Optimizting... \n\n";
          let prog4 = T.optimize() in 
          Mimple.print_prog prog4;

          print_newline();
          Dataflow.print_result prog;
          Pre.Test.from_prog prog; *)
          close_in inx
          )
  | _ -> fprintf stderr "Too many arguments! Expected 1\n"; exit(0)


  
