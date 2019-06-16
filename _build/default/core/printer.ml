open Ast 
open Symbol

let rec print_stmt pre = function 
  | Assign(id,e,_) -> 
      print_newline();
      print_string pre;
      print_var id;
      print_string " = "; print_exp e; print_string ";"
  | If(e,s,sop,_) -> 
      print_newline();
      print_string pre;
      print_string "if ("; print_exp e; print_string ") {"; 
      print_stmt (pre^"  ") s;
      print_newline();
      print_string (pre^"}");
      (match sop with
        | Some s ->  print_newline();
                     print_string (pre^"else {"); 
                     print_stmt (pre^"  ") s; 
                     print_newline();
                     print_string (pre^"}")
        | None -> ())
  | While(e,s,_) -> 
      print_newline();
      print_string (pre^"while (");
      print_exp e; print_string ") {";
      print_stmt (pre^"  ") s;
      print_newline();
      print_string (pre^"}")
  | Return(e,_) ->
      print_newline();
      print_string (pre^"return ");
      print_exp e; print_string ";"
  | Nop -> () 
  | Exp(e,_) -> 
      print_newline();
      print_string pre; print_exp e; print_string ";"
  | Seq(sl,_) -> 
      List.iter (fun s -> print_stmt pre s) sl;
  | Vardecl(id,t,s,_) -> 
      print_newline();
      print_string pre;
      print_ty t; print_string (" "^(name id)^";");
      print_stmt pre s ;
  | Fundecl(id,t,s,_) -> 
      print_newline();
      print_string pre;
      print_string ((name id)^" : "); print_ty t;
      print_string ";"; 
      print_stmt pre s
  | Fundefn(id,_,t,s',s,_) -> 
      print_newline();
      print_string pre;
      print_string ((name id)^" : "); print_ty t;
      print_string ": {"; 
      print_stmt (pre^"  ") s';
      print_newline();
      print_string (pre^"}");
      print_stmt pre s
  | Structdecl(id,s,_) -> 
      print_newline();
      print_string (pre^"class "^(name id)^";");
      print_stmt pre s 
  | Structdefn(id,itl,s,_) -> 
      print_newline();
      print_string (pre^"class "^(name id)^" {");
      print_endline (pre^"  ");
      List.iter (fun (id,t) -> 
                 print_string (pre^"  ");
                 print_ty t; print_string (" "^(name id)^";");
                 print_newline()) itl;
      print_string (pre ^ "}");
      print_stmt pre s
and print_exp = function 
  | Intconst(i,_) -> print_int i
  | Var(v) -> print_var v;
  | Un(op, exp,_) ->
      (match op with 
      | Not -> (match exp with 
               | Bin(_) -> print_string "!("; print_exp exp; print_string")"
               | _ -> print_string "!"; print_exp exp;))
  | Bin(e1,bop,e2,_) ->
      print_exp_paren e1;
      print_string 
      (match bop with 
      | Plus -> " + "
      | Minus -> " - "
      | Times -> " * "
      | Div -> " / "
      | And -> " && "
      | Or -> " || "
      | Lt -> " < "
      | Gt -> " > "
      | Eq -> " == ");
      print_exp_paren e2
  | App(id,argl,_) -> 
      print_string (name id);
      print_string "(";
      print_exp_list argl
  | ArrayAlloc(t,e,_) -> 
      print_string "new ";
      print_ty t;
      print_string "[";
      print_exp e;
      print_string "]"
  | Nil(_) -> print_string "NULL"
  | Void_exp -> ()
  | Alloc(t,_) -> 
      print_string "new ";
      print_ty t
  | True(_) -> print_string "true"
  | False(_) -> print_string "false"
and print_var = function 
  | SimpVar(id,_) -> print_string (name id)
  | FieldVar(v,id,_) -> 
    print_var v;
    print_string ("."^(name id))
  | SubscriptVar(v,e,_) ->
    print_var v;
    print_string "[";
    print_exp e;
    print_string "]";
and print_exp_list = function 
   [] -> print_string ")"
  | [e] -> print_exp e ; print_string ")"
  | e::el -> print_exp e; print_string ", "; print_exp_list el
and print_ty = function 
  | Int -> print_string "int"
  | Bool -> print_string "bool" 
  | Void -> print_string "()"
  | Arrow(tl,t) -> 
    print_string "(";
    print_ty_list tl;
    print_string " -> ";
    print_ty t
  | ArrayTy(t) -> 
    print_ty t;
    print_string "[]";
  | NameTy(id) -> print_string (name id)
  | Any -> ()
and print_field_list = function 
  | [] -> () 
  | (id,t)::l -> 
    print_string ((name id)^" ");
    print_ty t;
    print_string ";";
    print_field_list l
and print_ty_list = function 
  | [] -> print_string ")"
  | [t] -> print_ty t; print_string ")" 
  | t::tl -> print_ty t; print_string ", "; print_ty_list tl
and print_exp_paren = function 
  | Bin(_) as e ->  print_string "("; print_exp e; print_string")"
  | _ as e -> print_exp e
