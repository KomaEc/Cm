(*
type ident = string 
type const = int 
type binop = 
  | Plus | Minus | Times | Div 
type prog = stm list 
and stm = 
  | Assign of ident * exp 
  | Return of exp 
and exp = 
  | Const of const 
  | Var of ident 
  | Bin of exp * binop * exp 
*)
open Support.Error

type stmt = 
  | Assign of var * exp * info
  | If of exp * stmt * stmt option * info
  | While of exp * stmt * info 
  | Return of exp * info 
  | Nop
  | Exp of exp * info 
  | Seq of stmt list * info 
  | Vardecl of ident * ty * stmt * info  (* succesive statement *)
  | Fundecl of ident * ty * stmt * info 
  | Fundefn of ident * ident list * ty * stmt * stmt * info
  | Structdecl of ident * stmt * info 
  | Structdefn of ident * (ident * ty) list * stmt * info

and ident = Symbol.t 

and exp = 
  | Intconst of int * info
  | True of info 
  | False of info
  | Var of var
  | Bin of exp * binop * exp * info
  | Un of unop * exp * info
  | App of ident * exp list * info
  | ArrayAlloc of ty * exp * info
  | Alloc of ty * info
  | Nil of info
  | Void_exp

and var = 
  | SimpVar of ident * info 
  | FieldVar of var * ident * info 
  | SubscriptVar of var * exp * info

and binop = Plus | Minus | Times | Div | And | Or | Lt | Gt | Eq

and unop = Not

and ty = Int | Bool | Void
       | Arrow of ty list * ty
       | ArrayTy of ty
       | NameTy of ident | Any


module Util = struct

  let cons s i sl = 
    match sl with 
    | Seq(sl',_) -> Seq(s::sl',i) 
    | _ as sl -> Seq([s;sl], i)

  let rec simplify = function 
    | If(e,s,sop,i) -> 
      (match sop with 
       | Some s' -> If(e, simplify s, Some (simplify s'),i)
       | None -> If(e, simplify s, sop, i))
    | While(e,s,i) -> While(e, simplify s,  i) 
    | Seq([], _) -> Nop 
    | Seq([s], _) -> s
    | Seq(sl, i) -> Seq(simplify_seq sl, i)
    | Vardecl(id,t,s,i) -> Vardecl(id,t,simplify s,i) 
    | Fundecl(id,t,s,i) -> Fundecl(id,t,simplify s,i)
    | Fundefn(id,idl,t,s',s,i) -> Fundefn(id,idl,t,simplify s',simplify s,i)
    | _ as s -> s 
  and simplify_seq sl = 
    List.fold_right 
      (fun s acc -> 
      match simplify s with 
      | Seq(sl', _) -> sl' @ acc 
      | _ as s' -> s' :: acc) sl []

  let rec extract_info_stmt = function 
    | Assign(_,_,i) -> i 
    | If(_,_,_,i) -> i 
    | While(_,_,i) -> i 
    | Return(_,i) -> i 
    | Nop -> dummyinfo 
    | Exp(_,i) -> i 
    | Seq(_,i) -> i 
    | Vardecl(_,_,_,i) -> i 
    | Fundecl(_,_,_,i) -> i 
    | Fundefn(_,_,_,_,_,i) -> i
    | Structdecl(_,_,i) -> i 
    | Structdefn(_,_,_,i) -> i
  and extract_info_var = function 
    | SimpVar(_,i) -> i 
    | FieldVar(_,_,i) -> i 
    | SubscriptVar(_,_,i) -> i
  and extract_info_exp = function 
    | Intconst(_,i) -> i 
    | Var(v) -> extract_info_var v
    | Bin(_,_,_,i) -> i 
    | Un(_,_,i) -> i 
    | App(_,_,i) -> i
    | ArrayAlloc(_,_,i) -> i 
    | Alloc(_,i) -> i
    | True(i) -> i 
    | False(i) -> i
    | Nil(i) -> i
    | Void_exp -> dummyinfo

end
