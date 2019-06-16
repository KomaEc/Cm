(** Target of this intermedia representation:
 ** 1. isolate potentially effectful expressions, making their order
 ** of excution explicit. 
 ** 2. make the control flow explicit. 
 ** Therefore the IR here is described through pure expression and 
 ** command.  *)

 open Temp 
 open Types
 module UF = Cm_util.Union_and_find
 module S = Symbol

type immediate = [
  | `Const of const 
  | `Temp of Temp.t
]

and const = [
  | `Null_const 
  | `Int_const of int
  | `Bool_const of bool
]

and var = [
  | `Temp of Temp.t 
  | `Array_ref of immediate * immediate
  | `Instance_field_ref of immediate * field_signature
  | `Static_field_ref of Symbol.t
] 

and label = Temp.label 
 (* TODO : change label to a variant type 
  * Label of Temp.label | Line_num of int *)

and target = [
  | `Label of label 
  | `Line_num of int
]
(* Bugs!! rvalue should contains [`Invoke] !!*)
and rvalue = [
  | `Temp of Temp.t
  | `Const of const
  | `Expr of expr
  | `Array_ref of immediate * immediate
  | `Instance_field_ref of immediate * field_signature
  | `Static_field_ref of Symbol.t
]

and method_signature = Symbol.t * ty list * ty

and field_signature = Symbol.t * ty

and stmt = [
   | `Assign of var * rvalue
   | `Label of label 
   | `Goto of target 
   | `If of condition * target
   | `Static_invoke of method_signature * immediate list
   | `Ret of immediate 
   | `Ret_void
   | `Nop (* For optimization purpose *)
 ]

and identity = [
  | `Identity of [ `Temp of Temp.t ] * identity_value
]

and local_decl = [
  | `Temp_decl of [ `Temp of Temp.t ] * ty
]

and condition = [
  | `Temp of Temp.t
  | `Rel of immediate * relop * immediate
 ]

 and expr = [
   | `Bin of immediate * binop * immediate
   | `Rel of immediate * relop * immediate
   | `Static_invoke of method_signature * immediate list
   | `New_expr of obj_type
   | `New_array_expr of ty * immediate
 ]

 and identity_value = [
   | `Parameter_ref of int
 ]

 and binop = [ `Plus | `Minus | `Times | `Div ]

 and relop = [ `Eq | `Lt | `Gt | `And | `Or | `Not ]

and func = 
  {  func_name : Symbol.t;
     func_args : ty list;
     func_ret : ty;
     (* TODO : add identity declaration!!! *)
     local_decls : local_decl list;
     identities : identity list;
     func_body : stmt list
  }


(* TODO : add glb_vars info and cls info! *)
and prog = func list * (Symbol.t * Types.ty) list Symbol.table

let get_args : func -> Temp.t list = 
  fun func -> 
    List.rev_map
      (fun (`Identity(`Temp(t), _)) -> t) func.identities

let rvalue_to_var : rvalue -> var = function 
  | `Temp(t) -> `Temp(t) 
  | `Array_ref(x) -> `Array_ref(x)
  | `Instance_field_ref(x) -> `Instance_field_ref(x)
  | `Static_field_ref(x) -> `Static_field_ref(x) 
  | _ -> failwith "impossible"

module type MIMPLE_VISITOR = 
sig
  class visitor : 
  object ('self_type)

    method stmt : stmt -> (stmt * 'self_type)
    method expr : expr -> (expr * 'self_type)
    method rvalue : rvalue -> (rvalue * 'self_type)
    method immediate : immediate -> (immediate * 'self_type)
    method var : var -> (var * 'self_type)
    method label : label -> (label * 'self_type)
    method target : target -> (target * 'self_type)
    method condition : condition -> (condition * 'self_type)
    method const : const -> (const * 'self_type)
    method temp : Temp.t -> (Temp.t * 'self_type)
  end

end

module Transform : MIMPLE_VISITOR  =
struct
  class visitor = 
  object ((o : 'self_type))
  method stmt : stmt -> (stmt * 'self_type) = function 
    | `Assign(v, rv) -> 
      let (v', o) = o#var v in 
      let (rv', o) = o#rvalue rv in 
      `Assign(v', rv'), o
    | `Label(l) -> 
      let (l', o) = o#label l in 
      `Label(l'), o
    | `Goto(t) -> 
      let (t', o) = o#target t in 
      `Goto(t'), o 
    | `If(cond, t) -> 
      let (cond', o) = o#condition cond in 
      let (t', o) = o#target t in 
      `If(cond', t'), o 
    | `Static_invoke(msig, imm_list) -> 
      let (imm_list_rev, o) = 
        List.fold_left  
          (fun (acc, o) imm -> 
            let (imm', o) = o#immediate imm in
            imm'::acc, o) ([], o) imm_list in 
      `Static_invoke(msig, List.rev imm_list_rev), o
    | `Ret(imm) -> 
      let (imm', o) = o#immediate imm in 
      `Ret(imm'), o 
    | _ as s -> s, o

  method rvalue : rvalue -> (rvalue * 'self_type) = function 
    | `Temp(t) -> 
      let (t', o) = o#temp t in 
      `Temp(t'), o
    | `Const(c) -> 
      let (c', o) = o#const c in 
      `Const(c'), o
    | `Expr(expr) -> 
      let (expr', o) = o#expr expr in 
      `Expr(expr'), o
    | `Array_ref(imm1, imm2) ->
      let (imm1', o) = o#immediate imm1 in 
      let (imm2', o) = o#immediate imm2 in 
      `Array_ref(imm1', imm2'), o
    | `Instance_field_ref(imm, fsig) -> 
      let (imm', o) = o#immediate imm in 
      `Instance_field_ref(imm', fsig), o
    | `Static_field_ref(_) as s ->
      s, o 

  method var : var -> (var * 'self_type) = fun v -> 
    let (rv, o) = o#rvalue (v :> rvalue) in
    let v' = rvalue_to_var rv in 
    v', o
  
  method target : target -> (target * 'self_type) = fun t -> t, o
  method label : label -> (label * 'self_type) = fun l -> l, o
  method immediate : immediate -> (immediate * 'self_type) = fun imm -> imm, o
  method condition : condition -> (condition * 'self_type) = function 
    | `Temp(t) -> 
      let (t', o) = o#temp t in 
      `Temp(t'), o 
    | `Rel(imm1, rop, imm2) -> 
      let (imm1', o) = o#immediate imm1 in 
      let (imm2', o) = o#immediate imm2 in 
      `Rel(imm1', rop, imm2'), o 
  method temp : Temp.t -> (Temp.t * 'self_type) = fun t -> t, o
  method const : const -> (const * 'self_type) = fun c -> c, o
  method expr : expr -> (expr * 'self_type) = function 
    | `Bin(imm1, bop, imm2) -> 
      let (imm1', o) = o#immediate imm1 in 
      let (imm2', o) = o#immediate imm2 in 
      `Bin(imm1', bop, imm2'), o
    | `Rel(imm1, rop, imm2) -> 
      let (imm1', o) = o#immediate imm1 in 
      let (imm2', o) = o#immediate imm2 in 
      `Rel(imm1', rop, imm2'), o 
    | `Static_invoke(msig, imm_list) -> 
      let (imm_list_rev, o) = 
        List.fold_left  
          (fun (acc, o) imm -> 
            let (imm', o) = o#immediate imm in
            imm'::acc, o) ([], o) imm_list in 
      `Static_invoke(msig, List.rev imm_list_rev), o
    | `New_expr(_) as e -> e, o
    | `New_array_expr(ty, imm) ->
      let (imm', o) = o#immediate imm in 
      `New_array_expr(ty, imm'), o    

  end
end


module type MIMPLE_FOLDER = 
sig
  class ['acc, 'obj] visitor :
  object('self_type)
    method stmt : ('acc -> 'obj -> 'acc) -> 'acc -> stmt -> ('acc * 'self_type)
    method expr : ('acc -> 'obj -> 'acc) -> 'acc -> expr -> ('acc * 'self_type)
    method rvalue : ('acc -> 'obj -> 'acc) -> 'acc -> rvalue -> ('acc * 'self_type)
    method immediate : ('acc -> 'obj -> 'acc) -> 'acc -> immediate -> ('acc * 'self_type)
    method var : ('acc -> 'obj -> 'acc) -> 'acc -> var -> ('acc * 'self_type)
    method label : ('acc -> 'obj -> 'acc) -> 'acc -> label -> ('acc * 'self_type)
    method target : ('acc -> 'obj -> 'acc) -> 'acc -> target -> ('acc * 'self_type)
    method condition : ('acc -> 'obj -> 'acc) -> 'acc -> condition -> ('acc * 'self_type)
    method const : ('acc -> 'obj -> 'acc) -> 'acc -> const -> ('acc * 'self_type)
    method temp : ('acc -> 'obj -> 'acc) -> 'acc -> Temp.t -> ('acc * 'self_type)  
  end
end

module Fold : MIMPLE_FOLDER = 
struct
  class ['acc, 'obj] visitor =
  object((o : 'self_type))
  method stmt : ('acc -> 'obj -> 'acc) -> 'acc -> stmt -> ('acc * 'self_type) = fun f acc -> function 
    | `Assign(v, rv) -> 
      let (acc', o) = o#var f acc v in 
      let (acc'', o) = o#rvalue f acc' rv in 
      acc'', o
    | `Label(l) -> 
      o#label f acc l
    | `Goto(t) -> 
      o#target f acc t
    | `If(cond, t) -> 
      let (acc', o) = o#condition f acc cond in 
      o#target f acc' t 
    | `Static_invoke(_, imm_list) -> 
      List.fold_left
        (fun (acc', o') imm -> 
          o'#immediate f acc' imm) (acc, o) imm_list
    | `Ret(imm) -> 
      o#immediate f acc imm
    | _  -> acc, o

  method rvalue : ('acc -> 'obj -> 'acc) -> 'acc -> rvalue -> ('acc * 'self_type) = fun f acc -> function 
    | `Temp(t) -> 
      o#temp f acc t
    | `Const(c) -> 
      o#const f acc c
    | `Expr(expr) -> 
      o#expr f acc expr
    | `Array_ref(imm1, imm2) ->
      let (acc', o') = o#immediate f acc imm1 in
      o'#immediate f acc' imm2
    | `Instance_field_ref(imm, _) -> 
      o#immediate f acc imm
    | `Static_field_ref(_) ->
      acc, o 

  method var : ('acc -> 'obj -> 'acc) -> 'acc -> var -> ('acc * 'self_type) = fun f acc v -> 
    o#rvalue f acc (v :> rvalue)
  
  method target : ('acc -> 'obj -> 'acc) -> 'acc -> target -> ('acc * 'self_type) = fun _ acc _ -> acc, o
  method label : ('acc -> 'obj -> 'acc) -> 'acc -> label -> ('acc * 'self_type) = fun _ acc _ -> acc, o
  method immediate : ('acc -> 'obj -> 'acc) -> 'acc -> immediate -> ('acc * 'self_type) = fun _ acc _ -> acc, o
  method condition : ('acc -> 'obj -> 'acc) -> 'acc -> condition -> ('acc * 'self_type) = fun f acc -> function 
    | `Temp(t) -> 
      o#temp f acc t
    | `Rel(imm1, _, imm2) -> 
      let (acc', o') = o#immediate f acc imm1 in
      o'#immediate f acc' imm2
  method temp : ('acc -> 'obj -> 'acc) -> 'acc -> Temp.t -> ('acc * 'self_type) = fun _ acc _ -> acc, o
  method const : ('acc -> 'obj -> 'acc) -> 'acc -> const -> ('acc * 'self_type) = fun _ acc _ -> acc, o
  method expr : ('acc -> 'obj -> 'acc) -> 'acc -> expr -> ('acc * 'self_type) = fun f acc -> function 
    | `Bin(imm1, _, imm2) -> 
      let (acc', o') = o#immediate f acc imm1 in
      o'#immediate f acc' imm2
    | `Rel(imm1, _, imm2) -> 
      let (acc', o') = o#immediate f acc imm1 in
      o'#immediate f acc' imm2
    | `Static_invoke(_, imm_list) -> 
      List.fold_left
        (fun (acc', o') imm -> 
          o'#immediate f acc' imm) (acc, o) imm_list
    | `New_expr(_) -> acc, o
    | `New_array_expr(_, imm) ->  
      o#immediate f acc imm
  end
end








let get_locals (func : func) : (Temp.t * ty) list = 
  List.map (fun (`Temp_decl(`Temp(t), ty)) -> (t, ty)) func.local_decls

let get_formals (func : func) : (Temp.t * ty) list = 
  List.map2 
  (fun (`Identity(`Temp(t), _)) ty -> (t, ty)) func.identities func.func_args

let convert_to_lnum func : func = 
  let instrs = Array.of_list func.func_body in
  let tbl = Hashtbl.create 16 in 
  let offset = ref 0 in 
  Array.iteri 
  (fun i -> function 
  | `Label(l) -> 
    Hashtbl.add tbl l (i - !offset);
    incr offset 
  | _ -> ()) instrs;
  let aux = Array.make (Array.length instrs - !offset) `Nop in
  offset := 0;
  Array.iter
  (function 
  | `Label(_) -> () 
  | `Goto(`Label(l)) -> 
    aux.(!offset) <- (`Goto(`Line_num(Hashtbl.find tbl l)));
    incr offset 
  | `If(cond, `Label(l)) -> 
    aux.(!offset) <- (`If(cond, `Line_num(Hashtbl.find tbl l)));
    incr offset
  | _ as stmt -> 
    aux.(!offset) <- stmt;
    incr offset) instrs;
  let func_body = 
    Array.fold_right (fun stmt acc -> stmt :: acc) aux [] in
  {
    func with func_body = func_body
  }


let transform_stmt (to_var : Temp.t -> immediate) : stmt -> stmt * bool = 
  let flag = ref false in

let rec transform_stmt : stmt -> stmt = 
  function 
    | `Assign(var, rvalue) -> 
      begin 
        match var with 
          | `Temp(t) -> 
            begin 
              match to_var t with 
                | `Const(c) -> flag := true; `Assign(var, `Const(c))
                | _ -> `Assign(transform_var var, transform_rvalue rvalue)
            end 
          | _ as var -> `Assign(transform_var var, transform_rvalue rvalue)
      end
    | `Static_invoke(msig, il) -> 
      `Static_invoke(msig, List.map transform_immediate il) 
    | `Ret(i) -> `Ret(transform_immediate i)
    | _ as s -> s 
and transform_var = 
  function 
    | `Array_ref(i, i') -> `Array_ref(i, transform_immediate i') 
    | _ as var -> var
and transform_rvalue  = 
  function 
    | `Temp(t) -> (to_var t :> rvalue) 
    | `Expr(expr) -> `Expr(transform_expr expr) 
    | `Array_ref(i, i') -> `Array_ref(i, transform_immediate i')
    | _ as v -> v 
and transform_expr = 
  function 
    | `Bin(i1, op, i2) -> 
      `Bin(transform_immediate i1, op, transform_immediate i2) 
    | `Rel(i1, op, i2) -> 
      `Rel(transform_immediate i1, op, transform_immediate i2) 
    | `Static_invoke(msig, il) -> 
      `Static_invoke(msig, List.map transform_immediate il) 
    | `New_array_expr(ty, i) -> `New_array_expr(ty, transform_immediate i) 
    | _ as e -> e 
and transform_immediate = 
  function 
    | `Temp(t) -> 
      begin
        match to_var t with 
          | `Const(_) as c -> flag := true; c 
          | _ as t -> t 
      end
    | _ as c -> c in 

    fun stmt -> transform_stmt stmt, !flag




  
let rec temps_in_rvalue : rvalue -> Temp.t list = function 
  | `Temp(t) -> [t]
  | `Expr(expr) -> temps_in_expr expr 
  | `Array_ref(i1, i2) -> temps_in_immediate i1 @ temps_in_immediate i2 
  | `Instance_field_ref(i, _) -> temps_in_immediate i 
  | _ -> [] 
  
and temps_in_expr : expr -> Temp.t list = function 
  | `Bin(i1, _, i2) | `Rel(i1, _, i2) -> 
    temps_in_immediate i1 @ temps_in_immediate i2 
  | `Static_invoke(_, i_list) -> 
    List.fold_left (fun acc i -> temps_in_immediate i @ acc) [] i_list 
  | `New_array_expr(_, i) -> temps_in_immediate i 
  | _ -> [] 

and temps_in_immediate : immediate -> Temp.t list = 
  fun x -> temps_in_rvalue (x :> rvalue)

and temps_in_var : var -> Temp.t list = 
  fun x -> temps_in_rvalue (x :> rvalue)

and temps_in_condition : condition -> Temp.t list = function 
  | `Temp(t) -> [t] 
  | `Rel(x) -> temps_in_expr (`Rel(x))

(* Printing Utility *)

let var_to_rvalue : var -> rvalue = 
  fun x -> (x :> rvalue)


let immediate_to_rvalue : immediate -> rvalue = 
  fun x -> (x :> rvalue)

let string_of_const : const -> string = 
  function
    | `Int_const(num) -> string_of_int num 
    | `Null_const -> "NULL"
    | `Bool_const(true) -> "true"
    | `Bool_const(false) -> "false"


let rec string_of_value : rvalue -> string = 
  function 
    | `Temp(t) -> string_of_temp t 
    | `Const(c) -> string_of_const c
    | `Expr(expr) -> string_of_expr expr 
    | `Array_ref(i, i') -> 
      string_of_value (immediate_to_rvalue i) ^ "[" ^ string_of_value (immediate_to_rvalue i') ^ "]"
    | `Instance_field_ref(i, fsig) -> 
      string_of_value (immediate_to_rvalue i) ^ "." ^ string_of_field_sig fsig
    | `Static_field_ref(id) -> 
      Symbol.name id 

and string_of_method_sig : method_signature -> string = 
  fun (label, ty_list, ty) -> 
    string_of_label label 
    ^ " [" ^ string_of_ty_list ty_list ^ " : " ^ string_of_ty ty ^ "]"

and string_of_field_sig : field_signature -> string = 
  fun (name, ty) -> 
    Symbol.name name ^ " : " ^ string_of_ty ty

and string_of_target : target -> string = 
  function 
    | `Label(l) -> string_of_label l 
    | `Line_num(i) -> string_of_int i

and string_of_stmt : stmt -> string = 
  function 
    | `Assign(var, rvalue) -> 
      "  " ^ string_of_value (var_to_rvalue var) ^ " = " ^ string_of_value rvalue 
      ^ ";"
    | `Label(l) -> 
      string_of_label l ^ ":"
    | `Goto(l) -> 
      "  goto " ^ string_of_target l ^ ";"
    | `If(`Temp(t), l) -> 
      "  if " ^ string_of_temp t ^ " goto " ^ string_of_target l ^ ";"
    | `If(`Rel(_) as rexpr, l) -> 
      "  if " ^ string_of_expr rexpr ^ " goto " ^ string_of_target l ^ ";"
    | `Static_invoke(_) as expr -> 
      "  " ^ string_of_expr expr ^ ";"
    | `Ret(i) -> 
      "  return " ^ string_of_value (immediate_to_rvalue i) ^ ";"
    | `Ret_void -> 
      "  return;"
    | `Nop -> ""

and string_of_decl : local_decl -> string = 
  function 
    | `Temp_decl(`Temp(t), ty) -> 
    "  " ^ string_of_ty ty ^ " " ^ string_of_temp t ^ ";"

and string_of_identity : identity -> string = 
  function 
  | `Identity(`Temp(t), id_value) -> 
      "  " ^ string_of_value (`Temp(t)) ^ " := " 
      ^ string_of_identity_value id_value ^ ";"

and string_of_expr : expr -> string = 
  function 
    | `Bin(i1, bop, i2) -> 
      string_of_value (immediate_to_rvalue i1) ^ string_of_bop bop ^ string_of_value (immediate_to_rvalue i2) 
    | `Rel(i1, rop, i2) -> 
      string_of_value (immediate_to_rvalue i1) ^ string_of_rop rop ^ string_of_value (immediate_to_rvalue i2) 
    | `Static_invoke(msig, i_list) -> 
      string_of_method_sig msig ^ "(" ^ string_of_immediate_list i_list
    | `New_expr(obj_type) ->
      "new " ^ string_of_ty (Object(obj_type)) ^ "()"
    | `New_array_expr(ty, i) -> 
      "new " ^ string_of_ty ty ^ "[" 
      ^ string_of_value (immediate_to_rvalue i) ^ "]"

and string_of_identity_value : identity_value -> string = 
  function
    | `Parameter_ref(num) -> 
      "@P" ^ string_of_int num

and string_of_bop : binop -> string = 
  function 
    | `Plus -> " + "
    | `Times -> " * "
    | `Minus -> " - "
    | `Div -> " / "

and string_of_rop : relop -> string = 
  function 
    | `Eq -> " == "
    | `Lt -> " < "
    | `Gt -> " > "
    | `And -> " && "
    | `Or -> " || "
    | `Not -> "!"

and string_of_immediate_list : immediate list -> string = 
  function 
    | [] -> ")"
    | [x] -> string_of_value (immediate_to_rvalue x) ^ ")"
    | x :: xl -> string_of_value (immediate_to_rvalue x) ^ ", " 
                 ^ string_of_immediate_list xl

let string_of_func : func -> string = 
  fun func -> 
    "\nBeginFunc " ^ Symbol.name func.func_name 
    ^ " : " ^ string_of_ty_list func.func_args ^ " -> " 
    ^ string_of_ty func.func_ret ^ "\n"
    (*^ (List.fold_left (fun acc decl -> acc ^ string_of_decl decl ^ "\n") "" local_decls)
    ^ (List.fold_left (fun acc idt -> acc ^ string_of_identity idt ^ "\n") "" identities)*)
    ^ (List.fold_left (fun acc stmt -> acc ^ string_of_stmt stmt ^ "\n") "" func.func_body)
    ^ "EndFunc\n"
    

let string_of_prog : prog -> string = fun (prog, _) -> 
  List.fold_left
    (fun prev method_chunk -> 
      prev ^ string_of_func method_chunk) "" prog

let print_prog : prog -> unit = 
  fun prog -> string_of_prog prog |> print_endline