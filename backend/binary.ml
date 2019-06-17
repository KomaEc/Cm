open Assembler
open Cm_core.Mimple
module Temp = Cm_core.Temp
module Types = Cm_core.Types
module Symbol = Cm_core.Symbol

module Config(X : sig val name2index : Symbol.t -> int val args : Temp.t list val lib_handler : int -> int -> int list -> lua_ops list * int val lib_length : int val is_child : bool val class_info : (Temp.label * Types.ty) list Symbol.table end) = 
struct

  module TempMap = Map.Make(struct type t = Temp.t let compare = compare end)
  class visitor = 
  object((o : 'self))

    val cur_reg : int = List.length X.args

    val temps2reg : int TempMap.t = 
      let i = ref (-1) in 
      List.fold_left
        (fun acc t -> incr i; TempMap.add t !i acc) TempMap.empty X.args

    method get_reg : unit -> int = fun () -> cur_reg

    method incr : unit -> 'self = fun () -> {<cur_reg = cur_reg + 1>}
    
    method const : const -> lua_const * 'self = function
      | `Null_const -> `L_Nill, o
      | `Int_const(i) -> `L_Double(float_of_int(i)), o
      | `Bool_const(b) -> `L_Bool(b), o

    method immediate : immediate -> rk_source * 'self = function 
      | `Const(const) -> 
        let lua_const, o' = o#const const in 
        (lua_const :> rk_source), o'
      | `Temp(t) -> 
        `Register(TempMap.find t temps2reg), o

    method immediate' : immediate -> int * lua_ops list * 'self = function 
      | `Const(const) ->  
        let lua_const, o = o#const const in
        cur_reg, [Load_Const(cur_reg, lua_const)], o#incr ()
      | `Temp(t) -> 
        let reg = TempMap.find t temps2reg in
        reg, [], o

    method binop : binop -> arith_op_types * 'self = function
      | `Plus -> Arith_Add, o
      | `Minus -> Arith_Sub, o
      | `Times -> Arith_Mul, o
      | `Div -> Arith_Div, o

    method expr : expr -> int * lua_ops list * 'self = function
      | `Bin(imm1, binop, imm2) ->
        let left_src, o = o#immediate imm1 in 
        let right_src, o = o#immediate imm2 in 
        let op_type, o = o#binop binop in 
        let arith_op = {
          op_type;
          left_src;
          right_src;
          dest = cur_reg;
        } in 
        cur_reg, [Arith arith_op], o#incr ()
      | `Rel(_) -> failwith "Compiling to lua bytecode ------ relation operation not implemented."
      | `Static_invoke(msig, imm_list) -> 
        let (pname, ty_list, ret_ty) = msig in 
        let p_idx = X.name2index pname in 
        let args_num = List.length ty_list in 
        let ret_num = match ret_ty with Types.Primitive(`Void) -> 0 | _ -> 1 in 
        let reg_list, ops, o = 
          List.fold_left
            (fun (acc, ops, o) imm -> 
              let (reg, ops', o) = o#immediate' imm in 
              acc@[reg], ops@ops', o) ([], [], o) imm_list in 
        if p_idx < X.lib_length then 
          let ops, cur_reg' = X.lib_handler p_idx (o#get_reg()) reg_list in 
          -1, ops, {<cur_reg = cur_reg'>}
        else
        let f_reg, o = o#get_reg(), o#incr() in
        let ops, o = 
          List.fold_left
            (fun (ops, o) reg -> 
              ops@[Move(o#get_reg(), reg)], o#incr ()) (ops, o) reg_list in
        let func_ops = 
        begin
          if X.is_child then [Get_Global(f_reg, Symbol.name pname)]
          else [Closure(f_reg, p_idx); Set_Global(Symbol.name pname, f_reg)] 
        end in
        let ops= ops @ func_ops in
        let ops = ops@[Call(f_reg, args_num+1, ret_num+1)] in 
        f_reg, ops, o
      | `New_array_expr(_, imm) -> 
        let _, o = o#immediate imm in 
        o#get_reg(), [Make_Table(o#get_reg(), 100, 0)], o#incr()
      | `New_expr(`ClassTy(class_name)) -> 
        let field_ty_list = Symbol.lookup class_name X.class_info in 
        let tbl_reg, o = o#get_reg(), o#incr() in 
        let op = [Make_Table(tbl_reg, 0, 100)] in
        let ops = List.fold_left
          (fun acc (fname, _) -> 
            acc@[Set_Table(tbl_reg, (`L_String (Symbol.name fname)), `L_Nill)]) op field_ty_list in 
        tbl_reg, ops, o

      | _ -> failwith "Compiling to lua bytecode ------ new expr not implemented "


    method rvalue : rvalue -> int * lua_ops list * 'self = function 
      | `Temp(t) -> TempMap.find t temps2reg, [], o (* must've been defined !*)
      | `Const(const) -> 
        let lua_const, o' = o#const const in
        cur_reg, [Load_Const(cur_reg, lua_const)], o'#incr ()
      | `Expr(expr) -> 
        o#expr expr
      | `Array_ref(imm1, imm2) -> 
        let tbl_reg, ops, o = o#immediate' imm1 in 
        let key_src, o = o#immediate imm2 in 
        o#get_reg(), ops@[Load_Table(o#get_reg(), tbl_reg, key_src)], o
      | `Instance_field_ref(imm1, (fname, _)) -> 
        let tbl_reg, ops, o = o#immediate' imm1 in 
        let key_src : rk_source = `L_String (Symbol.name fname) in
        o#get_reg(), ops@[Load_Table(o#get_reg(), tbl_reg, key_src)], o
      | _ -> failwith "Compiling to lua bytecode ------ part of rvalue not implemented "
    
    method var : var -> int * lua_ops list * 'self = function
      | `Temp(t) -> 
        begin
        try let reg = TempMap.find t temps2reg in 
          reg, [], o 
        with Not_found ->
        let newmap = TempMap.add t cur_reg temps2reg in 
        cur_reg, [], {<cur_reg = cur_reg + 1; temps2reg = newmap>} end
      | _ -> failwith "Compiling to lua bytecode ------ part of var not implemented "

    method stmt : stmt -> lua_ops list * 'self = function
      | `Assign(`Array_ref(imm1, imm2), rvalue) -> 
        let tbl_reg, ops, o = o#immediate' imm1 in 
        let idx_src, o = o#immediate imm2 in 
        let val_reg, ops', o = o#rvalue rvalue in 
        ops@ops'@[Set_Table(tbl_reg, idx_src, `Register(val_reg))], o
      | `Assign(`Instance_field_ref(imm1, (fname, _)), rvalue) -> 
        let tbl_reg, ops, o = o#immediate' imm1 in 
        let idx_src : rk_source = `L_String (Symbol.name fname) in 
        let val_reg, ops', o = o#rvalue rvalue in 
        ops@ops'@[Set_Table(tbl_reg, idx_src, `Register(val_reg))], o
      | `Assign(var, rvalue) -> 
        let (reg_var, var_ops, o) = o#var var in 
        let (reg_rvalue, rvalue_ops, o) = o#rvalue rvalue in 
        var_ops@rvalue_ops@[(Move(reg_var, reg_rvalue))(*; Move(10, reg_var); Get_Global(9, "print"); Call(9, 2, 1)*)], o
      | `Ret(imm) -> 
        let (reg, ops, o) = o#immediate' imm in 
        ops@[Return(Return_One(reg))], o
      | `Ret_void -> 
        [Return(No_Value)], o
      | `Goto(`Line_num(i)) -> [Jump(i)], o
      | `If(`Temp(t), `Line_num(i)) -> 
        let reg = TempMap.find t temps2reg in 
        [Cmp({skip_if_not = true; right_operand = `Register(reg); left_operand = `L_Double(0.); comp_type = Comp_Lt}); Jump(i)], o
      | `If(`Rel(imm1, `Eq, imm2), `Line_num(i)) -> 
        let left_operand, o = o#immediate imm1 in 
        let right_operand, o = o#immediate imm2 in 
        [Cmp({skip_if_not = true; right_operand; left_operand; comp_type = Comp_Eq}); Jump(i)], o
      | `If(`Rel(imm1, `Lt, imm2), `Line_num(i)) -> 
        let left_operand, o = o#immediate imm1 in 
        let right_operand, o = o#immediate imm2 in 
        [Cmp({skip_if_not = true; right_operand; left_operand; comp_type = Comp_Lt}); Jump(i)], o
      | `If(`Rel(imm1, `Gt, imm2), `Line_num(i)) -> 
        let left_operand, o = o#immediate imm2 in 
        let right_operand, o = o#immediate imm1 in 
        [Cmp({skip_if_not = true; right_operand; left_operand; comp_type = Comp_Lt}); Jump(i)], o
      | `Static_invoke(_ as cont) -> 
        let (_, snd, thd) = o#expr (`Static_invoke(cont)) in 
        (snd, thd)
      | _ -> failwith "Unimplemented"

    
    method func : stmt list -> lua_ops list * 'self = fun stmt_list -> 
      let lnum2reallnum = Array.make (List.length stmt_list) (-1) in 
      let ops_list, o = 
        List.fold_left
          (fun (ops, o) stmt -> 
            let ops', o' = o#stmt stmt in 
            ops@[ops'], o') ([], o) stmt_list in
      let j = ref 0 in
      List.iteri (fun i ops -> lnum2reallnum.(i) <- !j; j := !j + List.length ops) ops_list;
      let ops = List.fold_right (fun ops acc -> ops @ acc) ops_list [] in 
      let ops = List.mapi 
        (fun j -> function 
          | Jump(i) -> 
            let i = lnum2reallnum.(i) in (*print_int (i-j-1); print_newline(); *)Jump(i-j-1)
          | _ as op -> op) ops in 
      ops@[Return(No_Value)], o

(*
    method func : stmt list -> lua_ops list * 'self = 
      List.fold_left
        (fun (acc, o) stmt -> 
          let ops, o = o#stmt stmt in 
          acc@ops, o) ([], o) *)


  end
end

module type LUA_PRINTER = 
sig 
  class visitor:
  object('self)
    method lua_ops : lua_ops -> string * 'self
    method lua_const : lua_const -> string * 'self
    method arith_op : arith_op -> string * 'self
    method comparison_op : comparison_op -> string * 'self
    method rk_source : rk_source -> string * 'self
    method arith_op_types : arith_op_types -> string * 'self
    method comparison_type : comparison_type -> string * 'self
    method return_val : return_val -> string * 'self
  end
end

module Printer : LUA_PRINTER = 
struct 
  class visitor = 
  object((o : 'self))

    val reg_helper : int -> string = 
      fun i -> "reg%" ^ (string_of_int i)

    method rk_source : rk_source -> string * 'self = function 
      | `L_String s -> "\"" ^ s ^ "\"", o
      | `L_Double d -> string_of_float d, o
      | `L_Bool b -> string_of_bool b, o
      | `L_Nill -> "nill", o
      | `Register i -> "reg%" ^ (string_of_int i), o
    method lua_const : lua_const -> string * 'self = 
      fun lc -> o#rk_source (lc :> rk_source)

    method arith_op_types : arith_op_types -> string * 'self = function
      | Arith_Add -> "\t+\t", o
      | Arith_Sub -> "\t-\t", o
      | Arith_Mul -> "\t*\t", o
      | Arith_Div -> "\t/\t", o
    
    method arith_op : arith_op -> string * 'self = 
      fun { op_type; left_src; right_src; dest} -> 
        let l, o = o#rk_source left_src in 
        let r, o = o#rk_source right_src in 
        let op, o = o#arith_op_types op_type in 
        (reg_helper dest) ^ " = " ^ l ^ op ^ r, o
    
    method comparison_type : comparison_type -> string * 'self = function 
      | Comp_Eq -> "\t=\t", o
      | Comp_Le -> "\t<=\t", o
      | Comp_Lt -> "\t<\t", o

    method comparison_op : comparison_op -> string * 'self = 
      fun { skip_if_not; right_operand; left_operand; comp_type } -> 
        let l, o = o#rk_source left_operand in 
        let r, o = o#rk_source right_operand in 
        let op, o = o#comparison_type comp_type in 
        l ^ op ^ r ^ "\t skip if not " ^ (string_of_bool skip_if_not), o

    method return_val : return_val -> string * 'self = function 
      | No_Value -> "", o
      | Return_One i -> reg_helper i, o 
      | _ -> failwith "imppossible"
      
    method lua_ops : lua_ops -> string * 'self = function
      | Load_Const(i, lua_const) -> 
        let const_string, o = o#lua_const lua_const in 
        "load_const\t" ^ (reg_helper i) ^"\t"^ const_string, o
      | Get_Global(i, s) -> 
        "get_global\t"^(reg_helper i) ^ "\t\"" ^ s ^ "\"", o 
      | Set_Global(s, i) -> 
        "set_global\t" ^ "\t\"" ^ s ^ "\"\t"^(reg_helper i), o 
      | Jump(i) -> 
        "jump\t" ^ (string_of_int i), o
      | Cmp(cmp) -> 
        o#comparison_op cmp
      | Move(i, j) -> 
        "mov\t" ^ (reg_helper i) ^ "\t" ^ (reg_helper j), o
      | Arith(arith) -> 
        o#arith_op arith 
      | Call(i, j, k) -> 
        "call\t" ^ (reg_helper i) ^ "\t" ^ (string_of_int j)^ "\t" ^ (string_of_int k), o
      | Closure(dest, p_index) -> 
        "closure\t" ^ (reg_helper dest) ^ "\t" ^ (string_of_int p_index), o
      | Load_Table(dst, table, key) ->
        let s, o = o#rk_source key in
        "load_table\t" ^ (reg_helper dst) ^ "\t" ^ (reg_helper table) ^ "\t" ^ s, o
      | Set_Table(table, k, v) -> 
        let s, o = o#rk_source k in 
        let s', o = o#rk_source v in 
        "set_table\t" ^ (reg_helper table) ^ "\t" ^ s ^ "\t" ^ s', o
      | Make_Table(i, j, k) -> 
        "make_table\t" ^ (reg_helper i) ^ "\t" ^ (string_of_int j) ^ "\t" ^ (string_of_int k), o
      | Return(ret_val) -> 
        let ret_s, o = o#return_val ret_val in 
        "return\t" ^ ret_s, o
      | _ -> 
        failwith "can't print! unimplemented"
  end
end

let string_of_lua : lua_ops list -> string = 
  let visitor = new Printer.visitor in 
  fun lua_list -> 
    List.map (fun s -> "\t" ^ (visitor#lua_ops s |> fst) ^ "\n") lua_list
    |> List.fold_left (^) ""



let base = [
  { func_name=Symbol.symbol "print_int"; func_args=[Types.Primitive(`Int)]; func_ret=Types.Primitive(`Void); local_decls=[]; identities=[]; func_body=[]}
]

let lib_func_length = 
  List.length base

let lib_handler i = 
  match i with 
    | 0 -> 
      fun cur_reg reg_ins -> 
        assert (List.length reg_ins = 1);
        let reg_in = List.hd reg_ins in
        [
          Get_Global(cur_reg, "string");
          Load_Table(cur_reg+1, cur_reg, `L_String "format");
          Load_Const(cur_reg+2, `L_String "%i");
          Move(cur_reg+3, reg_in);
          Call(cur_reg+1, 3, 2);
          Get_Global(cur_reg, "print");
          Call(cur_reg, 2, 1);
        ], cur_reg
    | _ -> failwith "impossible"


let compile (prog : prog) = 
  let (funcs, class_info) = prog in 
  let funcs = List.map convert_to_lnum funcs in
  (*let () = 
    List.iter 
      (fun func -> print_endline (string_of_func func)) funcs
    in *)
  let main = 
    try List.find (fun func -> func.func_name = Symbol.symbol "main") funcs
    with _ -> failwith "No Main Routine!!" in 
  let funcs = List.filter (fun func -> func.func_name <> Symbol.symbol "main") funcs |> (@) base in 
  let tbl = Hashtbl.create 16 in 
  List.iteri (fun i func -> Hashtbl.add tbl func.func_name i) funcs;
  let sub_units = 
    List.map (fun func -> 
      let module X = struct let name2index = Hashtbl.find tbl let args = get_args func let lib_handler = lib_handler let lib_length = lib_func_length let is_child = true let class_info = class_info end in 
      let module C = Config(X) in 
      let visitor = new C.visitor in 
      let body = visitor#func func.func_body |> fst in 

      let () = string_of_lua body |> print_endline in

      let num_params = List.length func.func_args in 
      {
        instructions = body;
        num_params;
        child_functions = [];
      }) funcs in
  let module X = struct let name2index = Hashtbl.find tbl let args = [] let lib_handler = lib_handler let lib_length = lib_func_length let is_child = false let class_info = class_info end in 
  let module C = Config(X) in 
  let visitor = new C.visitor in 
  let main_body = (visitor#func main.func_body |> fst) in 

  let () = string_of_lua main_body |> print_endline in

  let func_unit = 
  {
    instructions = main_body;
    num_params = 0;
    child_functions = sub_units;
  } in 
  let out_chan = open_out_bin "cm.out" in
    assemble func_unit out_chan
