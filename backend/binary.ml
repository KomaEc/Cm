open Assembler
open Cm_core.Mimple
module Temp = Cm_core.Temp
module Types = Cm_core.Types
module Symbol = Cm_core.Symbol

module Config(X : sig val name2index : Symbol.t -> int val args : Temp.t list end) = 
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
        let f_reg, o = o#get_reg(), o#incr() in
        let ops, o = 
          List.fold_left
            (fun (ops, o) reg -> 
              ops@[Move(o#get_reg(), reg)], o#incr ()) (ops, o) reg_list in
        let ops= ops @ [Closure(f_reg, p_idx)] in
        let ops = ops@[Call(f_reg, args_num+1, ret_num+1)] in 
        f_reg, ops, o
      | _ -> failwith "Compiling to lua bytecode ------ new expr not implemented "


    method rvalue : rvalue -> int * lua_ops list * 'self = function 
      | `Temp(t) -> TempMap.find t temps2reg, [], o (* must've been defined !*)
      | `Const(const) -> 
        let lua_const, o' = o#const const in
        cur_reg, [Load_Const(cur_reg, lua_const)], o'#incr ()
      | `Expr(expr) -> 
        o#expr expr
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




let compile (prog : prog) = 
  let (funcs, _) = prog in 
  let funcs = List.map convert_to_lnum funcs in
  let () = 
    List.iter 
      (fun func -> print_endline (string_of_func func)) funcs
    in 
  let main = 
    try List.find (fun func -> func.func_name = Symbol.symbol "main") funcs
    with _ -> failwith "No Main Routine!!" in 
  let funcs = List.filter (fun func -> func.func_name <> Symbol.symbol "main") funcs in 
  let tbl = Hashtbl.create 16 in 
  List.iteri (fun i func -> Hashtbl.add tbl func.func_name i) funcs;
  let sub_units = 
    List.map (fun func -> 
      let module X = struct let name2index = Hashtbl.find tbl let args = get_args func end in 
      let module C = Config(X) in 
      let visitor = new C.visitor in 
      let body = visitor#func func.func_body |> fst in 
      let num_params = List.length func.func_args in 
      {
        instructions = body;
        num_params;
        child_functions = [];
      }) funcs in
  let module X = struct let name2index = Hashtbl.find tbl let args = [] end in 
  let module C = Config(X) in 
  let visitor = new C.visitor in 
  let main_body = visitor#func main.func_body |> fst in 
  let func_unit = 
  {
    instructions = main_body;
    num_params = 0;
    child_functions = sub_units;
  } in 
  let out_chan = open_out_bin "cm.out" in
    assemble func_unit out_chan
