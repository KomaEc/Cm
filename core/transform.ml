module M = Mimple
module Dfa = Dfa
module Ty = Types 
module UF = Cm_util.Union_and_find
module S = Symbol
module T = Temp 
module Bs = Bvset
module U = Cm_util.Util
open U

open M 



let simplify_func_body : stmt list -> stmt list = fun stmt_list ->
  let label_to_point : S.t UF.point S.table = 
    List.fold_left 
    (fun prev stmt ->
    match stmt with 
      | `Label(l) -> S.enter l (UF.fresh l) prev
      | _ -> prev) S.empty stmt_list in
  let rec dedup : stmt list -> stmt list = 
    function 
      | [] -> []
      | [s] -> [s]
      | s :: (s' :: _ as sl) ->
        begin 
          match s, s' with 
          (* [!!!] Problematic, since this label can be function 
           * Consider reveal the label type in Temp ??? *)
          (* [!!!] Problematic!!!! Can [`Label l] and 
           * [`Goto l'] really be pruned?????? 
           * I think so. But what if there's a jump
           * -to-nowhere "goto" ?? Cyclic? *)
            | `Label l, `Label l' | `Label l, `Goto (`Label l') ->
              UF.union (S.lookup l label_to_point) 
              (S.lookup l' label_to_point);
              dedup sl
            | _ -> s :: dedup sl
        end in
  let get_repr : label -> label = fun l ->
    let point = S.lookup l label_to_point in 
    UF.find point in
  let substitute : stmt -> stmt = 
    function 
      | `If(cond, `Label(l)) -> `If(cond, `Label (get_repr l))
      | `Goto(`Label(l)) -> `Goto(`Label(get_repr l))
      | `Label(l) -> `Label(get_repr l)
      | _ as s -> s in
  List.map substitute (dedup stmt_list)


module Fold_Label_Outter = 
struct
  class ['acc] visitor = 
  object((o : 'self_type))
  inherit ['acc, Mimple.label] Mimple.Fold.visitor as super
  (*method! label : ('acc -> Mimple.label -> 'acc) -> 'acc -> Mimple.label -> 'acc * 'self_type = 
    fun f acc l -> f acc l, o*)
  method! stmt : ('acc -> Mimple.label -> 'acc) -> 'acc -> Mimple.stmt -> 'acc * 'self_type = fun f acc -> function
    | `Goto(`Label(l))
    | `If(_, `Label(l)) -> 
      f acc l, o
    | _ as stmt -> super#stmt f acc stmt
  end
end

let unique_label : stmt list -> label -> int =
  let tbl : (label, int) Hashtbl.t = Hashtbl.create 16 in 
  let add l = 
    try
      match Hashtbl.find tbl l with 
        | n -> Hashtbl.replace tbl l (n+1)
    with Not_found -> Hashtbl.add tbl l 1 in
  let check : stmt -> unit = function
    | `Goto(`Label(l)) | `If(_, `Label(l)) -> add l
    | _ -> () in
  let rec scan : stmt list -> label -> int = function 
    | [] -> fun l -> (try Hashtbl.find tbl l with Not_found -> Hashtbl.add tbl l 0; 0)
    | [x] -> check x; scan []
    | (`Goto(`Label(l)))::(`Label(l')):: xs when l = l' -> 
      add l; scan xs
    | `Ret_void::_::xs | `Ret(_)::_::xs -> scan xs
    | (`Goto(_) as x)::xs -> check x; scan xs
    | x::(`Label(l))::xs -> 
      check x; add l; scan xs
    | x::xs -> check x; scan xs in scan

module Fold_label = 
struct
  class ['acc] visitor = 
  object((o : 'self_type))
    inherit ['acc, Mimple.label] Mimple.Fold.visitor as super
    method! target =
      fun f acc -> function 
        | `Label(l) -> f acc l, o
        | _ -> acc, o
    method! stmt = fun f acc -> function
      | `Label(l) -> f acc l, o
      | _ as s -> super#stmt f acc s
  end
end

let get_labels : stmt list -> label list = 
  let visitor = new Fold_label.visitor in
  fun stmt_list -> 
    List.fold_left
      (fun acc stmt -> acc @ (visitor#stmt (fun xs x -> xs @ [x]) [] stmt |> fst)) [] stmt_list
    |> List.sort_uniq compare


let goto_peephole : stmt list -> stmt list = fun stmt_list -> 
  let is_unique = unique_label stmt_list in
  let labels = get_labels stmt_list in
  Cm_util.Printing.string_of_list
    (fun l -> Temp.string_of_label l ^ " : " ^ string_of_int (is_unique l)) labels 
|> print_endline;
  let rec aux = function
    | [] -> []
    | [s] -> [s]
    | (`Ret(_) as r)::((`Label(l))::_ as sl) | (`Ret_void as r)::((`Label(l))::_ as sl) when is_unique l >= 1 -> r :: aux sl (* !!!!! since when finding next label, the one next to return doesn't count. *)
    | (`Ret(_) as r)::_::sl | (`Ret_void as r)::_::sl -> aux (r::sl)
    | (`Goto(`Label(l)))::((`Label(l'))::sl) when l = l' && is_unique l' = 1 -> aux sl
    | (`Goto(`Label(l)))::((`Label(l'))::sl) when l = l' -> (`Label(l')) :: aux sl
    | (`Goto(_ as l))::((`Goto(_))::sl) -> (`Goto(l)) :: sl |> aux
    | (`Label(l))::sl when is_unique l = 0 -> aux sl
    | s::sl -> s :: aux sl in
  aux stmt_list

let simplify_func : func -> func = fun func -> 
  { func 
    with func_body = func.func_body
                    |> simplify_func_body
                    |> goto_peephole
                    (*|> simple_jump_peephole*) }
                    (* buggy!! consider if (..) {..} else {} *)


let pre_optimize : func -> func = 
  simplify_func <-- Pre.PRE.run <-- simplify_func


;;



type instrs = M.stmt array 

let transform_stmt_cp (to_var : Temp.t -> immediate) : stmt -> stmt * bool = 
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
      | `Temp(t) -> (transform_immediate (`Temp(t)) :> rvalue)
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

let transform_stmt_cop (to_var : Temp.t -> immediate) : stmt -> stmt * bool = 
  let flag = ref false in

  let rec transform_stmt : stmt -> stmt = 
    function 
      | `Assign(var, rvalue) -> 
        `Assign(transform_var var, transform_rvalue rvalue)
      | `Static_invoke(msig, il) -> 
        `Static_invoke(msig, List.map transform_immediate il) 
      | `Ret(i) -> `Ret(transform_immediate i)
      | _ as s -> s 
  and transform_var = 
    function 
      | `Array_ref(i, i') -> `Array_ref(transform_immediate i, transform_immediate i') 
      | `Instance_field_ref(i, fsig) -> `Instance_field_ref(transform_immediate i, fsig)
      | _ as var -> var
  and transform_rvalue  = 
    function 
      | `Temp(t) -> (transform_immediate (`Temp(t)) :> rvalue)
      | `Expr(expr) -> `Expr(transform_expr expr) 
      | `Array_ref(i, i') -> `Array_ref(transform_immediate i, transform_immediate i')
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
            | `Temp(t') when not (t = t') -> flag := true; `Temp(t') 
            | _ -> `Temp(t)
        end
      | _ as c -> c in 

    fun stmt -> transform_stmt stmt, !flag


let constant_propagation : M.func -> M.func * bool = fun func -> 
  let cp_dfa = Dfa.Cp.constant_propagation func in
  let pred, succ = Dfa.calculate_pred_succ cp_dfa.instrs in
  let cp_res = Dfa.do_dfa cp_dfa pred succ |> fst in 
  let to_var i = fun t -> 
    match FiniteMap.find cp_res.(i) t with 
      | Dfa.Cp.Const(c) -> 
        `Const(c) 
      | _ -> `Temp(t) in
  let res_func = ref [] in 
  let flag = ref false in
  Array.iteri 
  (fun i stmt -> 
  match transform_stmt_cp (to_var i) stmt with 
    (stmt, b) -> flag := !flag || b; res_func := stmt :: !res_func) cp_dfa.instrs;
  {func with func_body = List.rev !res_func}, !flag

let copy_propagation : M.func -> M.func * bool = fun func -> 
  let cop_dfa = Dfa.Cop.copy_propagation func in 
  let pred, succ = Dfa.calculate_pred_succ cop_dfa.instrs in 
  let cop_res = Dfa.do_dfa cop_dfa pred succ |> snd in 
  let to_var i = fun t -> 
    match FiniteMap.find cop_res.(i) t with 
      | `Copy(t') -> `Temp(t') 
      | _ -> `Temp(t) in 
  let res_func = ref [] in 
  let flag = ref false in 
  Array.iteri 
  (fun i stmt -> 
  match transform_stmt_cop (to_var i) stmt with 
    (stmt, b) -> flag := !flag || b; res_func := stmt :: !res_func) cop_dfa.instrs;
  {func with func_body = List.rev ! res_func}, !flag


let simplify_func2 : M.func -> M.func = 
  fun func -> fst (constant_propagation func)

let simplify_func3 : M.func -> M.func = 
  fun func -> fst (copy_propagation func)


let is_effectless : M.rvalue -> bool = function 
  | `Temp(_) 
  | `Const(_) 
  | `Array_ref(_) 
  | `Expr(`Bin(_)) 
  | `Expr(`Rel(_)) -> true 
  | _ -> false
  

let dead_code_elimination : M.func -> M.func * bool = fun func -> 
  let lv_dfa = Dfa.Lv.live_vars func in 
  let pred, succ = Dfa.calculate_pred_succ lv_dfa.instrs in 
  let lv_res = Dfa.do_dfa lv_dfa pred succ |> snd in 
  let instrs = 
    func.func_body 
    |> Array.of_list in 
  let flag = ref false in
  let () = Array.iteri 
    (fun i -> function 
    | `Assign(`Temp(t), rvalue) 
      when not (Bvset.mem t lv_res.(i)) && is_effectless rvalue -> 
      flag := true; instrs.(i) <- `Nop 
    | _ -> ()) instrs in 
  { func with 
    func_body = Array.to_list instrs }, !flag

let compact : M.func -> M.func = fun func -> 
  { func with 
    func_body = List.fold_right 
    (fun stmt acc -> 
    match stmt with 
      | `Nop -> acc 
      | _ as s -> s :: acc) func.func_body []}

(*
let rec optimize : M.func -> M.func = fun func -> 
  let (func, flag1) = constant_propagation func in 
  let (func, flag2) = copy_propagation func in 
  let (func, flag3) = dead_code_elimination func in 
  if flag1 || flag2 || flag3 then optimize func 
  else compact func

*)

(*  ALERT : cp and cop have wrong semantics. 
 * 'cause the transfer functions only gen don't kill ! *)

let rec optimize : M.func -> M.func = 
  let aux transfer (func, flag) = 
    let (func', flag') = transfer func in 
      (func', flag || flag') in 
  fun func -> 
    let (func, flag) = 
      aux constant_propagation (func, false) 
      |> aux copy_propagation   
      |> aux dead_code_elimination in 
    (if flag then optimize <-- compact
    else compact) func