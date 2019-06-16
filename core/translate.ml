
(* open Mimple *)
open Types
module M = Mimple
module T = Transform
module U = Cm_util.Util
open U

(* TODO : add global var reference, when doing semantics check, 
 * the occurence of glb vars are recorded here 
 * Treated as a static field ! *)

let object_id = Symbol.symbol "Object"

let rec type_convert : Ast.ty -> ty = 
  function 
    | Int -> Primitive(`Int) 
    | Bool -> Primitive(`Bool)
    | Void -> Primitive(`Void)
    | NameTy(ty_id) when ty_id = object_id -> Object(`Object)
    | NameTy(ty_id) -> Object(`ClassTy(ty_id))
    | ArrayTy(ty) -> 
      let ty' = type_convert ty in Object(`ArrayTy(ty'))
    | _ -> raise (Invalid_argument "primitive_type_convert")

let glb_static_vars_tbl_ref : (Symbol.t * (ty * [ `Const of M.const ] option ref)) list ref = 
  ref []

let add_glb_vars : Symbol.t -> Ast.ty -> unit = 
  fun name ty ->
    let ty  = type_convert ty in
    glb_static_vars_tbl_ref := (name, (ty, ref None)) :: !glb_static_vars_tbl_ref

let assign_glb_vars : Symbol.t -> [ `Const of M.const ] -> unit = 
  fun id c -> 
    let (_, init_ref) = List.assoc id !glb_static_vars_tbl_ref in 
    init_ref := Some(c)

let glb_class_sig_tbl_ref : (Symbol.t * ty) list Symbol.table ref
  = ref Symbol.empty

(* TODO: get glb_class_sig_tbl *)

let cur_identities : M.identity list ref = 
  ref [] 

let emit_identity : M.identity -> unit = 
  fun idt -> cur_identities := idt :: !cur_identities


let cur_local_def : (Temp.t * ty) list ref = 
  ref [] 

let emit_local_def : Temp.t -> Ast.ty -> unit = 
  fun t ty -> 
    let ty = type_convert ty in
    cur_local_def := (t, ty) :: !cur_local_def


let cur_func_body : M.stmt list ref = 
  ref []

let emit_stmt : M.stmt -> unit = 
  fun s -> cur_func_body := s :: !cur_func_body

let cur_func_ty : (Symbol.t * ty list * ty) option ref = ref None 

let begin_function name ty_list ty = 
  let ty_list' = List.map type_convert ty_list in 
  let ty' = type_convert ty in 
  cur_func_ty := Some(name, ty_list', ty')



let prog_frag : M.func list ref = ref []


(* TODO : modify here *)
let end_function () = 
  let decl_list = 
    List.rev !cur_local_def 
    |> List.map (fun (t, ty) -> `Temp_decl(`Temp(t), ty))
    in 
  let stmt_list = List.rev !cur_func_body in
  let identities = !cur_identities in
  let () = cur_identities := [] in
  let () = cur_local_def := [] in 
  let () = cur_func_body := [] in
  let func : M.func = 
    begin
      match !cur_func_ty with 
        | Some(name, ty_list, ty) -> 
          { func_name = name; 
            func_args = ty_list; 
            func_ret = ty;
            identities = identities;
            local_decls = decl_list; 
            func_body = stmt_list; }
        | _ -> assert false
        
    end in
  prog_frag := func :: !prog_frag

let get_mimple_ori () = 
  List.rev !prog_frag 


let get_mimple () = 
  List.rev !prog_frag |> List.map T.simplify_func

let get_mimple_after_pre () = 
  List.rev !prog_frag |> List.map T.pre_optimize

let get_mimple1 () = 
  List.rev !prog_frag |> List.map (M.convert_to_lnum <-- T.simplify_func)

let get_mimple2 () = 
  List.rev !prog_frag |> List.map T.simplify_func2

let get_mimple3 () = 
  List.rev !prog_frag |> List.map T.simplify_func3

let optimize () = 
  List.rev !prog_frag |> List.map (T.simplify_func <-- T.optimize <-- T.simplify_func)

