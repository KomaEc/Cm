
(*
open Mimple
open Types
module S = Symbol
let partition_into_blocks : stmt list -> stmt list list = 
  fun stmt_list ->

  let stmt_array = Array.of_list stmt_list in 

  let stmt_num = Array.length stmt_array in

  let is_leader = Array.make stmt_num false in 

  let find_useful_labels = 
    List.fold_left
      (fun prev stmt -> 
        match stmt with 
          | `If(_, l) -> S.enter l () prev 
          | `Goto(l) -> S.enter l () prev 
          | _ -> prev) S.empty stmt_list in 

  let find_leader : unit -> unit = fun () -> 
    let is_contained : S.t -> unit S.table -> bool = 
      fun s tbl -> 
      begin
        try let () = S.lookup s tbl in true with 
        _ -> false 
      end in
    is_leader.(0) <- true;
    Array.iteri
    (fun i stmt -> 
    begin
      match stmt with 
      | `If(_) -> if i < stmt_num - 1 then is_leader.(i+1) <- true 
      | `Goto(_) -> if i < stmt_num - 1 then is_leader.(i+1) <- true 
      | `Label(l) ->
        if is_contained l find_useful_labels then 
        is_leader.(i) <- true 
      | _ -> ()
    end) stmt_array in 

  let () = find_leader () in 

  let block : stmt list ref = ref [] in 

  let res : stmt list list ref = ref [] in 

  let emit_stmt : stmt -> unit = 
    fun s -> block := s :: !block in 

  let end_block () = 
    res := List.rev !block :: !res;
    block := [] in 

  let end_all () = 
    res := List.rev !res in 

  Array.iteri 
    (fun i stmt -> 
      if is_leader.(i) then
        begin
          end_block();
          emit_stmt stmt
        end
      else emit_stmt stmt) stmt_array;
      end_all ();
  match !res with 
    | [] -> failwith "impossible"
    | _ :: l -> l


let print_blocks : stmt list list -> unit = 
  fun block_list -> 
    List.iter
      (fun stmt_list -> 
        print_endline "Start Block: ";
        List.iter (fun stmt -> string_of_stmt stmt |> print_string) stmt_list;
        print_endline "End Block") block_list

let string_of_block : stmt list -> string = fun stmt_list ->
  "\nBegin Block:\n"
  ^ (List.fold_left (fun acc stmt -> acc ^ string_of_stmt stmt) "" stmt_list)
  ^ "End Block\n"


let string_of_block : stmt list -> string = fun stmt_list ->
  let block_list = partition_into_blocks stmt_list in 
  List.fold_left 
    (fun prev block_list -> 
      prev ^ string_of_block block_list) "" block_list


let string_of_good_func : func -> string = 
  fun { func_name; func_args; func_ret; local_decls; func_body } -> 
  "\nBeginFunc " ^ Symbol.name func_name 
  ^ " : " ^ string_of_ty_list func_args ^ " -> " 
  ^ string_of_ty func_ret ^ "\n"
  ^ (List.fold_left (fun acc decl -> acc ^ string_of_decl decl) "" local_decls)    
  ^ string_of_block func_body
  ^ "EndFunc\n"

let string_of_good_prog : prog -> string = 
  List.fold_left 
    (fun prev method_chunk -> 
      prev ^ string_of_good_func method_chunk) ""


let print_good_prog : prog -> unit = 
  fun prog -> string_of_good_prog prog |> print_endline
  

  *)