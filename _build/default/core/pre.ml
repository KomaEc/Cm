
module M = Mimple
module T = Temp
module U = Cm_util.Util
open U

module DfaExpr = Dfa.DfaExpr


module type S = 
sig
  type expr_set_at_pp

  type expr = [
      `Bin of M.immediate * M.binop * M.immediate
    | `Rel of M.immediate * M.relop * M.immediate
  ]

  val occur : expr -> expr_set_at_pp -> bool

  val collapse : expr_set_at_pp -> expr list

  val union : expr_set_at_pp -> expr_set_at_pp -> expr_set_at_pp

  val inter : expr_set_at_pp -> expr_set_at_pp -> expr_set_at_pp

  val diff : expr_set_at_pp -> expr_set_at_pp -> expr_set_at_pp

  val equal : expr_set_at_pp -> expr_set_at_pp -> bool

  val e_kill : M.stmt -> expr_set_at_pp -> expr_set_at_pp 

  val e_gen : M.stmt -> expr_set_at_pp -> expr_set_at_pp

  val gen_all_and_empty : M.func -> expr_set_at_pp * expr_set_at_pp

  val string_of_result : expr_set_at_pp -> string
end

let augment (proc : Procdesc.t) : Procdesc.t = 
  let module Set = Set.Make(struct type t = Procdesc.Node.t let compare node1 node2 = compare (Procdesc.Node.get_id node1) (Procdesc.Node.get_id node2) end) in
  let new_nodes_ref = ref Set.empty in
  Procdesc.iter 
    (fun node -> 
      if Procdesc.Node.is_internal node then 
        begin 
          let node_set =
            Procdesc.Node.fold_succ
              (fun acc node' -> if List.length (Procdesc.Node.get_preds node') >= 2 then Set.add node' acc else acc
                ) Set.empty node in 
          Procdesc.Node.iter_succ
            (fun node' -> 
              begin
                match Set.find_opt node' node_set with 
                  | None -> () 
                  | Some _ -> 
                    let node'' = Procdesc.Node.make [||] (-10000) proc.pname in 
                    node.succ <- List.filter (fun node''' -> (Procdesc.Node.get_id node''') <> (Procdesc.Node.get_id node')) node.succ;
                    node'.pred <- List.filter (fun node''' -> (Procdesc.Node.get_id node''') <> (Procdesc.Node.get_id node)) node'.pred;
                    node.succ <- node'' :: node.succ;
                    node'.pred <- node'' :: node'.pred;
                    node''.succ <- [node'];
                    node''.pred <- [node];
                    let id', id'' = Procdesc.Node.get_id node', Procdesc.Node.get_id node'' in
                    let length = Procdesc.Node.get_instrs node |> Array.length in
                    begin
                      match (Procdesc.Node.get_instrs node).(length-1) with 
                        | `If(x, `Line_num(i)) when i = id' -> (Procdesc.Node.get_instrs node).(length-1) <- `If(x, `Line_num(id''))
                        | `Goto(`Line_num(i)) when i = id' -> (Procdesc.Node.get_instrs node).(length-1) <- `Goto(`Line_num(id''))
                        | _ -> ()
                    end;
                    new_nodes_ref := Set.add node'' !new_nodes_ref
              end) node
        end) proc; 
        let new_ones = Set.elements !new_nodes_ref in
        {
          proc with 
          nodes = proc.nodes @ new_ones;
          node_num =  proc.node_num + List.length new_ones;
        }


let add_one_instr : M.stmt -> Procdesc.Node.t -> Procdesc.Node.t = 
  fun stmt node -> 
    {
      node with 
        instrs = 
          let res = Array.make (Array.length node.instrs + 1) `Nop in 
          res.(0) <- stmt;
          Array.blit node.instrs 0 res 1 (Array.length node.instrs);
          res  
    }

module Transform_pre_make 
  (X : sig 
        val rvalue : M.rvalue -> M.rvalue
        end) 
  : Mimple.MIMPLE_VISITOR = 
struct 
  class visitor = 
  object ((o : 'self_type))
  inherit Mimple.Transform.visitor 
  method! rvalue : M.rvalue -> (M.rvalue * 'self_type) = 
    fun rv -> X.rvalue rv, o
  end
end

module Make 
  (X : S) = 
struct 
  type expr_set_at_pp = X.expr_set_at_pp

  type proc_info = {
    proc : Procdesc.t;
    all : expr_set_at_pp;
    empty : expr_set_at_pp;
  }

  let remove_newline : string -> string = 
    fun str -> 
      String.sub str 0 (String.length str - 1)

  let string_of_result : 
    bool -> expr_set_at_pp Dataflow.result -> proc_info -> string = fun b res dfa ->
    Procdesc.fold 
    (fun acc node -> 
      if not (Procdesc.Node.is_internal node) then acc else
      if b then 
        begin
          acc
          ^ X.string_of_result (Hashtbl.find res (Procdesc.Node.get_id node))
          ^ "\n"
          ^ Procdesc.Node.string_of_node node
        end
      else 
        begin
          acc
          ^ (remove_newline (Procdesc.Node.string_of_node node))
          ^ X.string_of_result (Hashtbl.find res (Procdesc.Node.get_id node))
          ^"\n\n"
        end)
    "" dfa.proc   

  let anticipated_transfer : M.stmt -> expr_set_at_pp -> expr_set_at_pp = 
    fun s -> (X.e_gen s) <-- (X.e_kill s)

  let lift : [`Is_Backward | `Is_Forward ] -> (M.stmt -> expr_set_at_pp -> expr_set_at_pp) -> Procdesc.Node.t -> expr_set_at_pp -> expr_set_at_pp = 
    fun dir transfer node ->
      let instrs = Procdesc.Node.get_instrs node in 
      if dir = `Is_Forward then
        Array.fold_left
        (fun trs stmt -> 
        transfer stmt <-- trs) (fun x -> x) instrs
      else 
        Array.fold_right
        (fun stmt trs -> 
        transfer stmt <-- trs) instrs (fun x -> x) 


  let kill_trans : Procdesc.Node.t -> expr_set_at_pp -> expr_set_at_pp = 
    lift `Is_Forward X.e_kill

  let gen_trans : Procdesc.Node.t -> expr_set_at_pp -> expr_set_at_pp = 
    lift `Is_Forward X.e_gen


  (* Step 1, backward analysis to ge anticipated expressions *)
  let step1_transfer : Procdesc.Node.t -> expr_set_at_pp -> expr_set_at_pp = 
    lift `Is_Backward anticipated_transfer


  let make_step2_transfer : expr_set_at_pp Dataflow.result -> Procdesc.Node.t -> expr_set_at_pp -> expr_set_at_pp = 
    fun anti_in node value -> 
      X.union (Hashtbl.find anti_in (Procdesc.Node.get_id node)) value |> (lift `Is_Forward X.e_kill) node

  let make_step3_transfer : expr_set_at_pp Dataflow.result -> Procdesc.Node.t -> expr_set_at_pp -> expr_set_at_pp = 
    fun earliest node value -> 
      X.union (Hashtbl.find earliest (Procdesc.Node.get_id node)) value |> (lift `Is_Forward X.e_gen) node

  let make_step4_transfer : expr_set_at_pp Dataflow.result -> Procdesc.Node.t -> expr_set_at_pp -> expr_set_at_pp = 
    fun latest node value -> 
      X.diff (X.union ((lift `Is_Backward X.e_gen) node value) value) (Hashtbl.find latest (Procdesc.Node.get_id node))

  let func_to_proc_info : M.func -> proc_info = 
    fun func -> 
      let proc = Procdesc.from_func_singleton func
      and (all, empty) = X.gen_all_and_empty func in
      {
        proc;
        all;
        empty;
      }

  type transfer = Procdesc.Node.t -> expr_set_at_pp -> expr_set_at_pp

  let config_dfa : proc_info -> [`Is_Backward | `Is_Forward ] -> [`May | `Must] -> transfer -> expr_set_at_pp Dataflow.t = 
    fun proc_info dir may_must transfer ->
      {
        proc = proc_info.proc;
        dir = (match dir with `Is_Backward -> Dataflow.D_Backward | `Is_Forward -> Dataflow.D_Forward);
        meet = (match may_must with `May -> X.union | `Must -> X.inter);
        equal = X.equal;
        transfer;
        entry_or_exit_facts = proc_info.empty;
        bottom = (match may_must with `May -> proc_info.empty | `Must -> proc_info.all);
      }

  let do_dfa ?verbose ?string_of_result (dfa : 'a Dataflow.t) : 'a Dataflow.result * 'a Dataflow.result = 
    let (res1, res2) = Dataflow.do_dfa ?verbose ?string_of_result dfa in 
      match dfa.dir with 
        | D_Forward -> (res2, res1)
        | D_Backward -> (res1, res2)
  (* result : result of in and out *)


  let run (func : M.func) : M.func = 
    let proc_info = func_to_proc_info func in 

    let new_proc = augment proc_info.proc in

    let proc_info = {
      proc_info with proc = new_proc;
    } in
(*
    let kill = Hashtbl.create 16 in 

    let () = Procdesc.iter (fun node -> Hashtbl.add use (Procdesc.Node.get_id node) (gen_trans node proc_info.empty)) proc_info.proc in
*)
    let e_use = Hashtbl.create 16 in

    let () = Procdesc.iter (fun node -> Hashtbl.add e_use (Procdesc.Node.get_id node) (gen_trans node proc_info.empty)) proc_info.proc in

    (*let e_use_string = string_of_result true e_use proc_info in

    let () = print_endline ("******************************e_use**************************\n\n" ^ e_use_string ^ "\n***************************end**************************\n") in*)

    let dfa_val_node_all = Hashtbl.create 16 in

    let () = 
      Procdesc.iter 
        (fun node -> Hashtbl.add dfa_val_node_all (Procdesc.Node.get_id node) proc_info.all) proc_info.proc in

    let node_wise_op : (expr_set_at_pp -> expr_set_at_pp -> expr_set_at_pp) -> expr_set_at_pp Dataflow.result -> expr_set_at_pp Dataflow.result -> expr_set_at_pp Dataflow.result = 
    begin
    fun (<+>) res1 res2 -> 
      let newres = Hashtbl.create 16 in
      Procdesc.iter 
        (fun node -> 
          let id = Procdesc.Node.get_id node in
          let val1, val2 = Hashtbl.find res1 id, Hashtbl.find res2 id in 
          Hashtbl.add newres id (val1 <+> val2)) proc_info.proc ; newres end in

    let ( || ) = node_wise_op X.union in 
    let ( && ) = node_wise_op X.inter in 
    let ( - ) = node_wise_op X.diff in  
    (*
    let ( ! ) : expr_set_at_pp Dataflow.result -> expr_set_at_pp Dataflow.result = 
      begin
      fun res -> 
      let new_res = Hashtbl.create 16 in
        Procdesc.iter 
          (fun node -> 
            let id = Procdesc.Node.get_id node in
            let val1 = Hashtbl.find res id in 
            Hashtbl.add new_res id (X.diff proc_info.all val1)) proc_info.proc; new_res 
      end in *)

    let ( ! ) : expr_set_at_pp Dataflow.result -> expr_set_at_pp Dataflow.result = 
      (-) dfa_val_node_all in
    


    let step1_dfa = config_dfa proc_info `Is_Backward `Must step1_transfer in 
    let (anti_in, _) = do_dfa step1_dfa in 

    (*let anti_in_string = string_of_result true anti_in proc_info in

    let () = print_endline ("*****************************anti_in************************\n\n" ^ anti_in_string ^ "\n***************************end***************************\n") in*)


    let step2_transfer = make_step2_transfer anti_in in 
    let step2_dfa = config_dfa proc_info `Is_Forward `Must step2_transfer in
    let (aval_in, _) = do_dfa step2_dfa in 


    (*let aval_in_string = string_of_result true aval_in proc_info in 

    let () = print_endline ("*****************************avail_in************************\n\n" ^ aval_in_string ^ "\n***************************end***************************\n") in*)


    let earliest = anti_in - aval_in in


    (*let earliest_string = string_of_result true earliest proc_info in

    let () = print_endline ("*****************************earliest************************\n\n" ^ earliest_string ^ "\n***************************end***************************\n") in*)


    let post_tranfer node value = 
      X.diff (X.union value (Hashtbl.find earliest (Procdesc.Node.get_id node))) (Hashtbl.find e_use (Procdesc.Node.get_id node))
    in

    let _ = make_step3_transfer earliest in
    let step3_transfer = post_tranfer in
    let step3_dfa = config_dfa proc_info `Is_Forward `Must step3_transfer in 
    let (postponable_in, _) = do_dfa step3_dfa in

    (*let postponable_in_string = string_of_result true postponable_in proc_info in 

    let () = print_endline ("*****************************postponable_in************************\n\n" ^ postponable_in_string ^ "\n***************************end***************************\n") in

    let postponable_out_string = string_of_result false postponable_out proc_info in 

    let () = print_endline ("*****************************postponable_out************************\n\n" ^ postponable_out_string ^ "\n***************************end***************************\n") in*)
    

    let fold_succ_inter_inner_union res1 res2 = 
      begin
        let new_res = Hashtbl.create 16 in 
        Procdesc.iter
          (fun node -> 
            let id = Procdesc.Node.get_id node in 
            let new_val = Procdesc.Node.fold_succ
              (fun acc node' -> 
                let val_e = Hashtbl.find res1 (Procdesc.Node.get_id node') in 
                let val_p = Hashtbl.find res2 (Procdesc.Node.get_id node') in
                X.inter acc (X.union val_e val_p)) proc_info.all node in 
            Hashtbl.add new_res id new_val) proc_info.proc;
        new_res
      end in  
    (*
    let some_val : expr_set_at_pp Dataflow.result =
      begin
        let new_res = Hashtbl.create 16 in 
        Procdesc.iter
          (fun node -> 
            let id = Procdesc.Node.get_id node in 
            let new_val = Procdesc.Node.fold_succ
              (fun acc node' -> 
                let val_e = Hashtbl.find earliest (Procdesc.Node.get_id node') in 
                let val_p = Hashtbl.find postponable_in (Procdesc.Node.get_id node') in
                X.inter acc (X.union val_e val_p)) proc_info.all node in 
            Hashtbl.add new_res id new_val) proc_info.proc;
        new_res 
      end in
      *)
    let (<&>) = fold_succ_inter_inner_union in
    let latest = 
      (earliest || postponable_in) 
      && (e_use || !(earliest <&> postponable_in)) in

    (*let latest_string = string_of_result true latest proc_info in 

    let () = print_endline ("*****************************latest************************\n\n" ^ latest_string ^ "\n***************************end***************************\n") in*)


    let use_transfer node value = 
      X.diff (X.union value (Hashtbl.find e_use (Procdesc.Node.get_id node))) (Hashtbl.find latest (Procdesc.Node.get_id node))
    in
    
    let _ = make_step4_transfer latest in
    let step4_transfer = use_transfer in
    let step4_dfa = config_dfa proc_info `Is_Backward `May step4_transfer in

    let (_, use_out) = do_dfa (*~verbose:() ~string_of_result:X.string_of_result*) step4_dfa in


    (*let use_out_string = string_of_result false use_out proc_info in 

    let use_in_string = string_of_result true use_in proc_info in

    let () = print_endline ("*****************************use_out************************\n\n" ^ use_out_string ^ "\n***************************end***************************\n") in

    let () = print_endline ("*****************************use_in************************\n\n" ^ use_in_string ^ "\n***************************end***************************\n") in*)



    let all_exprs = X.collapse proc_info.all in 
    let temp2exp = Hashtbl.create 16
    and exp2temp = Hashtbl.create 16 in 
    let () = 
      List.iter 
        (fun expr ->
          let t = T.newtemp () in 
            Hashtbl.add temp2exp t expr;
            Hashtbl.add exp2temp expr t) all_exprs in

    let transform_base : M.rvalue -> M.rvalue = function 
      | `Expr(`Bin(_, _, _ as cont)) as e -> 
        begin
          try let t = Hashtbl.find exp2temp (`Bin(cont)) in 
            `Temp(t) with
          Not_found -> e
        end
      | `Expr(`Rel(_, _, _ as cont)) as e -> 
        begin 
          try let t = Hashtbl.find exp2temp (`Rel(cont)) in 
            `Temp(t) with 
          Not_found -> e
        end
      | _ as e -> e in

    let subst = 
      e_use && (!latest || use_out) in 
    
    let () = 
      Procdesc.iter 
        (fun node -> 
          let id = Procdesc.Node.get_id node in 
          let subst' = Hashtbl.find subst id in
          let rvalue : M.rvalue -> M.rvalue = function 
            | `Expr(`Bin(_, _, _ as cont)) as e ->  
              begin 
                match X.occur (`Bin(cont)) subst' with 
                  | true -> transform_base e
                  | false -> e
              end
            | `Expr(`Rel(_, _, _ as cont)) as e ->  
              begin 
                match X.occur (`Rel(cont)) subst' with 
                  | true -> transform_base e
                  | false -> e
              end
            | _ as e -> e in
          let module Transform = Transform_pre_make(struct let rvalue = rvalue end) in 
          let visitor = new Transform.visitor in
          Array.iteri 
            (fun i stmt -> 
              let (stmt', _) = visitor#stmt stmt in 
              node.instrs.(i) <- stmt') node.instrs) proc_info.proc in
    
    let add = 
      latest && use_out in 

    let new_nodes = 
      Procdesc.fold 
        (fun nodes node -> 
          let id = Procdesc.Node.get_id node in 
          let add' = Hashtbl.find add id in 
          (List.fold_left 
            (fun node expr -> 
              if X.occur expr add' then 
                let t = Hashtbl.find exp2temp expr in 
                let stmt = `Assign(`Temp(t), `Expr(expr :> M.expr)) in 
                add_one_instr stmt node 
              else node) node all_exprs) :: nodes) [] proc_info.proc in

    proc_info.proc.nodes <- (List.rev new_nodes);
    {
      func with func_body = proc_info.proc |> Procdesc.recover
    }
  

end


module PRE = Make(Dfa.DfaExpr)

(*
module Test = 
struct 
  let from_prog (p : M.prog) = 
    List.iter 
      (fun func -> 
        (*Procdesc.from_func func 
        |> augment *)
        func
        |> PRE.run
        |> Procdesc.string_of_proc
        |> print_string) p
end
*)