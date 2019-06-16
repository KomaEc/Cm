
module P = Cm_util.Printing
module M = Mimple
open P
module U = Cm_util.Util
open U

module Node = struct 

  type id = int 

  let cnt = ref 0

  let reset () = cnt := 0
  
  type t = {
    id: id;
    mutable instrs: M.stmt array;
    loc: int; (* Start index in the original procedure *)
    pname: Symbol.t;
    mutable pred: t list;
    mutable succ: t list;
  } 

  let make_exit (pname : Symbol.t) : t =
  {
    id = -2;
    instrs = [||];
    loc = -2;
    pname;
    pred = [];
    succ = [];
  }

  let make_entry (pname : Symbol.t) : t =
  {
    id = -1;
    instrs = [||];
    loc = -1;
    pname;
    pred = [];
    succ = [];
  }

  let is_exit : t -> bool = fun node -> node.id = -2
  
  let is_entry : t -> bool = fun node -> node.id = -1

  let is_internal : t -> bool = fun node -> not (is_entry node) && not (is_exit node)

  let is_internal_id : id -> bool = fun id -> id <> -2 && id <> -1


  let compare node1 node2 = Pervasives.compare node1.id node2.id 

  let hash node = Hashtbl.hash node.id 

  let equal node1 node2 = node1.id = node2.id 

  let get_id node = node.id 

  let get_succs node = node.succ 

  let get_preds node = node.pred 

  let get_instrs node = node.instrs
  
  let get_loc node = node.loc 

  let fold_succ : ('acc -> t -> 'acc) -> 'acc -> t -> 'acc = 
    fun f acc node -> 
      List.fold_left f acc node.succ

  let fold_pred : ('acc -> t -> 'acc) -> 'acc -> t -> 'acc = 
    fun f acc node -> 
      List.fold_left f acc node.pred

  let iter_succ : (t -> unit) -> t -> unit = 
    fun f node -> List.iter f node.succ

  let iter_pred : (t -> unit) -> t -> unit = 
    fun f node -> List.iter f node.pred

  (* very bad design !!!! *)
  let make instrs loc pname =
    incr cnt;
    {
      id = !cnt;
      instrs;
      loc;
      pname;
      pred = [];
      succ = []
    }

  let create id instrs loc pname = 
    {
      id;
      instrs;
      loc;
      pname;
      pred = [];
      succ = []
    }
(*
  let make id instrs loc pname = 
  {
    id;
    instrs;
    loc;
    pname;
    pred = [];
    succ = [];
  } *)

  let string_of_node : t -> string = fun node -> 
    let res = ref ("=====Start node " ^ string_of_int node.id ^ "=====\n") in
    res := !res ^ "preds : " ^ (string_of_list string_of_int (List.map get_id node.pred)) ^ "\n";
    Array.iteri 
    (fun i stmt -> 
    res := !res ^ "line " ^ string_of_int (i + node.loc) ^ 
    M.string_of_stmt stmt ^ "\n") node.instrs; 
    res := !res ^ "succs : " ^ (string_of_list string_of_int (List.map get_id node.succ));
    res := !res ^ "\n======End node " ^ string_of_int node.id ^ "=====\n\n";
    !res

end 


type t = 
{
  pname: Symbol.t;
  locals: (Temp.t * Types.ty) list;
  formals: (Temp.t * Types.ty) list;
  mutable nodes: Node.t list;
  mutable node_num: int;
  mutable start_node: Node.t;
  mutable exit_node: Node.t;
}


let iter : (Node.t -> unit) -> t -> unit = 
  fun f proc ->
    List.iter f proc.nodes

let fold : ('acc -> Node.t -> 'acc) -> 'acc -> t -> 'acc = 
  fun f acc proc -> 
    List.fold_left f acc proc.nodes



module type PROC_FOLDER = 
sig
  class ['acc] visitor :
  object('self)

    method proc : (Node.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc * 'self
    method node : (Node.t -> 'acc -> 'acc) -> Node.t -> 'acc -> 'acc * 'self
  end
end

module Fold_preorder : PROC_FOLDER = 
struct 
  module IntSet = Set.Make(struct type t = int let compare = compare end)
  let mem node set = let id = Node.get_id node in IntSet.mem id set
  class ['acc] visitor = 
  object((o : 'self))

    val checked : IntSet.t = IntSet.empty
    method node : (Node.t -> 'acc -> 'acc) -> Node.t -> 'acc -> 'acc * 'self = 
      fun f node acc -> 
        let id = Node.get_id node in
        let acc' = f node acc 
        and checked' = IntSet.add id checked in
        Node.fold_succ
          (fun ((acc''', o'') as prev) node' -> if mem node' checked then prev else o''#node f node' acc''')
            (acc', {<checked = checked'>}) node
    method proc : (Node.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc * 'self = 
      fun f proc acc -> 
        o#node f proc.start_node acc

  end
end

module Fold_bfs : PROC_FOLDER =
struct
  module IntSet = Set.Make(struct type t = int let compare = compare end)
  let mem node set = let id = Node.get_id node in IntSet.mem id set
  let add node set = let id = Node.get_id node in IntSet.add id set
  class ['acc] visitor = 
  object((o : 'self))
    val checked : IntSet.t = IntSet.empty
    val queue : Node.t Queue.t = Queue.create ()
    method node : (Node.t -> 'acc -> 'acc) -> Node.t -> 'acc -> 'acc * 'self = 
      fun f node acc -> 
        (*let () = print_int (Node.get_id node); print_newline () in*)
        let acc' = f node acc in
        let checked' = Node.fold_succ (fun checked' node' -> if mem node' checked' |> not then (Queue.add node' queue; add node' checked') else checked') (add node checked) node in
        let o' = {<checked = checked'>} in
          if Queue.is_empty queue then acc', o'
          else 
            let node' = Queue.pop queue in
              o'#node f node' acc'
    method proc : (Node.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc * 'self = 
      fun f proc acc -> 
        o#node f proc.start_node acc

  end
end

(* Need a false branch first !!! *)

let fold_preorder f proc acc = 
  let visitor = new Fold_preorder.visitor in
  visitor#proc f proc acc
  |> fst

let fold_bfs f proc acc = 
  let visitor = new Fold_bfs.visitor in 
  visitor#proc f proc acc
  |> fst

let iter_preorder f proc = 
  fold_preorder (fun node () -> f node) proc ()

let iter_bfs f proc =
  fold_bfs (fun node () -> f node) proc () 

let layout (proc : t) = 
  fold_preorder
    (fun node acc -> if Node.is_internal node then (Node.get_id node) :: acc else acc) 
      proc []
  |> List.rev

(*
 let test (procs : t list) = 
  let s = 
    List.fold_left
      (fun acc proc -> 
        acc ^ (P.string_of_list string_of_int (layout proc)) ^ "\n") "" procs in 
  print_endline s

*)

module BlockNum2LNum = 
struct 
  class visitor id2lnum = 
  object ((o : 'self_type))
  inherit M.Transform.visitor
  method! target : M.target -> (M.target * 'self_type) = function 
    | `Line_num(id) -> 
      let lnum = Hashtbl.find id2lnum id in 
      `Line_num(lnum), o
    | _ -> assert false
  end
end


let insert_goto (proc : t) = 
  iter 
    (fun node -> 
      if Node.is_internal node then 
        let length = Array.length node.instrs in 
        let ori_label = 
          begin
            if length > 0 then 
              match node.instrs.(length-1) with 
                | `If(_, `Line_num(n)) | `Goto(`Line_num(n)) -> Some n
                | _ -> None
            else None
          end in 
        let filter = 
          match ori_label with 
            | Some n -> (fun node -> let id = Node.get_id node in Node.is_internal node && id <> n)
            | _ -> Node.is_internal in 
        let legal_succ = List.filter filter (Node.get_succs node) in 
        match legal_succ with 
          | [] -> ()
          | [n] -> 
            let new_instrs = Array.make (length+1) `Nop in 
            let () = Array.blit node.instrs 0 new_instrs 0 length in 
            new_instrs.(length) <- (`Goto(`Line_num(Node.get_id n)));
            node.instrs <- new_instrs;
          | _ as l -> failwith ("too many succs!!" ^ " " ^ (List.length l |> string_of_int) ^ " with problems in node " ^ (node |> Node.get_id |> string_of_int))) proc

let insert_all_label : M.stmt list -> M.stmt list = fun stmt_list ->
  let module T (X : sig val convert : int -> M.label end) : Mimple.MIMPLE_VISITOR = 
    struct 
      class visitor = 
      object((o : 'self_type))
      inherit Mimple.Transform.visitor
      method! target : M.target -> M.target * 'self_type = function
        | `Line_num(i) -> `Label(X.convert i), o
        | _ -> assert false
      end
    end in
  let lnum_2_label = Array.make (List.length stmt_list) Temp.dummy in
  let i = ref 0 in
  let rec aux : M.stmt list -> M.stmt list = function
    | [] -> []
    | x :: xs -> 
      let l = Temp.newlabel () in 
      lnum_2_label.(!i) <- l;
      incr i;
      (`Label(l)) :: x :: (aux xs) in 

  let module T = T(struct let convert = fun i -> lnum_2_label.(i) end) in
  let visitor = new T.visitor in
  aux stmt_list |> List.map (fst <-- visitor#stmt)

let recover (proc : t) : M.stmt list = 
  let () = insert_goto proc in
  let order = layout proc 
  and id2node = Hashtbl.create 16 
  and id2lnum = Hashtbl.create 16 in 
  let () = iter (fun node -> Hashtbl.add id2node (Node.get_id node) node) proc
  and _ = List.fold_left (fun i idx -> Hashtbl.add id2lnum idx i; i+(Hashtbl.find id2node idx |> Node.get_instrs |> Array.length)) 0 order
  (*and () = List.iteri (fun i idx -> Hashtbl.add id2lnum idx i) order *)
  and visitor = new BlockNum2LNum.visitor(id2lnum) in
  List.map (Hashtbl.find id2node) order 
  |> List.fold_left (fun acc node -> 
                    Node.get_instrs node 
                    |> Array.to_list 
                    |> List.map (fst <-- visitor#stmt)
                    |> (@) acc) []
  |> insert_all_label


let test (stmt_list : M.stmt list) =
  List.fold_left
    (fun acc stmt -> acc ^ M.string_of_stmt stmt ^ "\n") "" stmt_list
  |> print_endline
(*
  List.iteri
    (fun i stmt -> 
      print_endline ("line " ^ (string_of_int i) ^ M.string_of_stmt stmt)) stmt_list *)





    

(*
let singleton_from_func (func : M.func) : t =
  let locals = M.get_locals func 
  and formals = M.get_formals func 
  and instrs = Array.of_list func.func_body in
  let convert_to_lnum () = 
    let tbl = Hashtbl.create 16 in 
    let offset = ref 0 in 
    Array.iteri 
    (fun i -> function 
    | `Label(l) when i > 0 -> 
      Hashtbl.add tbl l (i - !offset - 1);
      incr offset 
    | _ -> ()) instrs;
    let aux = Array.make (Array.length instrs - !offset - 1) `Nop in
    offset := 0;
    Array.iteri
    (fun i -> function 
    | _ when i = 0 -> () 
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
    aux in 
  let instrs = convert_to_lnum () in
  let length = Array.length instrs in
*)

let find_leader (instrs : M.stmt array) : bool array = 
  let length = Array.length instrs in
  let is_leader = Array.make (length + 1) false in
  is_leader.(length) <- true;
  Array.iteri 
  (fun i -> function 
    | _ when i = 0 -> is_leader.(i) <- true 
    | `Goto(`Line_num(j)) | `If(_, `Line_num(j)) -> 
      is_leader.(j) <- true;
      if i < length - 1 then is_leader.(i+1) <- true
    | _ -> ()) instrs;
  is_leader

let from_func (func: M.func) (find_leader : M.stmt array -> bool array) : t = 
  Node.reset ();
  let locals = M.get_locals func 
  and formals = M.get_formals func 
  and instrs = Array.of_list func.func_body in
  let convert_to_lnum () = 
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
    aux in 
  let () = 
    Array.fold_left
      (fun acc stmt -> acc ^ Mimple.string_of_stmt stmt ^ "\n")
        "" instrs 
    |> print_endline in
  let instrs = convert_to_lnum() in
  print_endline "hihihihih";
  let () = 
    Array.fold_left
      (fun acc stmt -> acc ^ Mimple.string_of_stmt stmt ^ "\n")
        "" instrs 
    |> print_endline in
  let length = Array.length instrs in
  (*
  let is_leader = Array.make (length + 1) false in
  is_leader.(length) <- true;
  Array.iteri 
  (fun i -> function 
    | _ when i = 0 -> is_leader.(i) <- true 
    | `Goto(`Line_num(j)) | `If(_, `Line_num(j)) -> 
      is_leader.(j) <- true;
      if i < length - 1 then is_leader.(i+1) <- true
    | _ -> ()) instrs;
    *)
  let is_leader = find_leader instrs in
  let nodes_ref = ref [] 
  and id = ref 0
  and l = ref 0
  and tbl : (int, int) Hashtbl.t = Hashtbl.create 16 (* maps starting instrs line number to node id*)
   in
  Hashtbl.add tbl 0 0 ;
  for r = 1 to length do 
    if is_leader.(r) then 
    let newinstrs = Array.make (r - !l) `Nop in 
    Array.blit instrs !l newinstrs 0 (r - !l);
    let node = Node.make (*!id *)newinstrs !l func.func_name in 
    Hashtbl.add tbl r (!id + 1);
    nodes_ref := node :: !nodes_ref; incr id; l := r
  done;
  let nodes = !nodes_ref |> List.rev in 
  let nodes_array = Array.of_list nodes in
  let length_of_nodes = List.length nodes in
  let open Node in
  Array.iteri (fun i node -> 
             let lst_idx = Array.length node.instrs - 1 in
             match node.instrs.(lst_idx) with 
               | `Goto(`Line_num(j)) -> 
                 let id' = Hashtbl.find tbl j in
                 node.succ <- [nodes_array.(id')];
                 nodes_array.(id').pred <- node :: nodes_array.(id').pred; 
                 node.instrs.(lst_idx) <- `Goto(`Line_num(id'+1))
               | `If(c, `Line_num(j)) -> 
                 let id' = Hashtbl.find tbl j in
                 node.succ <- [nodes_array.(id')];
                 nodes_array.(id').pred <- node :: nodes_array.(id').pred; 
                 if i < length_of_nodes - 1 then 
                 (node.succ <- nodes_array.(i+1) :: node.succ;
                 nodes_array.(i+1).pred <- node :: nodes_array.(i+1).pred);
                 node.instrs.(lst_idx) <- `If(c, `Line_num(id'+1))
               | _ -> 
                 if i < length_of_nodes - 1 then 
                 (node.succ <- nodes_array.(i+1) :: node.succ;
                 nodes_array.(i+1).pred <- node :: nodes_array.(i+1).pred)) nodes_array;
  let entry = Node.make_entry func.func_name
  and exit = Node.make_exit func.func_name in
  List.iter
  (fun node -> 
    begin 
      match get_preds node with 
        | [] -> node.pred <- [entry]; entry.succ <- node :: entry.succ
        | _ -> ()
    end;
    begin
      match get_succs node with 
        | [] -> node.succ <- [exit]; exit.pred <- node :: exit.pred
        | _ -> ()
    end) nodes;
  {
    pname = func.func_name;
    locals;
    formals;
    nodes = entry :: (nodes @ [exit]);
    node_num = List.length nodes + 2;
    start_node = entry;
    exit_node = exit;
  }




let from_func_singleton instrs = 
  from_func instrs (fun instrs -> let length = Array.length instrs in Array.make (length + 1) true)


let from_func instrs = from_func instrs find_leader

let string_of_proc : t -> string = fun procdesc -> 
  List.fold_left 
  (fun str node -> if Node.is_internal node then str ^ Node.string_of_node node else str) "" procdesc.nodes
  ^ "\n"

let from_prog : M.prog -> t list = 
  List.map from_func_singleton <-- fst

let string_of_t_list : t list -> string = 
  List.fold_left 
  (fun str proc -> str ^ "\n" ^ string_of_proc proc) "" 