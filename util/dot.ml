

type graph_type = [
  | `Digraph 
  | `Graph
]  
and graph = (* bool stands for strict or not *)
  bool * graph_type * string option * stmt list

and stmt = [
  | `NodeStmt of node_stmt
  | `EdgeStmt of edge_stmt
  | `AttrStmt of attr_stmt
  | `Eq of string * string
  | `Sub of string * graph
]

and attr_stmt = 
  [ `Graph | `Node | `Edge ] * attr_list

and attr_list = (string * string) list (* `color=red` *)

and edge_stmt = 
  [ `Node of string | `Sub of string ] * edgeRHS * attr_list option

and edgeRHS = 
  RHS of edgeop * [ `Node of string | `Sub of string ] * edgeRHS option

and edgeop = graph_type (* depend on graph type *)

and node_stmt = 
  string * attr_list option

let get_id_from_node = function
  | `Node id -> id
  | `Sub id -> id

let string_of_graph_type : graph_type -> string = function 
  | `Digraph -> "digraph"
  | `Graph -> "graph"

let string_of_edgeop : edgeop -> string = function 
  | `Digraph -> "->"
  | `Graph -> "--"

let string_of_attr_list : attr_list -> string = 
  (Printing.string_of_list (fun (id1, id2) -> id1 ^ "=" ^ id2))

let rec string_of_edgeRHS : edgeRHS -> string = 
  function 
    RHS (edgeop, x, someRHS) -> 
      let id = get_id_from_node x in 
      let arrow = match edgeop with 
        | `Digraph -> "->"
        | `Graph -> "--" in 
      arrow ^ id ^ 
      match someRHS with 
        | Some rhs -> 
          string_of_edgeRHS rhs
        | None -> ""

let prefix = ref 0

let resume () = prefix := 0

let into () = incr prefix

let outof () = decr prefix

let prefix_tap () = 
  let rec aux n = 
    if n = 0 then ""
    else "\t" ^ aux (n-1) in 
  aux !prefix


let output_stmt : out_channel -> stmt -> unit = fun out -> 
  fun stmt -> (match stmt with
    | `NodeStmt (id, None) -> 
      output_string out @@ prefix_tap() ^  id
    | `NodeStmt (id, Some(attr_list)) -> 
      output_string out @@ prefix_tap() ^  id ^ " [" ^ string_of_attr_list attr_list ^ "]"
    | `EdgeStmt (x, rhs, maybeAttr) -> 
      let id = get_id_from_node x in 
      let tl = match maybeAttr with 
        | Some attr_list -> " [" ^ string_of_attr_list attr_list ^ "]"
        | None -> "" in
      output_string out @@ prefix_tap() ^  id ^ string_of_edgeRHS rhs ^ tl
    | `AttrStmt (keyword, attr_list) ->  
      let keyword = 
        match keyword with 
          | `Graph -> "graph"
          | `Node -> "node"
          | `Edge -> "edge" in 
      output_string out @@ prefix_tap() ^  keyword ^ " [" ^ string_of_attr_list attr_list ^ "]"
    | `Eq (id1, id2) -> 
      output_string out @@ prefix_tap() ^  id1 ^ "=" ^ id2
    | `Sub _ -> 
      failwith "unimplemented");
      output_string out ";\n"

let output_graph_aux : out_channel -> graph -> unit = fun log_out -> function (strict, graph_type, maybeId, stmt_list) -> 
    let sign = if strict then "strict " else "" in
    let ty = string_of_graph_type graph_type in
    let id = match maybeId with 
      | Some id -> " " ^ id
      | None -> "" in
    output_string log_out (prefix_tap() ^ sign ^ ty ^ id ^ " {\n");
    into ();
    List.iter (output_stmt log_out) stmt_list;
    outof ();
    output_string log_out (prefix_tap() ^ "}")

(*
let output_graph ?name : graph -> unit = 
  let fn = (match name with 
    | Some name -> name
    | None -> "default") ^ ".dot" in
  let log_out = open_out fn in 
  let aux : graph -> unit = function (strict, graph_type, maybeId, stmt_list) -> 
    let sign = if strict then "strict " else "" in
    let ty = string_of_graph_type graph_type in
    let id = match maybeId with 
      | Some id -> " " ^ id
      | None -> "" in
    output_string log_out (prefix_tap() ^ sign ^ ty ^ id ^ " {\n");
    into ();
    List.iter (output_stmt log_out) stmt_list;
    outof ();
    output_string log_out (prefix_tap() ^ "}")
  in 
  resume();
  aux
  *)


let output_graph : graph -> unit =
  fun g -> 
    let _,_,maybeName,_ = g in
    let name = match maybeName with 
      | Some name -> name
      | _ -> "default" in
    let log_out = open_out @@ name ^ ".dot" in
    output_graph_aux log_out g



let make_edge : edgeop -> string list -> stmt = fun edgeop -> 
  let rec aux : string list -> edgeRHS option = function
    | [] -> None
    | id::ids -> 
      Some (RHS (edgeop, `Node id, aux ids)) in
  function 
    | [] -> assert false
    | x::xs -> 
      match aux xs with 
        | None -> assert false
        | Some rhs -> 
          `EdgeStmt (`Node x, rhs, None)


let example_graph : graph = 
  ( false
  , `Digraph
  , Some "example"
  , [
    make_edge `Digraph ["A"; "B"; "C"];
    make_edge `Digraph ["B"; "D"];
    make_edge `Digraph ["D"; "A"]
  ])