
module U = Cm_util.Util
open U

module IdSet = Set.Make(struct type t = Procdesc.Node.id let compare = compare end)

type abstract_value = IdSet.t

module type S = 
sig

  val transfer : Mimple.stmt -> abstract_value -> abstract_value

  val meet : abstract_value -> abstract_value -> abstract_value

  val equal : abstract_value -> abstract_value -> bool

end

module X : S = 
struct
  let transfer _ x = x

  let meet = IdSet.inter

  let equal = IdSet.equal
end

module D
  (X : S) = 
struct

  let transfer_for_node : Procdesc.Node.t -> abstract_value -> abstract_value = 
    fun node -> 
      let instrs = Procdesc.Node.get_instrs node in 
      Array.fold_left
        (fun trs stmt -> 
        X.transfer stmt <-- trs) (fun x -> x) instrs

  let from_proc (proc : Procdesc.t) : abstract_value Dataflow.t = 
    let bottom = Procdesc.fold 
                  (fun acc node -> IdSet.add (Procdesc.Node.get_id node) acc)
                  IdSet.empty proc
    and entry_facts = IdSet.singleton (Procdesc.Node.entry_id) in
    let transfer_table : (Procdesc.Node.id, abstract_value -> abstract_value) Hashtbl.t = Hashtbl.create 16 in 
    Procdesc.iter (fun node -> Hashtbl.add transfer_table (Procdesc.Node.get_id node) (transfer_for_node node)) proc;
    {
      proc;
      dir = Dataflow.D_Forward;
      meet = X.meet;
      equal = X.equal;
      transfer = (fun node -> Hashtbl.find transfer_table (Procdesc.Node.get_id node));
      entry_or_exit_facts = entry_facts;
      bottom;
    }   
end

module Dom = D(X)

open Dom

let dominator_set (proc : Procdesc.t) =
  proc
  |> from_proc
  |> Dataflow.do_dfa
  |> fst