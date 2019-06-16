
module Bitv = Cm_util.Bitv

(* elt_to_int, int_to_elt, bitvector *)
type 'a t = ('a,int) Hashtbl.t * (int,'a) Hashtbl.t * Bitv.t

let mkempty : 'a list -> 'a t = fun all ->
  let length = List.length all in
  let elt_to_int = Hashtbl.create length 
  and int_to_elt = Hashtbl.create length in
  begin 
    List.iteri 
      (fun i elt -> 
        Hashtbl.add elt_to_int elt i;
        Hashtbl.add int_to_elt i elt) all
  end; (elt_to_int, int_to_elt, Bitv.create length false)


let insert : 'a -> 'a t -> 'a t = fun x (tbl1, tbl2, bitv) -> 
  let bitv = Bitv.copy bitv in 
  Bitv.set bitv (Hashtbl.find tbl1 x) true; (tbl1, tbl2, bitv)

let remove : 'a -> 'a t -> 'a t = fun x (tbl1, tbl2, bitv) ->
  let bitv = Bitv.copy bitv in   
  Bitv.set bitv (Hashtbl.find tbl1 x) false; (tbl1, tbl2, bitv)

let mem : 'a -> 'a t -> bool = fun x (tbl1, _, bitv) -> 
  Bitv.get bitv (Hashtbl.find tbl1 x)

let mems : 'a t -> 'a list = fun (_, tbl2, bitv) -> 
  Bitv.foldi_left 
    (fun acc i b -> 
      if b then Hashtbl.find tbl2 i :: acc else acc) [] bitv

let union : 'a t -> 'a t -> 'a t = 
  fun (tbl1, tbl2, bv1) (_, _, bv2) -> 
    (tbl1, tbl2, Bitv.bw_or bv1 bv2)

let inter : 'a t -> 'a t -> 'a t = 
  fun (tbl1, tbl2, bv1) (_, _, bv2) -> 
    (tbl1, tbl2, Bitv.bw_and bv1 bv2)

let negate : 'a t -> 'a t = 
  fun (tbl1, tbl2, bv) ->
    (tbl1, tbl2, Bitv.bw_not bv)

let diff : 'a t -> 'a t -> 'a t = 
  fun bs1 bs2 -> 
    inter bs1 (negate bs2)

let equal : 'a t -> 'a t -> bool = 
  fun (_, _, bv1) (_, _, bv2) -> 
    let res = ref true in 
    Bitv.iteri_true
      (fun i -> if not (Bitv.get bv2 i) then res := false)
        bv1; !res


let to_list : 'a t -> 'a list = 
  fun (_, id_to_elt, bv) -> 
    let id_list = Bitv.to_list bv in 
    List.map (fun id -> Hashtbl.find id_to_elt id) id_list
