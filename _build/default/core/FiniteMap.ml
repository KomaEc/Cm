module H = Hashtbl 

type ('a, 'value) t = ('a, 'value) H.t 

let mkempty : ('a * 'value) list -> ('a, 'value) t = fun all -> 
  let tbl = H.create 16 in 
  List.iter (fun (x, v) -> H.add tbl x v) all; tbl

let replace : 'a -> 'value -> ('a, 'value) t -> ('a, 'value) t =
  fun x v tbl -> 
    let tbl' = H.copy tbl in 
    H.replace tbl' x v; tbl' 

let fold_meet
: ('value -> 'value -> 'value) -> ('a, 'value) t -> ('a, 'value) t -> ('a, 'value) t = 
  fun meet tbl1 tbl2 -> 
    let res = H.copy tbl1 in 
    H.iter 
      (fun x v -> 
        let v' = H.find tbl2 x in 
        H.replace res x (meet v v')) tbl1; res

let find : ('a, 'value) t -> 'a -> 'value = 
  H.find


let to_alist : ('a, 'value) t -> ('a * 'value) list = 
  fun tbl -> H.to_seq tbl 
  |> Seq.fold_left 
     (fun acc x -> x :: acc) []


let equal' : ('a, 'value) t -> ('a, 'value) t -> bool = 
  fun tbl1 tbl2 -> 
  H.fold 
  (fun x v acc -> 
  let v' = H.find tbl2 x in 
  acc && (v = v')) tbl1 true (* Notice polymorphic equal !!!!!!!!!!!!!!!!!!! For example, it can't be used to play with ExprSet*)

let equal tbl1 tbl2 = equal' tbl1 tbl2 && equal' tbl2 tbl1


let fold_equal : ('v -> 'v -> bool) -> ('a, 'v) t -> ('a, 'v) t -> bool = fun (=) ->
  let equal' = 
    fun tbl1 tbl2 -> 
    H.fold 
    (fun x v acc -> 
    let v' = H.find tbl2 x in 
    acc && (v = v')) tbl1 true in
  fun tbl1 tbl2 -> equal' tbl1 tbl2 && equal' tbl2 tbl1


let iter : ('a -> 'value -> unit) -> ('a, 'value) t -> unit = 
  H.iter

let copy : ('a, 'value) t -> ('a, 'value) t =
  H.copy


let fold : ('a -> 'value -> 'acc -> 'acc) -> ('a, 'value) t -> 'acc -> 'acc = 
  H.fold

let filter_map_inplace : ('a -> 'value -> 'value option) -> ('a, 'value) t -> unit =
  H.filter_map_inplace