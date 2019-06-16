
type t = string * int 

let idx = ref 0
let newsymb () = let t = !idx in idx := t - 1; t
let hashtbl = Hashtbl.create 128

let symbol s =
  try let i = Hashtbl.find hashtbl s in (s,i) 
  with Not_found ->  let i = newsymb () in 
                     Hashtbl.add hashtbl s i;
                     (s,i)

let name (s,_) = s

module IntMap = Map.Make(struct type t = int let compare = compare end)

type 'a table = 'a IntMap.t

let empty = IntMap.empty

let enter (_,n) a t = IntMap.add n a t
let lookup (_,n) t = IntMap.find n t

let map = IntMap.map