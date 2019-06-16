
type t = Name of Symbol.t | Abstract of int

let tempbase = ref (-1)

let newtemp ?hint () = 
  match hint with 
  | None -> let t = !tempbase in 
            incr tempbase; Abstract(t)
  | Some name -> Name(name)

let place_holder = newtemp()

let string_of_temp = function 
  | Abstract(i) -> "_t"^(string_of_int i)
  | Name(s) -> Symbol.name s

type label = Symbol.t 

let labbase = ref 0

let dummy : label = Symbol.symbol "dummy"

let newlabel ?hint () = 
  match hint with 
  | None -> let i = !labbase in 
            incr labbase;
            Symbol.symbol ("L"^(string_of_int i))
  | Some name -> name

let re_fresh_label () = 
  labbase := 0

let string_of_label = Symbol.name