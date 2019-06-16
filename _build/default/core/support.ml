open Printf

module Error = struct 

exception Exit of int 

type info = FI of string * int * int | UNKNOWN 
type 'a withinfo = {i : info; v : 'a}

let dummyinfo = UNKNOWN 
let createInfo f i c = FI(f, i, c)


let printInfo outx = function
  | FI (f, l, c) -> 
    fprintf outx "%s : line %d : offset %d:" f l c
  | UNKNOWN -> 
    fprintf outx "%s" "<Unknown file and line>: "

end