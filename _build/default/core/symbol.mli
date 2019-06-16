(* This module maps a string (identifier) to an abstract symbol
   to enable fast comparison *)

type t 

val symbol : string -> t 

val name : t -> string 

type 'a table 

val empty : 'a table

val enter : t -> 'a -> 'a table -> 'a table 

val lookup : t -> 'a table -> 'a

val map : ('a -> 'b) -> 'a table -> 'b table