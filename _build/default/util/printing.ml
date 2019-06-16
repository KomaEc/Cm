

let rec string_of_list : ('a -> string) -> 'a list -> string = 
  fun string_of -> 
    function 
      | [] -> "empty" 
      | [x] -> string_of x 
      | x :: xs -> string_of x ^ ", " ^ string_of_list string_of xs