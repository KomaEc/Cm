

type ty = 
  | Primitive of primitive_type
  | Object of obj_type

and primitive_type = [
  | `Int | `Bool | `Void 
]

and obj_type = [
  | `Object
  | `ClassTy of Symbol.t
  | `ArrayTy of ty
]

let rec string_of_ty : ty -> string = 
  function 
    | Primitive(`Int) -> "int"
    | Primitive(`Bool) -> "bool"
    | Primitive(`Void) -> "void"
    | Object(`Object) -> "Object" 
    | Object(`ClassTy(name)) -> string_of_class_type name
    | Object(`ArrayTy(ty)) -> string_of_ty ty ^ "[]"

and string_of_class_type : Symbol.t -> string = 
  Symbol.name
      
and string_of_ty_list : ty list -> string = 
  fun ty_list -> 
    begin 
      match ty_list with 
        | [] -> ")" 
        | [ty] -> string_of_ty ty ^ ")"
        | ty :: tyl -> 
          string_of_ty ty 
          ^ List.fold_left 
              (fun acc ty -> 
                acc ^ ", " ^ string_of_ty ty) "" tyl ^ ")"
    end
    |> ( ^ ) "("