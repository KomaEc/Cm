

(* Points are represented as a cell !!! *)
type 'a point = {
  mutable link : 'a link
}

and 'a link = 
  | Link of 'a point
  | Info of 'a info

and 'a info = {
  mutable weight : int;
  mutable descriptor : 'a;
}

(* create a fresh point *)
let fresh desc = 
  { link = Info { weight = 1; descriptor = desc } }


let rec repr point = 
  match point.link with 
    | Info _ -> point 
    | Link point' -> 
    let point'' = repr point' in 
      if point'' != point' then 
        point.link <- point'.link;
      point''

let rec find point = 
  match point.link with 
    | Info info -> info.descriptor 
    | Link { link = Info info } -> info.descriptor 
    | Link { link = Link point' } -> find point'


let rec change point v = 
  match point.link with 
    | Info info 
    | Link { link = Info info } -> 
    info.descriptor <- v 
    | Link _ -> 
    change (repr point) v

let equivalent point1 point2 = 
  repr point1 == repr point2 


let union point1 point2 = 
  let point1 = repr point1 
  and point2 = repr point2 in 
    assert (point1 != point2);
    match point1.link, point2.link with 
      | Info info1, Info info2 -> 
      let weight1 = info1.weight
      and weight2 = info2.weight in
        if weight1 >= weight2 then 
          begin
            point2.link <- Link point1;
            info1.weight <- weight1 + weight2;
            info1.descriptor <- info2.descriptor
          end 
        else 
          begin
            point1.link <- Link point2;
            info2.weight <- weight1 + weight2
          end 
      | _, _ -> assert false
