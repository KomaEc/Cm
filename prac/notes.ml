module type FILE = sig 

  type readonly
  type readwrite
  type 'a t
  val open_readwrite : string -> readwrite t 
  val open_readonly : string -> readonly t 
  val read : 'a t -> string
  val write : readwrite t -> string -> unit

end

(* module File : FILE = struct 
 *   type readonly
 *   type readwrite
 *   type 'a t = int (* Phantom type!! *)
 *   ...
 * end*)


 (* expressing existential type vai pack and open in sum type *)

 type t = E : 'a * ('a -> 'a) * ('a -> string) -> t
 (* exist 'a. 'a * ('a -> 'a) * ('a -> string) *)

(* pack *)
let ints = E(0, (fun x -> x + 1), string_of_int)

(* open *)
let s =
  let E(z, s, p) = ints in 
  p (s (s z))



type z = Z : z
type 'n s = S : 'n -> 'n s

let ex1 = S (S (S Z))

type ('a, _) gtree = 
  | EmptyG : ('a, z) gtree
  | TreeG : ('a, 'n) gtree * 'a * ('a, 'n) gtree -> ('a, 'n s) gtree


type _ ntree = EmptyN : 'a ntree
| TreeN : 'a * ('a * 'a) ntree -> 'a ntree

let rec swiv : 'a. ('a -> 'a) -> 'a ntree -> 'a ntree =  
  fun f -> function 
    | EmptyN -> EmptyN 
    | TreeN(v, t) -> TreeN(f v, swiv (fun (x, y) -> (f y, f x)) t)

let swivelN = swiv (fun x -> x)

let swivelN p = swiv (fun x -> x) p (* watch out for value restriction! *)

(* Cannot annotate it with type signatrue 'a 'n. ('a, 'n) gtree -> 'n
 * Because the return type is different for each branche.
 * Introduce type parameter a and n for type refinement for each
 * branch *)
let rec depthG : type a n. (a, n) gtree -> n =
  function
    | EmptyG -> Z
    | TreeG(l, _, _) -> S (depthG l)

let rec topG : type a n. (a, n s) gtree -> a =
  function
    | TreeG(_, v, _) -> v

let rec swivelG : type a n. (a, n) gtree -> (a, n) gtree = 
  function 
    | EmptyG -> EmptyG
    | TreeG(l, v, r) -> TreeG(swivelG r, v, swivelG l)

let rec zipTree
  : type a b n. (a, n) gtree -> (b, n) gtree -> (a * b, n) gtree = 
  fun x y -> match x, y with 
    | EmptyG, EmptyG -> EmptyG
    | TreeG(l1, v1, r1), TreeG(l2, v2, r2) ->
      TreeG(zipTree l1 l2, (v1, v2), zipTree r1 r2)

let rec nestify : type a n. (a, n) gtree -> a ntree = 
  function
    | EmptyG -> EmptyN
    | TreeG(l, v, r) -> TreeN(v, nestify (zipTree l r))

type (_,_,_) max =
    MaxEq : 'a -> ('a, 'a, 'a) max
  | MaxFlip : ('a, 'b, 'c) max -> ('b, 'a, 'c) max
  | MaxSuc : ('a, 'b, 'a) max -> ('a s, 'b, 'a s) max

let witness1 = MaxFlip (MaxSuc (MaxSuc (MaxEq (S Z))))

let rec max : type a b c. (a, b, c) max -> c = function
  | MaxEq a -> a
  | MaxFlip m -> max m
  | MaxSuc m -> S (max m)

type ('a,_) dtree = 
    EmptyD : ('a,z) dtree
  | TreeD : ('a,'m) dtree * 'a * ('a,'n) dtree * ('m,'n,'o) max -> ('a,'o s) dtree

let rec depthD : type a n. (a, n) dtree -> n =
  function
    | EmptyD -> Z
    | TreeD(l, v, r, m) -> S (max m)

let rec topD : type a n. (a, n s) dtree -> a = function
  TreeD(_, v, _, m) -> v

let rec swivelD : type a n. (a, n) dtree -> (a, n) dtree = function
  | EmptyD -> EmptyD
  | TreeD(l, v, r, m) -> TreeD(swivelD r, v, swivelD l, MaxFlip m)

type (_,_) eql = Refl : ('a, 'a) eql

let symm : type a b. (a, b) eql -> (b, a) eql = 
  fun Refl -> Refl

let trans : type a b c. (a, b) eql -> (b, c) eql -> (a, c) eql =
  fun Refl Refl -> Refl

module Lift (T : sig type _ t end) : 
sig
  val lift : ('a, 'b) eql -> ('a T.t, 'b T.t) eql
end = 
struct 
  let lift : type a b. (a, b) eql -> (a T.t, b T.t) eql = 
    fun Refl -> Refl
end

let cast : type a b. (a, b) eql -> a -> b = 
  fun Refl a -> a

type ('a, 'n) etree = 
  | EmptyE : ('n, z) eql -> ('a, 'n) etree
  | TreeE : ('n, 'm s) eql *
    ('a, 'm) etree * 'a * ('a, 'm) etree -> ('a, 'n) etree

(* When pattern matching, the Refl brings local type refinement *)
  



module type MONAD = 
sig 
  type 'a t
  val return : 'a -> 'a t (* represent trivial computation *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

(* the type 'a t represent some computation that 
 * performs some effects then returns a result 
 * off type 'a *)

(* Three monad laws :: 
 * 1. return v >>= k === k v 
 * 2. m >>= return === m   -- return is kind of right unit of the operator >>=
 * 3. (m >>= f) >>= g === m >>= (fun x -> f x >>= g)  -- >>= is associative!  *)


(* reading and updating a single reference cell *)
module type STATE = 
sig
  type state (* denoting the type of the cell *)
  type 'a t
  module Monad : MONAD with type 'a t = 'a t
  val get : state t  (* a computation without parameters that return a value of type state *)
  val put : state -> unit t
  val runState : 'a t -> state -> state * 'a (* run computation *)
end




module State (S : sig type t end) : STATE with type state = S.t =  
struct
  type state = S.t
  type 'a t = state -> state * 'a 
  module Monad : MONAD with type 'a t = 'a t = 
  struct
    type 'a t = state -> state * 'a
    let return v s = (s, v)
    let (>>=) m k s = let (s', v) = m s in k v s'
  end
  let get s = (s, s)
  let put s = fun _ -> (s, ())
  let runState m init = m init
end

type 'a tree = 
  | Empty : 'a tree 
  | Tree : 'a tree * 'a * 'a tree -> 'a tree

module Monad_of_state(S : STATE) : MONAD with type 'a t = 'a S.t = S.Monad

module IState = State(struct type t = int end)

let fresh_name : string IState.t = (* fresh_name is a computation that returns a string *)
  let open IState in 
  let open Monad in 
  get >>= fun i -> 
  put (i + 1) >>= fun () -> 
  return (Printf.sprintf "x%d" i)

