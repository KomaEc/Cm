type 'a t (* a bit vector set containing elements of type 'a *)

(* Return an empty set that ranges over the elements in the list. *)
val mkempty : 'a list -> 'a t

(* Add an element to a set, or do nothing if elt already present.
   Raises an exception if elt not in range of set. *)
val insert : 'a -> 'a t -> 'a t

(* Remove an element to a set, or do nothing if elt not present.
   Raises an exception if elt not in range of set. *)
val remove : 'a -> 'a t -> 'a t

(* Return true iff element in set. Raises an exception if elt not in
   range of set. *)
val mem : 'a -> 'a t -> bool

(* Return a list of all members of the set. *)
val mems : 'a t -> 'a list

(* Set union. Raises an exception if sets not derived from same
   mkempty call. *)
val union : 'a t -> 'a t -> 'a t

(* Set intersection. Raises an exception if sets not derived from same
   mkempty call. *)
val inter : 'a t -> 'a t -> 'a t

(* diff a b returns a - b. Raises an exception if sets not derived
   from same mkempty call. *)
val diff : 'a t -> 'a t -> 'a t

(* negate a returns 1 - a, where 1 is the set of all elements. Thus,
   negate (mkempty l) returns top. *)
val negate : 'a t -> 'a t

(* Return true iff sets are equal. Raises an exception if sets not
   derived from same mkempty call. *)
val equal : 'a t -> 'a t -> bool


val to_list : 'a t -> 'a list
