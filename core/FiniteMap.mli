type ('a, 'value) t

val mkempty : ('a * 'value) list -> ('a, 'value) t 

val replace : 'a -> 'value -> ('a, 'value) t -> ('a, 'value) t 

val fold_meet : ('value -> 'value -> 'value) -> ('a, 'value) t -> ('a, 'value) t -> ('a, 'value) t 

val find : ('a, 'value) t -> 'a -> 'value

val to_alist : ('a, 'value) t -> ('a * 'value) list

val equal : ('a, 'value) t -> ('a, 'value) t -> bool

val copy : ('a, 'value) t -> ('a, 'value) t

val iter : ('a -> 'value -> unit) -> ('a, 'value) t -> unit

val fold : ('a -> 'value -> 'acc -> 'acc) -> ('a, 'value) t -> 'acc -> 'acc

val filter_map_inplace : ('a -> 'value -> 'value option) -> ('a, 'value) t -> unit

val fold_equal : ('v -> 'v -> bool) -> ('a, 'v) t -> ('a, 'v) t -> bool