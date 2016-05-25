type t

val make : int -> int -> t

val is_promotion_zone : t -> color:Color.t -> bool

val add : t -> int * int -> t option
val sub : t -> t -> int * int

val of_tuple : int * int -> t
val to_tuple : t -> int * int

val to_string : t -> string
val to_kif : t -> string
val to_usi : t -> string
