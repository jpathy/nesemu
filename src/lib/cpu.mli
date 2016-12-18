type t

val init : Mem.t -> t
val reset : t -> Mem.t -> unit
val step : t -> Mem.t -> int
