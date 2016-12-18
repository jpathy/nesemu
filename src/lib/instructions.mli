open Integer

type t = u16 * (u16 -> u8)

val cycle_table : int array
val page_cross_penalty : int array
val pp : Format.formatter -> t -> unit
