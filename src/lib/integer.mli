type u8
type s8
type u16
type s16

module type S = sig
  type t

  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val neg : t -> t
  val abs : t -> t

  val max_int : t
  val min_int : t

  val to_int : t -> int
  val of_int : int -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t

  val shift_left_logical : t -> int -> t
  val shift_right_logical : t -> int -> t

  val logor : t -> t -> t
  val logxor : t -> t -> t
  val logand : t -> t -> t
  val lognot : t -> t

  val compare : t -> t -> int

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( % ) : t -> t -> t
  val ( << ) : t -> int -> t
  val ( >> ) : t -> int -> t
  val ( &&& ) : t -> t -> t
  val ( ^^^ ) : t -> t -> t
  val ( ~~~ ) : t -> t
  val ( ||| ) : t -> t -> t
  val ( === ) : t -> t -> bool
  val ( <=> ) : t -> t -> bool
  val ( <<< ) : t -> t -> bool
  val ( >>> ) : t -> t -> bool
  val ( <<<= ) : t -> t -> bool
  val ( >>>= ) : t -> t -> bool

  val of_u8 : u8 -> t
  val to_u8 : t -> u8
  val of_s8 : s8 -> t
  val to_s8 : t -> s8
  val of_u16 : u16 -> t
  val to_u16 : t -> u16
  val of_s16 : s16 -> t
  val to_s16 : t -> s16
end

module U8 : S with type t = u8
module S8 : S with type t = s8
module U16 : S with type t = u16
module S16 : S with type t = s16
