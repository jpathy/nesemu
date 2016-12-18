(* Make_signed/unsigned taken from ocaml-stdint 
 * needed because stdint conv_* are using FFI *)
type u8 = int
type s8 = int
type u16 = int
type s16 = int

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

module Infix = struct
  module type IntSig = sig
    type t

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
  end

  module type S = sig
    type t
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
  end

  module Make_infix(I : IntSig) : S with type t = I.t = struct
    type t = I.t
    let ( + ) = I.add
    let ( - ) = I.sub
    let ( * ) = I.mul
    let ( / ) = I.div
    let ( % ) = I.rem
    let ( << ) = I.shift_left_logical
    let ( >> ) = I.shift_right_logical
    let ( &&& ) = I.logand
    let ( ^^^ ) = I.logxor
    let ( ~~~ ) = I.lognot
    let ( ||| ) = I.logor
    let ( === ) x y = I.compare x y = 0
    let ( <=> ) x y = not (I.compare x y = 0)
    let ( <<< ) x y = I.compare x y < 0
    let ( >>> ) x y = I.compare x y > 0
    let ( <<<= ) x y = x === y || x <<< y
    let ( >>>= ) x y = x === y || x >>> y
  end
end

module type Size = sig
  val bits : int
end

module Make_signed(I : Size) = struct
  type t = int

  let mask = (1 lsl I.bits) - 1

  let of_int x = x lsl (Sys.word_size - I.bits - 1)
  let to_int x = x asr (Sys.word_size - I.bits - 1)

  let max_int = of_int (1 lsl (I.bits - 1)) - 1
  let min_int = of_int ((-1) * (1 lsl (I.bits - 1)))

  let add = (+)
  let sub = (-)
  let mul a b = (to_int a) * b
  let div a b = of_int (a / b)
  let rem = (mod)

  let zero = 0
  let one = of_int 1
  let succ = add one
  let pred x = sub x one
  let abs = abs
  let neg x = (-1) * x

  let logand = (land)
  let logor = (lor)
  let logxor a b = (a lxor b) land (of_int mask)
  let lognot = logxor (of_int (-1))

  let shift_left_logical a b = a lsl b
  let shift_right_logical a b = (a lsr b) land (of_int mask)

  let compare = Pervasives.compare
end

module Make_unsigned(I : Size) = struct
  type t = int

  let mask = (1 lsl I.bits) - 1

  let of_int = (land) mask
  external to_int : t -> int = "%identity"

  let max_int = of_int ((1 lsl I.bits) - 1)
  let min_int = of_int 0

  let add a b = of_int (a + b)
  let sub a b = of_int (a - b)
  let mul a b = of_int (a * b)
  let div = (/)
  let rem = (mod)

  let zero = of_int 0
  let one = of_int 1
  let succ = add one
  let pred x = sub x one
  external abs : t -> t = "%identity"
  let neg x = of_int ((-1) * x)

  let logand = (land)
  let logor = (lor)
  let logxor a b = of_int (a lxor b)
  let lognot = logxor max_int

  let shift_left_logical a b = of_int (a lsl b)
  let shift_right_logical = (lsr)

  let compare = Pervasives.compare
end

module U8 = struct
  module M = Make_unsigned(struct let bits = 8 end)

  include M
  module Inf = Infix.Make_infix(M)
  include (Inf : module type of Inf with type t := u8)

  external of_u8 : t -> t = "%identity"
  external to_u8 : t -> t = "%identity"
  let of_u16 = of_int
  let to_u16 = to_int
  let of_s8 x = x lsr (Sys.word_size - 8 - 1)
  let to_s8 x = x lsl (Sys.word_size - 8 - 1)
  let of_s16 x = (x lsr (Sys.word_size - 16 - 1)) land M.mask
  let to_s16 x = (x land M.mask) lsl (Sys.word_size - 16 - 1)
end

module S8 = struct
  module M = Make_signed(struct let bits = 8 end)

  include M
  module Inf = Infix.Make_infix(M)
  include (Inf : module type of Inf with type t := s8)

  external of_s8 : t -> t = "%identity"
  external to_s8 : t -> t = "%identity"
  let of_s16 x = x lsl 8
  let to_s16 x = x asr 8
  let of_u8 = of_int
  let to_u8 = to_int
  let of_u16 = of_int
  let to_u16 = to_int
end

module U16 = struct
  module M = Make_unsigned(struct let bits = 16 end)

  include M
  module Inf = Infix.Make_infix(M)
  include (Inf : module type of Inf with type t := u16)

  external of_u16 : t -> t = "%identity"
  external to_u16 : t -> t = "%identity"
  let of_u8 = U8.to_u16
  let to_u8 = U8.of_u16
  let of_s8 = S8.to_u16
  let to_s8 = S8.of_u16
  let of_s16 x = x lsr (Sys.word_size - 16 - 1)
  let to_s16 x = x lsl (Sys.word_size - 16 - 1)
end

module S16 = struct
  module M = Make_signed(struct let bits = 16 end)

  include M
  module Inf = Infix.Make_infix(M)
  include (Inf : module type of Inf with type t := s16)

  external of_s16 : t -> t = "%identity"
  external to_s16 : t -> t = "%identity"
  let of_s8 = S8.to_u16
  let to_s8 = S8.of_u16
  let of_u8 = U8.to_s16
  let to_u8 = U8.of_s16
  let of_u16 = U16.to_s16
  let to_u16 = U16.of_s16
end
