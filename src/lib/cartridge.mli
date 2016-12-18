open Core_kernel

module Mirroring : sig
  type t =
    | Vertical
    | Horizontal
    | FourScreen
    | SingleA
    | SingleB
end

type t = {
  bat_sram : bool;
  mapper_id : int;
  mirror : Mirroring.t;
  trainer : Bigstring.t;
  prgram : Bigstring.t;
  prgrom : Bigstring.t;
  chr : Bigstring.t;
}

val of_bigarray : ?off:int -> Bigstring.t -> t
