open Core_kernel
open Integer

module type S = sig
  type t
  val init : Cartridge.t option -> t
  val mirroring : t -> Cartridge.Mirroring.t
  val read8 : t -> u16 -> u8
  val write8 : t -> u16 -> u8 -> unit
end

(* Mapper invalid *)
module NoneMMC = struct
  type empty
  type t = empty option
  let init (_:Cartridge.t option) : t = None
  let mirroring _ = invalid_arg "uninitialized mapper"
  let read8 _ _ = invalid_arg "uninitialized mapper"
  let write8 _ _ _ = invalid_arg "uninitialized mapper"
end

(* Mapper 0 *)
module Nrom = struct
  type t = {
    mutable mirror: Cartridge.Mirroring.t;
    cart : Cartridge.t
  }

  let init = function
    | Some c -> Cartridge.{mirror = c.mirror; cart = c}
    | None -> invalid_arg "invalid cartridge"

  let mirroring r = r.mirror

  let read8 m addr =
    let char_to_u8 c = U8.of_int @@ int_of_char c in
    let open Cartridge in
    match () with
    | _ when U16.(addr <<< of_int 0x2000) -> char_to_u8 m.cart.chr.{U16.to_int addr} (* ppu address space *)
    | _ when U16.(addr <<< of_int 0x4020) -> assert false
    | _ when U16.(addr <<< of_int 0x6000) -> U8.zero (* unused *)
    | _ when U16.(addr <<< of_int 0x8000) -> char_to_u8 m.cart.prgram.{U16.to_int addr - 0x6000}
    | _ ->
        if Bigstring.length m.cart.prgrom > 0x4000 then
          char_to_u8 m.cart.prgrom.{U16.to_int addr - 0x8000}
        else (* mirror read *)
          char_to_u8 m.cart.prgrom.{U16.(to_int (addr &&& of_int 0x3FFF))}

  let write8 m addr v =
    let u8_to_char v = Char.unsafe_chr @@ U8.to_int v in
    let open Cartridge in
    match () with
    | _ when U16.(addr <<< of_int 0x2000) -> m.cart.chr.{U16.to_int addr} <- u8_to_char v (* ppu address space *)
    | _ when U16.(addr <<< of_int 0x4020) -> assert false
    | _ when U16.(addr <<< of_int 0x6000) -> () (* unused *)
    | _ when U16.(addr <<< of_int 0x8000) -> m.cart.prgram.{U16.to_int addr - 0x6000} <- u8_to_char v
    | _ -> failwith "ROM not writable" (* rom *)
end

module MMC1 = struct
end

module MMC2 = struct
end

module type Obj = sig
  module Mapper : S
  val this : Mapper.t
end

let make_mapper (module M : S) cart =
  (module struct
    module Mapper = M
    let this = M.init cart
  end : Obj)

(* global reference to mapper instance *)
let mmc = ref (make_mapper (module NoneMMC) None)

let register_mapper cart =
  match cart with
  | Some c ->
      begin
      let open Cartridge in
      match c.mapper_id with
      | 0 -> mmc := make_mapper (module Nrom) cart
      | _ -> invalid_arg "Mapper not implemented"
      end
  | None -> invalid_arg "invalid cartridge"
