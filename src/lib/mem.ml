open Core_kernel
open Integer

type t = {
  ppu: Ppu.t;
(*  apumap: Apumem.t;
  inputmem: Inputmem.t; *)

  ram: Bigstring.t;
}

let init ppu =
  let ram = Bigstring.create 0x800 in
  { ppu; ram }

let read8 mem addr =
  if U16.(addr <<< of_int 0x2000) then
    U8.of_int @@ int_of_char @@ mem.ram.{U16.(to_int (addr &&& (of_int 0x7FF)))}
  else if U16.(addr <<< of_int 0x4000) then
    Ppu.read_register mem.ppu addr
(*  else if U16.(addr <<< of_int 0x4016) then
    Apumem.read8 mmc.apumem addr
  else if U16.(addr <<< of_int 0x4018) then
    Inputmem.read8 mmc.inputmem addr *)
  else if U16.(addr <<< of_int 0x4020) then (* CPU test mode *)
    U8.zero
  else (* 0x4020 - 0x5FFF ( some mappers map it or just read 0; *)
    let module M = (val !Mapper.mmc : Mapper.Obj) in
    M.Mapper.read8 M.this addr

(* TODO: OAMDMA 0x4014 *)
let write8 mem addr v =
  if U16.(addr <<< of_int 0x2000) then
    mem.ram.{U16.(to_int (addr &&& (of_int 0x7FF)))} <- Char.unsafe_chr @@ U8.to_int v
  else if U16.(addr <<< of_int 0x4000) then
    Ppu.write_register mem.ppu addr v
(*  else if U16.(addr === of_int 0x4016) then
    InputMem.write8 mmc.inputmem addr v
  else if U16.(addr <<< of_int 0x4018) then
    Apumem.write8 mmc.apumem addr v*)
  else if U16.(addr <<< of_int 0x4020) then (* CPU test mode *)
    ()
  else (* 0x4020 - 0x5FFF ( some mappers map it or do nothing; *)
    let module M = (val !Mapper.mmc : Mapper.Obj) in
    M.Mapper.write8 M.this addr v
