open Core_kernel
open Integer

module Mem = struct
(* https://wiki.nesdev.com/w/index.php/PPU_registers
 * https://wiki.nesdev.com/w/index.php/PPU_memory_map
 * https://wiki.nesdev.com/w/index.php/PPU_scrolling *)
  type spritesize =
    | Sprite8x8
    | Sprite8x16

  type coloring = {gray: bool; red: bool; green: bool; blue: bool}

  type t = {
    oam: Bigstring.t; (* 256 bytes *)
    ciram: Bigstring.t; (* 2KiB *)
    palette: Bigstring.t; (* 32 bytes *)

    (* regs *)
    mutable ctrl: u8;
    mutable mask: u8;
    mutable status: u8;
    mutable oamaddr: u8;
    (* internal registers *)
    mutable rbuf: u8;
    mutable v: u16;
    mutable t: u16;
    mutable x: u8;
    mutable w: bool;
  }

  let init () =
    let oam = Bigstring.create 0x100 in
    let palette = Bigstring.create 0x20 in
    let ciram = Bigstring.create 0x800 in
    {
      oam; ciram; palette;
      ctrl=U8.zero; mask=U8.zero; status=U8.zero; oamaddr=U8.zero;
      rbuf=U8.zero; v=U16.zero; t=U16.zero; x=U8.zero; w=false;
    }

  (* PPUCTRL : 0x2000 *)
  let nmi_output mem = U8.(mem.ctrl &&& (of_int 0x80) <=> zero)

  let sprite_size mem =
    if U8.(mem.ctrl &&& (of_int 0x20) <=> zero) then Sprite8x16 else Sprite8x8

  let pta_bg mem =
    U16.of_int @@ if U8.(mem.ctrl &&& (of_int 0x10) <=> zero) then 0x1000 else 0

  let pta_8x8spr mem =
    U16.of_int @@ if U8.(mem.ctrl &&& (of_int 0x08) <=> zero) then 0x1000 else 0

  let vram_incr mem =
    U16.of_int @@ if U8.(mem.ctrl &&& (of_int 0x04) <=> zero) then 32 else 1

  (* PPUMASK : 0x2001 *)
  let show_bg mem = U8.(mem.mask &&& (of_int 0x08) <=> zero)

  let show_sprites mem = U8.(mem.mask &&& (of_int 0x10) <=> zero)

  let hide_bgleft mem = U8.(mem.mask &&& (of_int 0x02) === zero)

  let hide_spriteleft mem = U8.(mem.mask &&& (of_int 0x04) === zero)

  let color_info mem =
    let gray = U8.(mem.mask &&& (of_int 0x01) <=> zero) in
    let red = U8.(mem.mask &&& (of_int 0x20) <=> zero) in
    let green = U8.(mem.mask &&& (of_int 0x40) <=> zero) in
    let blue = U8.(mem.mask &&& (of_int 0x80) <=> zero) in
    {gray; red; green; blue}

  (* PPUSTATUS : 0x2002 *)
  let nmi_occured mem = U8.(mem.status &&& (of_int 0x80) <=> zero)

  let set_nmi_occured mem b =
    let bit = U8.of_int 0x80 in
    let f = if b then U8.((|||) bit) else U8.((&&&) ~~~bit) in
    mem.status <- f mem.status

  let set_spr0_hit mem b =
    let bit = U8.of_int 0x40 in
    let f = if b then U8.((|||) bit) else U8.((&&&) ~~~bit) in
    mem.status <- f mem.status

  let set_spr0_overflow mem b =
    let bit = U8.of_int 0x20 in
    let f = if b then U8.((|||) bit) else U8.((&&&) ~~~bit) in
    mem.status <- f mem.status

  let read_status mem =
    let v = mem.status in
    mem.status <- U8.(mem.status &&& (of_int 0x7F)); (* clear nmi_occured *)
    mem.w <- false; v

  let read_oam mem =
    U8.of_int @@ int_of_char @@ mem.oam.{U8.to_int mem.oamaddr} (* read from addr *)

  let incvram mem =
    mem.v <- U16.((mem.v + vram_incr mem) &&& (of_int 0x3FF))

  let read8 mem addr =
    let module M = (val !Mapper.mmc : Mapper.Obj) in
    if U16.(addr <<< of_int 0x2000) then (* pattern table *)
      M.Mapper.read8 M.this addr
    else if U16.(addr <<< of_int 0x3F00) then (* ciram(mirrored nametable) *)
      let lo = U16.(addr &&& (of_int 0x800) === zero) in
      let addr = U16.(addr &&& (of_int 0x7FF)) in (* 0x2000 .. 0x2FFF -> 0x0 .. 0xFFF and 0x3000..0x3F00 mirrors *)
      match M.Mapper.mirroring M.this with
      | Cartridge.Mirroring.Horizontal ->
          U8.of_int @@ int_of_char @@
          if lo then
            mem.ciram.{U16.(to_int (addr &&& (of_int 0x3FF)))}
          else
            mem.ciram.{U16.(to_int (addr ||| (of_int 0x400)))}
      | Cartridge.Mirroring.Vertical ->
          U8.of_int @@ int_of_char mem.ciram.{U16.to_int addr}
      | Cartridge.Mirroring.SingleA ->
          U8.of_int @@ int_of_char mem.ciram.{U16.(to_int (addr &&& (of_int 0x3FF)))}
      | Cartridge.Mirroring.SingleB ->
          U8.of_int @@ int_of_char mem.ciram.{U16.(to_int (addr ||| (of_int 0x400)))}
      | Cartridge.Mirroring.FourScreen -> M.Mapper.read8 M.this addr
    else if U16.(addr <<< of_int 0x3F00) then (* palette memory *)
      let addr = U16.(addr &&& (of_int 0x1F)) in
      U8.of_int @@ int_of_char mem.palette.{U16.to_int addr}
    else
      assert false

  let read_data mem =
    if U16.(mem.v <<< of_int 0x3F00) then
      let v = mem.rbuf in (mem.rbuf <- read8 mem mem.v; incvram mem; v)
    else
      (mem.rbuf <- read8 mem U16.(mem.v &&& (of_int 0x2FFF)); read8 mem mem.v)

  let write_ppuctrl mem v =
    mem.ctrl <- v;
    mem.t <- U16.(mem.t &&& (of_int 0xF3FF) ||| ((of_u8 v) << 10))

  let write_oam mem v =
    mem.oam.{U8.to_int mem.oamaddr} <- Char.unsafe_chr @@ U8.to_int v;
    mem.oamaddr <- U8.(mem.oamaddr+one)

  let write8 mem addr v =
    let module M = (val !Mapper.mmc : Mapper.Obj) in
    if U16.(addr <<< of_int 0x2000) then (* pattern table *)
      M.Mapper.write8 M.this addr v
    else if U16.(addr <<< of_int 0x3F00) then (* ciram(mirrored nametable) *)
      let _v = Char.unsafe_chr @@ U8.to_int v in
      let lo = U16.(addr &&& (of_int 0x800) === zero) in
      let addr = U16.(addr &&& (of_int 0x7FF)) in (* 0x2000 .. 0x2FFF -> 0x0 .. 0xFFF and 0x3000..0x3F00 mirrors *)
      match M.Mapper.mirroring M.this with
      | Cartridge.Mirroring.Horizontal ->
          if lo then
            mem.ciram.{U16.(to_int (addr &&& (of_int 0x3FF)))} <- _v
          else
            mem.ciram.{U16.(to_int (addr ||| (of_int 0x400)))} <- _v
      | Cartridge.Mirroring.Vertical ->
          mem.ciram.{U16.to_int addr} <- _v
      | Cartridge.Mirroring.SingleA ->
          mem.ciram.{U16.(to_int (addr &&& (of_int 0x3FF)))} <- _v
      | Cartridge.Mirroring.SingleB ->
          mem.ciram.{U16.(to_int (addr ||| (of_int 0x400)))} <- _v
      | Cartridge.Mirroring.FourScreen -> M.Mapper.write8 M.this addr v
    else if U16.(addr <<< of_int 0x4000) then (* palette memory *)
      let addr = U16.(addr &&& (of_int 0x1F)) in
      mem.palette.{U16.to_int addr} <- Char.unsafe_chr @@ U8.to_int v
    else
      assert false

  let write_addr mem v =
    if not mem.w then
      (mem.t <- U16.(mem.t &&& (of_int 0xFF) ||| (((of_u8 v) << 8) &&& (of_int 0x3F00))); mem.w <- true)
    else
      mem.t <- U16.(mem.t &&& (of_int 0xFF00) ||| (of_u8 v)); mem.v <- mem.t; mem.w <- false

  let write_data mem v =
    write8 mem mem.v v; incvram mem

  let write_scroll mem v =
    if mem.w then
      (mem.t <- U16.(mem.t &&& (of_int 0xFFE0) ||| ((of_u8 v) >> 3)); mem.x <- U8.(v &&& (of_int 0x7)); mem.w <- true)
    else
      mem.t <- U16.(mem.t &&& (of_int 0x0C1F) ||| ((of_u8 v) << 12) ||| (((of_u8 v) >> 3) << 5)); mem.w <- false
end

type t = {
  mem: Mem.t;
}

let init () =
  { mem=Mem.init () }

let read_register ppu addr =
  let open Mem in
  match U16.(to_int (addr &&& (of_int 0x7))) with
  | 0x2 -> read_status ppu.mem
  | 0x4 -> read_oam ppu.mem
  | 0x7 -> read_data ppu.mem
  | 0x0 | 0x1 | 0x3 | 0x5 | 0x6 -> U8.zero
  | _ -> assert false

let write_register ppu addr v =
  let open Mem in
  match U16.(to_int (addr &&& (of_int 0x7))) with
  | 0x0 -> write_ppuctrl ppu.mem v
  | 0x1 -> ppu.mem.mask <- v
  | 0x2 -> () (* PPUSTATUS read only *)
  | 0x3 -> ppu.mem.oamaddr <- v
  | 0x4 -> write_oam ppu.mem v
  | 0x5 -> write_scroll ppu.mem v
  | 0x6 -> write_addr ppu.mem v
  | 0x7 -> write_data ppu.mem v
  | _ -> assert false
