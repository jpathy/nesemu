open Core_kernel

module Mirroring = struct
  type t =
  | Vertical
  | Horizontal
  | FourScreen
  | SingleA
  | SingleB
end

module Header = struct
  type t = Cstruct.t

  [%%cstruct
  type ines_header = {
    magic: uint32_t;
    prgcount: uint8_t;
    chrcount: uint8_t;
    flag_6: uint8_t;
    flag_7: uint8_t;
    prgramsz: uint8_t;
    flag_9: uint8_t;
    flag_10: uint8_t;
    rest: uint8_t[@len 5];
  } [@@little_endian]]

  let size = sizeof_ines_header

  let of_bigarray ?(off=0) buf =
    let hdr = Cstruct.of_bigarray ~off:off ~len:size buf in
    if get_ines_header_magic hdr <> Int32.of_int 0x1a53454e (* "NES" *)
    || get_ines_header_prgcount hdr = 0
    || copy_ines_header_rest hdr <> "\x00\x00\x00\x00\x00" then
      invalid_arg "iNES Header not recognized"
    else if get_ines_header_flag_7 hdr land 0x3 <> 0 then
      invalid_arg "ROM not supported"
    else
      hdr

  let get_prgsize hdr = get_ines_header_prgcount hdr * 16 * 1024

  let get_chrsize hdr = get_ines_header_chrcount hdr * 8 * 1024

  let has_trainer hdr = get_ines_header_flag_6 hdr land 0x4 <> 0

  let get_mirroring hdr =
    let f6 = get_ines_header_flag_6 hdr in
    if f6 land 0x80 <> 0 then
      Mirroring.FourScreen
    else
      if f6 land 0x1 = 0 then Mirroring.Horizontal else Mirroring.Vertical

  let get_mapper hdr = get_ines_header_flag_6 hdr lsr 4 land 0xF lor (get_ines_header_flag_7 hdr land 0xF0)

  let has_bat_sram hdr = get_ines_header_flag_6 hdr land 0x2 <> 0
end

type t = {
  bat_sram: bool;
  mapper_id: int;
  mirror: Mirroring.t;
  trainer: Bigstring.t;
  prgram: Bigstring.t;
  prgrom: Bigstring.t;
  chr: Bigstring.t;
}

let of_bigarray ?(off=0) buf =
  let m_off = ref off in
  let hdr = Header.of_bigarray ~off:!m_off buf in
  let tr_len = if Header.has_trainer hdr then 512 else 0 in
  if Bigstring.length buf < Header.size+tr_len+Header.get_prgsize hdr+Header.get_chrsize hdr then
    invalid_arg "ROM corrupted"
  else
    let mirror = Header.get_mirroring hdr in
    let bat_sram = Header.has_bat_sram hdr in
    let mapper_id = Header.get_mapper hdr in
    let sub_copy s1 off len =
      let s2 = Bigstring.create len in
      Bigstring.blit s1 off s2 0 len; s2
    in
    let trainer = m_off := !m_off+Header.size; sub_copy buf !m_off tr_len
    in
    let prgrom = m_off := !m_off+tr_len; sub_copy buf !m_off (Header.get_prgsize hdr)
    in
    let chr =
      m_off := !m_off+Bigstring.length prgrom;
      let chrsz = Header.get_chrsize hdr in
      if chrsz <> 0 then sub_copy buf !m_off chrsz else Bigstring.create (8*1024)
    in
    let prgram = Bigstring.create 0x2000 in
    { bat_sram; mapper_id; mirror; trainer; prgram; prgrom; chr }
