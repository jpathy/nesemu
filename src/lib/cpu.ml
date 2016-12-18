open Integer

module Pflags = struct
  type t =
    | S [@value 0x80]
    | V [@value 0x40]
    | B [@value 0x10]
    | D [@value 0x08]
    | I [@value 0x04]
    | Z [@value 0x02]
    | C [@value 0x01]
    [@@deriving enum]

  let show = function
    | S -> "S" | V -> "V" | B -> "B" | D -> "D"
    | I -> "I" | Z -> "Z" | C -> "C"

  let set p f = U8.(p ||| of_int (to_enum f))
  let clear p f = U8.(p &&& ~~~(of_int (to_enum f)))
  let test p f = U8.(p &&& of_int (to_enum f) <=> zero)
end

module IRQ_Delay = struct
  type t =
    | None
    | CLI
    | SEI
end

type t = {
  mutable a: u8;
  mutable x: u8;
  mutable y: u8;
  mutable s: u8;
  mutable p: u8;
  mutable pc: u16;

  (* quirks *)
  mutable irq_delay: IRQ_Delay.t;
}

let pp fmt cpu =
  let rec show_flags p mask s =
    let str_f i =
      match Pflags.of_enum i with
      | Some f -> Pflags.show f
      | _ -> ""
    in
    let np = U8.(p &&& ~~~mask) in
    let nmask = U8.(mask >> 1) in
    match () with
    | _ when U8.(p === zero) -> s
    | _ when U8.((p &&& mask) <=> zero) -> show_flags np nmask (s ^ (str_f (U8.to_int mask)))
    | _ -> show_flags np nmask s
  in
  Format.fprintf fmt "A:%02X X:%02X Y:%02X P:%02X(%s) SP:%02X PC:%04X"
  (U8.to_int cpu.a) (U8.to_int cpu.x) (U8.to_int cpu.y) (U8.to_int cpu.p) (show_flags cpu.p (U8.of_int 0x80) "") (U8.to_int cpu.s) (U16.to_int cpu.pc)

let reset_vector = U16.of_int 0xFFFC
let nmi_vector = U16.of_int 0xFFFA
let irq_vector = U16.of_int 0xFFFE

let reset cpu mem =
  cpu.p <- Pflags.set cpu.p Pflags.I;
  cpu.pc <- U16.(of_u8 (Mem.read8 mem reset_vector) ||| (of_u8 (Mem.read8 mem (reset_vector+one)) << 8));
  cpu.irq_delay <- IRQ_Delay.None

let init mem =
  let cpu = {a=U8.zero; x=U8.zero; y=U8.zero; s=U8.zero; p=U8.zero; pc=U16.zero; irq_delay=IRQ_Delay.None} in
  reset cpu mem; cpu

(* with help from 9front nes cpu.c; https://emu-docs.org/CPU%2065xx/6502.txt
 * https://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
 * https://wiki.nesdev.com/w/index.php/CPU_interrupts *)
let step cpu mem =
  let module P = Pflags in
  let module I = Global.Interrupts in

  let pset f = cpu.p <- P.set cpu.p f
  in
  let pclear f = cpu.p <- P.clear cpu.p f
  in
  let memrd8 addr = Mem.read8 mem addr
  in
  let memwr8 addr v = Mem.write8 mem addr v
  in
  let fetch8 () =
    let v = memrd8 cpu.pc in
    cpu.pc <- U16.(cpu.pc+one); v
  in
  let fetch16 () = let v = fetch8 () in U16.(of_u8 v ||| (of_u8 (fetch8()) << 8))
  in
  let push8 v =
    memwr8 U16.(of_int 0x100 ||| (of_u8 cpu.s)) v;
    cpu.s <- U8.(cpu.s-one)
  in
  let push16 v16 =
    push8 U16.(to_u8 (v16>>8)); push8 (U16.to_u8 v16)
  in
  let pop8 () =
    cpu.s <- U8.(cpu.s+one);
    memrd8 U16.(of_int 0x100 ||| U8.(to_u16 cpu.s))
  in
  let pop16 () = let v = pop8() in U16.(of_u8 v ||| (of_u8 (pop8()) << 8))
  in
  (* operands corresponding addressing mode *)
  let imm = fetch8
  in
  let imm16 = fetch16
  in
  let azp () = fetch8() |> U8.to_u16
  in
  let zp () = azp() |> memrd8
  in
  let azpX () = U8.(fetch8() + cpu.x |> to_u16)
  in
  let zpX () = azpX() |> memrd8
  in
  let azpY () = U8.(fetch8() + cpu.y |> to_u16)
  in
  let zpY () = azpY() |> memrd8
  in
  let abso () = fetch16() |> memrd8
  in
  let aabsX () = U16.(fetch16() + (of_u8 cpu.x))
  in
  let absX () =
    let a = aabsX() in
    (memrd8 a, U8.(of_u16 a <<< cpu.x))
  in
  let aabsY () = U16.(fetch16() + (of_u8 cpu.y))
  in
  let absY () =
    let a = aabsY() in
    (memrd8 a, U8.(of_u16 a <<< cpu.y))
  in
  let aindX () =
    let a = U8.(fetch8() + cpu.x) in
    let lo = memrd8 (U16.of_u8 a) in
    let hi = memrd8 (U16.of_u8 U8.(a+one)) in
    U16.(((of_u8 hi) << 8) ||| of_u8 lo)
  in
  let indX () = memrd8 (aindX())
  in
  let aindY () =
    let a = fetch8() in
    let lo = memrd8 (U16.of_u8 a) in
    let hi = memrd8 (U16.of_u8 U8.(a+one)) in
    U16.((((of_u8 hi) << 8) ||| of_u8 lo) + of_u8 cpu.y)
  in
  let indY () = let a = aindY() in (memrd8 a, U8.(of_u16 a <<< cpu.y))
  in
  let branch rel =
    let npc = U16.(cpu.pc + (of_s8 (U8.to_s8 rel))) in
    let b = U16.((npc ^^^ cpu.pc) >> 8 <=> zero) in
    cpu.pc <- npc; b
  in
  let bit v =
    P.S |> if P.test v P.S then pset else pclear;
    P.V |> if P.test v P.V then pset else pclear;
    P.Z |> if U8.(cpu.a &&& v == zero) then pset else pclear
  in
  let sz v =
    P.S |> if P.test v P.S then pset else pclear;
    P.Z |> if U8.(v === zero) then pset else pclear
  in
  let adc v =
    let t = ref U16.((of_u8 v) + (of_u8 cpu.a) + (of_u8 U8.(cpu.p &&& one))) in
    (* NES 6502 doesn't implement decimal mode --
    if P.test cpu.p P.D then
      begin
        let m = U8.of_int 0xf in
        if U8.(((cpu.a &&& m) + (v &&& m) + (cpu.p &&& (of_int 0x1))) >>> of_int 0x9) then
          t := U16.(!t + (of_int 0x6));
        let t8 = U8.of_u16 !t in
        P.S |> if P.test t8 P.S then pset else pclear;
        P.V |> if U8.((cpu.a ^^^ v) &&& (cpu.a ^^^ t8) &&& (of_int 0x80) <=> zero) then pset else pclear;
        if U16.(!t >>> (of_int 0x99)) then t := U16.(!t + (of_int 0x60));
        P.C |> if U16.(!t >>> (of_int 0x99)) then pset else pclear
      end
    else *)
      begin
        let t8 = U8.of_u16 !t in
        sz t8;
        P.V |> if U8.(~~~(cpu.a ^^^ v) &&& (cpu.a ^^^ t8) &&& (of_int 0x80) <=> zero) then pset else pclear;
        P.C |> if U16.(!t >>> (of_int 0xFF)) then pset else pclear
      end
    ; cpu.a <- U8.of_u16 !t
  in
  let sbc v = adc U8.(~~~v)
  in
  let asl a =
    let v = ref (memrd8 a) in
    P.C |> if P.test !v P.S then pset else pclear;
    v := U8.(!v << 1);
    sz !v;
    memwr8 a !v; !v
  in
  let lsr8 a =
    let v = ref (memrd8 a) in
    pclear P.C;
    cpu.p <- U8.(cpu.p ||| (!v &&& one)); (* set/clear carry *)
    v := U8.(!v >> 1);
    sz !v;
    memwr8 a !v; !v
  in
  let rol a =
    let v = ref (memrd8 a) in
    let is_carry = P.test cpu.p P.C in
    P.C |> if P.test !v P.S then pset else pclear;
    v := U8.(!v << 1 ||| (of_int (if is_carry then 0x1 else 0x0)));
    sz !v;
    memwr8 a !v; !v
  in
  let ror a =
    let v = ref (memrd8 a) in
    let is_carry = P.test cpu.p P.C in
    P.C |> if P.test !v P.C then pset else pclear;
    v := U8.(!v >> 1 ||| (of_int (if is_carry then 0x80 else 0x0)));
    sz !v;
    memwr8 a !v; !v
  in
  let inc a =
    let v = U8.(memrd8 a + one) in
    sz v;
    memwr8 a v; v
  in
  let dec a =
    let v = U8.(memrd8 a - one) in
    sz v;
    memwr8 a v; v
  in
  let cmp x y =
    P.Z |> if U8.(x === y) then pset else pclear;
    P.C |> if U8.(x >>>= y) then pset else pclear;
    P.S |> if U8.(P.test (x-y) P.S) then pset else pclear
  in

  let intr_dispatch i =
    push16 cpu.pc;
    (match i with
    | `NMI | `IRQ -> push8 U8.(P.clear cpu.p P.B ||| (of_int 0x20)) (* bit 5 is set *)
    | `BRK -> push8 U8.(P.set cpu.p P.B ||| (of_int 0x20))
    );
    let ivec = if !I.nmi then nmi_vector else irq_vector in
    cpu.pc <- U16.(of_u8 (memrd8 ivec) ||| (of_u8 (memrd8 (ivec+one)) << 8));
    pset P.I
  in

  let exec () =
    let op = fetch8() in
    let cycles = ref Instructions.cycle_table.(U8.to_int op) in
    let page_crossed = ref false in
    begin
    match Char.unsafe_chr @@ U8.to_int op with
    | '\x00' -> cpu.pc <- U16.(cpu.pc+one); intr_dispatch `BRK
    | '\x01' -> cpu.a <- U8.(cpu.a ||| indX()); sz cpu.a
    | '\x05' -> cpu.a <- U8.(cpu.a ||| zp()); sz cpu.a
    | '\x06' -> asl (azp()) |> ignore
    | '\x08' -> push8 U8.(cpu.p ||| (of_int 0x30))
    | '\x09' -> cpu.a <- U8.(cpu.a ||| imm()); sz cpu.a
    | '\x0A' -> (* ASL cpu.a *)
        P.C |> if P.test cpu.a P.S then pset else pclear;
        cpu.a <- U8.(cpu.a << 1);
        sz cpu.a
    | '\x0D' -> cpu.a <- U8.(cpu.a ||| abso()); sz cpu.a
    | '\x0E' -> asl (imm16()) |> ignore
    | '\x10' -> let v = imm() in if not (P.test cpu.p P.S) then begin cycles := !cycles+1; page_crossed := branch v end
    | '\x11' -> let (v, b) = indY() in cpu.a <- U8.(cpu.a ||| v); sz cpu.a; page_crossed := b
    | '\x15' -> cpu.a <- U8.(cpu.a ||| zpX()); sz cpu.a
    | '\x16' -> asl (azpX()) |> ignore
    | '\x18' -> pclear P.C
    | '\x19' -> let (v, b) = absY() in cpu.a <- U8.(cpu.a ||| v); sz cpu.a; page_crossed := b
    | '\x1D' -> let (v, b) = absX() in cpu.a <- U8.(cpu.a ||| v); sz cpu.a; page_crossed := b
    | '\x1E' -> asl (aabsX()) |> ignore
    | '\x20' -> push16 U16.(cpu.pc+one); cpu.pc <- imm16()
    | '\x21' -> cpu.a <- U8.(cpu.a &&& indX()); sz cpu.a
    | '\x24' -> bit (zp())
    | '\x25' -> cpu.a <- U8.(cpu.a &&& zp()); sz cpu.a
    | '\x26' -> rol (azp()) |> ignore
    | '\x28' ->
        let oi = P.test cpu.p P.I in
        cpu.p <- U8.(pop8() &&& (of_int 0xCF));
        begin
        match oi, (P.test cpu.p P.I) with
        | (true, false) -> cpu.irq_delay <- IRQ_Delay.CLI
        | (false, true) -> cpu.irq_delay <- IRQ_Delay.SEI
        | _ -> ()
        end
    | '\x29' -> cpu.a <- U8.(cpu.a &&& imm()); sz cpu.a
    | '\x2A' -> (* ROL A *)
        let is_carry = P.test cpu.p P.C in
        P.C |> if P.test cpu.a P.S then pset else pclear;
        cpu.a <- U8.(cpu.a << 1 ||| (of_int (if is_carry then 0x1 else 0x0)));
        sz cpu.a
    | '\x2C' -> bit (abso())
    | '\x2D' -> cpu.a <- U8.(cpu.a &&& (abso())); sz cpu.a
    | '\x2E' -> rol (fetch16()) |> ignore
    | '\x30' -> let v = imm() in if (P.test cpu.p P.S) then begin cycles := !cycles+1; page_crossed := branch v end
    | '\x31' -> let (v, b) = indY() in cpu.a <- U8.(cpu.a &&& v); sz cpu.a; page_crossed := b
    | '\x35' -> cpu.a <- U8.(cpu.a &&& zpX()); sz cpu.a
    | '\x36' -> rol (azpX()) |> ignore
    | '\x38' -> pset P.C
    | '\x39' -> let (v, b) = absY() in cpu.a <- U8.(cpu.a &&& v); sz cpu.a; page_crossed := b
    | '\x3D' -> let (v, b) = absX() in cpu.a <- U8.(cpu.a &&& v); sz cpu.a; page_crossed := b
    | '\x3E' -> rol (aabsX()) |> ignore
    | '\x40' -> cpu.p <- U8.(pop8() &&& (of_int 0xCF)); cpu.pc <- pop16()
    | '\x41' -> cpu.a <- U8.(cpu.a ^^^ indX()); sz cpu.a
    | '\x45' -> cpu.a <- U8.(cpu.a ^^^ zp()); sz cpu.a
    | '\x46' -> lsr8 (azp()) |> ignore
    | '\x48' -> push8 cpu.a
    | '\x49' -> cpu.a <- U8.(cpu.a ^^^ imm()); sz cpu.a
    | '\x4A' -> (* LSR A *)
        pclear P.C;
        cpu.p <- U8.(cpu.p ||| (cpu.a &&& one)); (* set/clear carry *)
        cpu.a <- U8.(cpu.a >> 1);
        sz cpu.a
    | '\x4C' -> cpu.pc <- imm16()
    | '\x4D' -> cpu.a <- U8.(cpu.a ^^^ abso()); sz cpu.a
    | '\x4E' -> lsr8 (fetch16()) |> ignore
    | '\x50' -> let v = imm() in if not (P.test cpu.p P.V) then begin cycles := !cycles+1; page_crossed := branch v end
    | '\x51' -> let (v, b) = indY() in cpu.a <- U8.(cpu.a ^^^ v); sz cpu.a; page_crossed := b
    | '\x56' -> lsr8 (azpX()) |> ignore
    | '\x58' ->
        let oi = P.test cpu.p P.I in
        if oi then cpu.irq_delay <- IRQ_Delay.SEI; pclear P.I (* I : 0 -> 1 *)
    | '\x55' -> cpu.a <- U8.(cpu.a ^^^ zpX()); sz cpu.a
    | '\x59' -> let (v, b) = absY() in cpu.a <- U8.(cpu.a ^^^ v); sz cpu.a; page_crossed := b
    | '\x5D' -> let (v, b) = absX() in cpu.a <- U8.(cpu.a ^^^ v); sz cpu.a; page_crossed := b
    | '\x5E' -> lsr8 (aabsX()) |> ignore
    | '\x60' -> cpu.pc <- U16.(pop16() + one)
    | '\x61' -> adc (indX())
    | '\x65' -> adc (zp())
    | '\x66' -> ror (azp()) |> ignore
    | '\x68' -> cpu.a <- pop8(); sz cpu.a
    | '\x69' -> adc (imm())
    | '\x6A' -> (* ROR A *)
        let is_carry = P.test cpu.p P.C in
        P.C |> if P.test cpu.a P.C then pset else pclear;
        cpu.a <- U8.(cpu.a >> 1 ||| (of_int (if is_carry then 0x80 else 0x0)));
        sz cpu.a
    | '\x6C' -> (* JMP ind *)
        let a = imm16() in
        let v = memrd8 a in
        cpu.pc <- U16.(of_u8 v |||
        (of_u8
        (memrd8 ((a+one) &&& (of_int 0xFF) ||| (a &&& (of_int 0xFF00)))) << 8)) (* CPU BUG page wrap around *)
    | '\x6D' -> adc (abso())
    | '\x6E' -> ror (imm16()) |> ignore
    | '\x70' -> let v = imm() in if (P.test cpu.p P.V) then begin cycles := !cycles+1; page_crossed := branch v end
    | '\x71' -> let (v, b) = indY() in adc v; page_crossed := b
    | '\x75' -> adc (zpX())
    | '\x76' -> ror (azpX()) |> ignore
    | '\x78' -> (* SEI *)
        let oi = P.test cpu.p P.I in
        if not oi then cpu.irq_delay <- IRQ_Delay.SEI; pset P.I (* I : 0 -> 1 *)
    | '\x79' -> let (v, b) = absY() in adc v; page_crossed := b
    | '\x7D' -> let (v, b) = absX() in adc v; page_crossed := b
    | '\x7E' -> ror (aabsX()) |> ignore
    | '\x81' -> memwr8 (aindX()) cpu.a
    | '\x84' -> memwr8 (azp()) cpu.y
    | '\x85' -> memwr8 (azp()) cpu.a
    | '\x86' -> memwr8 (azp()) cpu.x
    | '\x88' -> cpu.y <- U8.(cpu.y-one); sz cpu.y
    | '\x8A' -> cpu.a <- cpu.x; sz cpu.a
    | '\x8C' -> memwr8 (imm16()) cpu.y
    | '\x8D' -> memwr8 (imm16()) cpu.a
    | '\x8E' -> memwr8 (imm16()) cpu.x
    | '\x90' -> let v = imm() in if not (P.test cpu.p P.C) then begin cycles := !cycles+1; page_crossed := branch v end
    | '\x91' -> memwr8 (aindY()) cpu.a
    | '\x94' -> memwr8 (azpX()) cpu.y
    | '\x95' -> memwr8 (azpX()) cpu.a
    | '\x96' -> memwr8 (azpY()) cpu.x
    | '\x98' -> cpu.a <- cpu.y; sz cpu.a
    | '\x99' -> memwr8 (aabsY()) cpu.a
    | '\x9A' -> cpu.s <- cpu.x
    | '\x9D' -> memwr8 (aabsX()) cpu.a
    | '\xA0' -> cpu.y <- imm(); sz cpu.y
    | '\xA1' -> cpu.a <- indX(); sz cpu.a
    | '\xA2' -> cpu.x <- imm(); sz cpu.x
    | '\xA4' -> cpu.y <- zp(); sz cpu.y
    | '\xA5' -> cpu.a <- zp(); sz cpu.a
    | '\xA6' -> cpu.x <- zp(); sz cpu.x
    | '\xA8' -> cpu.y <- cpu.a; sz cpu.y
    | '\xA9' -> cpu.a <- imm(); sz cpu.a
    | '\xAA' -> cpu.x <- cpu.a; sz cpu.x
    | '\xAC' -> cpu.y <- abso(); sz cpu.y
    | '\xAD' -> cpu.a <- abso(); sz cpu.a
    | '\xAE' -> cpu.x <- abso(); sz cpu.x
    | '\xB0' -> let v = imm() in if (P.test cpu.p P.C) then begin cycles := !cycles+1; page_crossed := branch v end
    | '\xB1' -> let (v, _) = indY() in cpu.a <- v; sz cpu.a
    | '\xB4' -> cpu.y <- zpX(); sz cpu.y
    | '\xB5' -> cpu.a <- zpX(); sz cpu.a
    | '\xB6' -> cpu.x <- zpY(); sz cpu.x
    | '\xB8' -> pclear P.V
    | '\xB9' -> let (v, b) = absY() in cpu.a <- v; sz cpu.a; page_crossed := b
    | '\xBA' -> cpu.x <- cpu.s; sz cpu.x
    | '\xBC' -> let (v, b) = absX() in cpu.y <- v; sz cpu.y; page_crossed := b
    | '\xBD' -> let (v, b) = absX() in cpu.a <- v; sz cpu.a; page_crossed := b
    | '\xBE' -> let (v, b) = absY() in cpu.x <- v; sz cpu.x; page_crossed := b
    | '\xC1' -> cmp cpu.a (indX())
    | '\xC5' -> cmp cpu.a (zp())
    | '\xC9' -> cmp cpu.a (imm())
    | '\xCD' -> cmp cpu.a (abso())
    | '\xD0' -> let v = imm() in if not (P.test cpu.p P.Z) then begin cycles := !cycles+1; page_crossed := branch v end
    | '\xD1' -> let (v, b) = indY() in cmp cpu.a v; page_crossed := b
    | '\xD5' -> cmp cpu.a (zpX())
    | '\xD8' -> pclear P.D
    | '\xD9' -> let (v, b) = absY() in cmp cpu.a v; page_crossed := b
    | '\xDD' -> let (v, b) = absX() in cmp cpu.a v; page_crossed := b
    | '\xC0' -> cmp cpu.y (imm())
    | '\xC4' -> cmp cpu.y (zp())
    | '\xC6' -> dec (azp()) |> ignore
    | '\xC8' -> cpu.y <- U8.(cpu.y+one); sz cpu.y
    | '\xCA' -> cpu.x <- U8.(cpu.x-one); sz cpu.x
    | '\xCC' -> cmp cpu.y (abso())
    | '\xCE' -> dec (imm16()) |> ignore
    | '\xD6' -> dec (azpX()) |> ignore
    | '\xDE' -> dec (aabsX()) |> ignore
    | '\xE0' -> cmp cpu.x (imm())
    | '\xE1' -> sbc (indX())
    | '\xE4' -> cmp cpu.x (zp())
    | '\xE5' -> sbc (zp())
    | '\xE6' -> inc (azp()) |> ignore
    | '\xE8' -> cpu.x <- U8.(cpu.x+one); sz cpu.x
    | '\xE9' -> sbc (imm())
    | '\xEA' -> ()
    | '\xEC' -> cmp cpu.x (abso())
    | '\xED' -> sbc (abso())
    | '\xEE' -> inc (imm16()) |> ignore
    | '\xF0' -> let v = imm() in if (P.test cpu.p P.Z) then begin cycles := !cycles+1; page_crossed := branch v end
    | '\xF1' -> let (a, b) = indY() in sbc a; page_crossed := b
    | '\xF5' -> sbc (zpX())
    | '\xF6' -> inc (azpX()) |> ignore
    | '\xF8' -> pset P.D
    | '\xF9' -> let (v, b) = absY() in sbc v; page_crossed := b
    | '\xFD' -> let (v, b) = absX() in sbc v; page_crossed := b
    | '\xFE' -> inc (aabsX()) |> ignore

    (* unofficial opcodes *)
    | '\x02' | '\x12' | '\x22' | '\x32' | '\x42' | '\x52' | '\x62' | '\x72' | '\x92' | '\xB2' | '\xD2' | '\xF2' -> () (* KIL *)
    | '\x0B' | '\x2B' -> cpu.a <- U8.(cpu.a &&& imm()); sz cpu.a; P.C |> if P.test cpu.p P.S then pset else pclear (* ANC #i *)
    (* NOP *)
    | '\x1A' | '\x3A' | '\x5A' | '\x7A' | '\xDA' | '\xFA' -> () (* NOP *)
    | '\x80' | '\x82' | '\x89' | '\xC2' | '\xE2' -> ignore (imm()) (* NOP immediate *)
    | '\x04' | '\x44' | '\x64' -> ignore (zp())
    | '\x0C' -> ignore (abso())
    | '\x1C' | '\x3C' | '\x5C' | '\x7C' | '\xDC' | '\xFC' -> ignore (absX())
    | '\x14' | '\x34' | '\x54' | '\x74' | '\xD4' | '\xF4' -> ignore (zpX())

    | '\x4B' -> (* ALR #i = AND #i, LSR A *)
        cpu.a <- U8.(cpu.a &&& imm());
        pclear P.C;
        cpu.p <- U8.(cpu.p ||| (cpu.a &&& one)); (* set/clear carry *)
        cpu.a <- U8.(cpu.a >> 1);
        sz cpu.a
    | '\x6B' -> (* ARR #i = AND #i, ROR A *)
        cpu.a <- U8.(cpu.a &&& imm());
        let is_carry = P.test cpu.p P.C in
        cpu.a <- U8.(cpu.a >> 1 ||| (of_int (if is_carry then 0x80 else 0x0)));
        P.C |> if P.test cpu.a P.V then pset else pclear;
        P.V |> if P.test U8.(cpu.a ^^^ (cpu.a<<1)) P.V then pset else pclear;
        sz cpu.a
    | '\xCB' -> (* AXS #i *)
        let v1 = U8.(cpu.a &&& cpu.x) in
        let v2 = imm() in
        P.C |> if U8.(v1 >>>= v2) then pset else pclear;
        cpu.x <- U8.(v1-v2); sz cpu.x;
    (* SLO *)
    | '\x03' -> cpu.a <- U8.(cpu.a ||| asl (aindX())); sz cpu.a
    | '\x07' -> cpu.a <- U8.(cpu.a ||| asl (azp())); sz cpu.a
    | '\x0F' -> cpu.a <- U8.(cpu.a ||| asl (imm16())); sz cpu.a
    | '\x13' -> cpu.a <- U8.(cpu.a ||| asl (aindY())); sz cpu.a
    | '\x17' -> cpu.a <- U8.(cpu.a ||| asl (azpX())); sz cpu.a
    | '\x1B' -> cpu.a <- U8.(cpu.a ||| asl (aabsY())); sz cpu.a
    | '\x1F' -> cpu.a <- U8.(cpu.a ||| asl (aabsX())); sz cpu.a
    (* RLA *)
    | '\x23' -> cpu.a <- U8.(cpu.a &&& rol (aindX())); sz cpu.a
    | '\x27' -> cpu.a <- U8.(cpu.a &&& rol (azp())); sz cpu.a
    | '\x2F' -> cpu.a <- U8.(cpu.a &&& rol (imm16())); sz cpu.a
    | '\x33' -> cpu.a <- U8.(cpu.a &&& rol (aindY())); sz cpu.a
    | '\x37' -> cpu.a <- U8.(cpu.a &&& rol (azpX())); sz cpu.a
    | '\x3B' -> cpu.a <- U8.(cpu.a &&& rol (aabsY())); sz cpu.a
    | '\x3F' -> cpu.a <- U8.(cpu.a &&& rol (aabsX())); sz cpu.a
    (* SRE *)
    | '\x43' -> cpu.a <- U8.(cpu.a ^^^ lsr8 (aindX())); sz cpu.a
    | '\x47' -> cpu.a <- U8.(cpu.a ^^^ lsr8 (azp())); sz cpu.a
    | '\x4F' -> cpu.a <- U8.(cpu.a ^^^ lsr8 (imm16())); sz cpu.a
    | '\x53' -> cpu.a <- U8.(cpu.a ^^^ lsr8 (aindY())); sz cpu.a
    | '\x57' -> cpu.a <- U8.(cpu.a ^^^ lsr8 (azpX())); sz cpu.a
    | '\x5B' -> cpu.a <- U8.(cpu.a ^^^ lsr8 (aabsY())); sz cpu.a
    | '\x5F' -> cpu.a <- U8.(cpu.a ^^^ lsr8 (aabsX())); sz cpu.a
    (* RRA *)
    | '\x63' -> ror (aindX()) |> adc
    | '\x67' -> ror (azp()) |> adc
    | '\x6F' -> ror (imm16()) |> adc
    | '\x73' -> ror (aindY()) |> adc
    | '\x77' -> ror (azpX()) |> adc
    | '\x7B' -> ror (aabsY()) |> adc
    | '\x7F' -> ror (aabsX()) |> adc
    (* SAX *)
    | '\x83' -> memwr8 (aindX()) U8.(cpu.a &&& cpu.x)
    | '\x87' -> memwr8 (azp()) U8.(cpu.a &&& cpu.x)
    | '\x8F' -> memwr8 (imm16()) U8.(cpu.a &&& cpu.x)
    | '\x97' -> memwr8 (azpY()) U8.(cpu.a &&& cpu.x)

    | '\x8B' -> (* XAA *)
        let magic = U8.of_int 0xFF in
        cpu.a <- U8.((cpu.a ||| magic) &&& cpu.x &&& imm()); sz cpu.a
    | '\x93' -> (* AHX #imm *)
        let a = imm() in
        let l = memrd8 (U16.of_u8 a) in let h = memrd8 U8.(to_u16 (a+one)) in
        let v = U8.(cpu.a &&& cpu.x &&& (h+one)) in
        let la = U8.(l+cpu.y) in
        let ha = if U8.(la <<< l) then v else h in
        memwr8 U16.(((of_u8 ha)<<8) ||| (of_u8 la)) v
    | '\x9F' -> (* AHX #imm16 *)
        let l = imm() in let h = imm() in
        let v = U8.(cpu.a &&& cpu.x &&& (h+one)) in
        let la = U8.(l+cpu.y) in
        let ha = if U8.(la <<< l) then v else h in
        memwr8 U16.(((of_u8 ha)<<8) ||| (of_u8 la)) v
    | '\x9B' -> (* TAS *)
        cpu.s <- U8.(cpu.x &&& cpu.a);
        let l = imm() in let h = imm() in
        let v = U8.(cpu.s &&& (h+one)) in
        let la = U8.(l+cpu.y) in
        let ha = if U8.(la <<< l) then v else h in
        memwr8 U16.(((of_u8 ha)<<8) ||| (of_u8 la)) v
    | '\x9C' -> (* SHY *)
        let l = imm() in let h = imm() in
        let v = U8.(cpu.y &&& (h+one)) in
        let la = U8.(l+cpu.x) in
        let ha = if U8.(la <<< l) then v else h in
        memwr8 U16.(((of_u8 ha)<<8) ||| (of_u8 la)) v
    | '\x9E' -> (* SHX *)
        let l = imm() in let h = imm() in
        let v = U8.(cpu.x &&& (h+one)) in
        let la = U8.(l+cpu.y) in
        let ha = if U8.(la <<< l) then v else h in
        memwr8 U16.(((of_u8 ha)<<8) ||| (of_u8 la)) v
    | '\xBB' -> (* LAS *)
        let (v, b) = absY() in
        let v = U8.(v &&& cpu.s) in
        cpu.s <- v; cpu.a <- v; cpu.x <- v; sz v; page_crossed := b
    (* LAX *)
    | '\xA3' -> cpu.a <- indX(); cpu.x <- cpu.a; sz cpu.x
    | '\xA7' -> cpu.a <- zp(); cpu.x <- cpu.a; sz cpu.x
    | '\xAB' -> cpu.a <- imm(); cpu.x <- cpu.a; sz cpu.x
    | '\xAF' -> cpu.a <- abso(); cpu.x <- cpu.a; sz cpu.x
    | '\xB3' -> let (v, b) = indY() in cpu.a <- v; cpu.x <- cpu.a; sz cpu.x; page_crossed := b
    | '\xB7' -> cpu.a <- zpY(); cpu.x <- cpu.a; sz cpu.x
    | '\xBF' -> let (v, b) = absY() in cpu.a <- v; cpu.x <- cpu.a; sz cpu.x; page_crossed := b
    (* DCP *)
    | '\xC3' -> dec (aindX()) |> cmp cpu.a
    | '\xC7' -> dec (azp()) |> cmp cpu.a
    | '\xCF' -> dec (imm16()) |> cmp cpu.a
    | '\xD3' -> dec (aindY()) |> cmp cpu.a
    | '\xD7' -> dec (azpX()) |> cmp cpu.a
    | '\xDB' -> dec (aabsY()) |> cmp cpu.a
    | '\xDF' -> dec (aabsX()) |> cmp cpu.a
    (* ISC *)
    | '\xE3' -> inc (aindX()) |> sbc
    | '\xE7' -> inc (azp()) |> sbc
    | '\xEF' -> inc (imm16()) |> sbc
    | '\xF3' -> inc (aindY()) |> sbc
    | '\xF7' -> inc (azpX()) |> sbc
    | '\xFB' -> inc (aabsY()) |> sbc
    | '\xFF' -> inc (aabsX()) |> sbc

    | '\xEB' -> sbc (imm())
    end
    ;
    cycles := !cycles +
    (if !page_crossed then Instructions.page_cross_penalty.(U8.to_int op) else 0);
    !cycles
  in

  (* execution starts here *)
  (* TODO: conditional enable debugging *)
  Format.(
    pp std_formatter cpu;
    pp_print_string std_formatter "\t";
    Instructions.pp std_formatter (cpu.pc, Mem.read8 mem);
    pp_print_newline std_formatter ()
  );

  if !I.nmi then
    begin
      intr_dispatch `NMI;
      I.nmi := false;
      7
    end
  else if !I.irq then
    begin
      match cpu.irq_delay with
      | IRQ_Delay.None -> if not (P.test cpu.p P.I) then (intr_dispatch `IRQ; 7) else exec ()
      | IRQ_Delay.CLI -> cpu.irq_delay <- IRQ_Delay.None; exec () (* I : 1 -> 0 => execute next instruction *)
      | IRQ_Delay.SEI -> cpu.irq_delay <- IRQ_Delay.None; intr_dispatch `IRQ; 7 (* 0 -> 1 => dispatch irq even if I is set *)
    end
  else
    exec()
