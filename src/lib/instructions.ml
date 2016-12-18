open Integer

(* mostly taken from blitzcode/neskell *)
type mnemonic =
  | ADC | AND | ASL | BCC | BCS | BEQ
  | BIT | BMI | BNE | BPL | BRK | BVC
  | BVS | CLC | CLD | CLI | CLV | CMP
  | CPX | CPY | DEC | DEX | DEY | EOR
  | INC | INX | INY | JMP | JSR | LDA
  | LDX | LDY | LSR | NOP | ORA | PHA
  | PHP | PLA | PLP | ROL | ROR | RTI
  | RTS | SBC | SEC | SED | SEI | STA
  | STX | STY | TAX | TAY | TSX | TXA
  | TXS | TYA
  (* unofficial *)
  | KIL | LAX | SAX | DCP | ISC | RLA
  | RRA | SLO | SRE | ANC | ALR | ARR
  | XAA | AHX | TAS | SHX | SHY | LAS
  | AXS

let mnemonic_to_string = function
  | ADC -> "ADC" | AND -> "AND" | ASL -> "ASL" | BCC -> "BCC" | BCS -> "BCS" | BEQ -> "BEQ"
  | BIT -> "BIT" | BMI -> "BMI" | BNE -> "BNE" | BPL -> "BPL" | BRK -> "BRK" | BVC -> "BVC"
  | BVS -> "BVS" | CLC -> "CLC" | CLD -> "CLD" | CLI -> "CLI" | CLV -> "CLV" | CMP -> "CMP"
  | CPX -> "CPX" | CPY -> "CPY" | DEC -> "DEC" | DEX -> "DEX" | DEY -> "DEY" | EOR -> "EOR"
  | INC -> "INC" | INX -> "INX" | INY -> "INY" | JMP -> "JMP" | JSR -> "JSR" | LDA -> "LDA"
  | LDX -> "LDX" | LDY -> "LDY" | LSR -> "LSR" | NOP -> "NOP" | ORA -> "ORA" | PHA -> "PHA"
  | PHP -> "PHP" | PLA -> "PLA" | PLP -> "PLP" | ROL -> "ROL" | ROR -> "ROR" | RTI -> "RTI"
  | RTS -> "RTS" | SBC -> "SBC" | SEC -> "SEC" | SED -> "SED" | SEI -> "SEI" | STA -> "STA"
  | STX -> "STX" | STY -> "STY" | TAX -> "TAX" | TAY -> "TAY" | TSX -> "TSX" | TXA -> "TXA"
  | TXS -> "TXS" | TYA -> "TYA"
  (* unofficial *)
  | KIL -> "KIL" | LAX -> "LAX" | SAX -> "SAX" | DCP -> "DCP" | ISC -> "ISC" | RLA -> "RLA"
  | RRA -> "RRA" | SLO -> "SLO" | SRE -> "SRE" | ANC -> "ANC" | ALR -> "ALR" | ARR -> "ARR"
  | XAA -> "XAA" | AHX -> "AHX" | TAS -> "TAS" | SHX -> "SHX" | SHY -> "SHY" | LAS -> "LAS"
  | AXS -> "AXS"

type addressing =
  | Implied
  | Immediate
  | Accumulator
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | Relative
  | IndirectX
  | IndirectY
  | AbsoluteInd

let cycle_table = [|
  7; 6; 2; 8; 3; 3; 5; 5; 3; 2; 2; 2; 4; 4; 6; 6; (* 0x0X *)
  2; 5; 2; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; (* 0x1X *)
  6; 6; 2; 8; 3; 3; 5; 5; 4; 2; 2; 2; 4; 4; 6; 6; (* 0x2X *)
  2; 5; 2; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; (* 0x3X *)
  6; 6; 2; 8; 3; 3; 5; 5; 3; 2; 2; 2; 3; 4; 6; 6; (* 0x4X *)
  2; 5; 2; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; (* 0x5X *)
  6; 6; 2; 8; 3; 3; 5; 5; 4; 2; 2; 2; 5; 4; 6; 6; (* 0x6X *)
  2; 5; 2; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; (* 0x7X *)
  2; 6; 2; 6; 3; 3; 3; 3; 2; 2; 2; 2; 4; 4; 4; 4; (* 0x8X *)
  2; 6; 2; 6; 4; 4; 4; 4; 2; 5; 2; 5; 5; 5; 5; 5; (* 0x9X *)
  2; 6; 2; 6; 3; 3; 3; 3; 2; 2; 2; 2; 4; 4; 4; 4; (* 0xAX *)
  2; 5; 2; 5; 4; 4; 4; 4; 2; 4; 2; 4; 4; 4; 4; 4; (* 0xBX *)
  2; 6; 2; 8; 3; 3; 5; 5; 2; 2; 2; 2; 4; 4; 6; 6; (* 0xCX *)
  2; 5; 2; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; (* 0xDX *)
  2; 6; 2; 8; 3; 3; 5; 5; 2; 2; 2; 2; 4; 4; 6; 6; (* 0xEX *)
  2; 5; 2; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; (* 0xFX *)
|]

let page_cross_penalty = [|
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0x0X *)
  1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0; (* 0x1X *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0x2X *)
  1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0; (* 0x3X *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0x4X *)
  1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0; (* 0x5X *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0x6X *)
  1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0; (* 0x7X *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0x8X *)
  1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0x9X *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0xAX *)
  1; 1; 0; 1; 0; 0; 0; 0; 0; 1; 0; 1; 1; 1; 1; 1; (* 0xBX *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0xCX *)
  1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0; (* 0xDX *)
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; (* 0xEX *)
  1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0; (* 0xFX *)
|]

let operand_len = function
  | Implied -> 0
  | Immediate -> 1
  | Accumulator -> 0
  | ZeroPage -> 1
  | ZeroPageX -> 1
  | ZeroPageY -> 1
  | Absolute -> 2
  | AbsoluteX -> 2
  | AbsoluteY -> 2
  | Relative -> 1
  | IndirectX -> 1
  | IndirectY -> 1
  | AbsoluteInd -> 2

let decode = function
  | 0x69 -> (ADC, Immediate)  | 0x65 -> (ADC, ZeroPage)   | 0x75 -> (ADC, ZeroPageX)
  | 0x6D -> (ADC, Absolute)   | 0x7D -> (ADC, AbsoluteX)  | 0x79 -> (ADC, AbsoluteY)
  | 0x61 -> (ADC, IndirectX)  | 0x71 -> (ADC, IndirectY)  | 0x29 -> (AND, Immediate)
  | 0x25 -> (AND, ZeroPage)   | 0x35 -> (AND, ZeroPageX)  | 0x2D -> (AND, Absolute)
  | 0x3D -> (AND, AbsoluteX)  | 0x39 -> (AND, AbsoluteY)  | 0x21 -> (AND, IndirectX)
  | 0x31 -> (AND, IndirectY)  | 0x0A -> (ASL, Accumulator)| 0x06 -> (ASL, ZeroPage)
  | 0x16 -> (ASL, ZeroPageX)  | 0x0E -> (ASL, Absolute)   | 0x1E -> (ASL, AbsoluteX)
  | 0x90 -> (BCC, Relative)   | 0xB0 -> (BCS, Relative)   | 0xF0 -> (BEQ, Relative)
  | 0x24 -> (BIT, ZeroPage)   | 0x2C -> (BIT, Absolute)   | 0x30 -> (BMI, Relative)
  | 0xD0 -> (BNE, Relative)   | 0x10 -> (BPL, Relative)   | 0x00 -> (BRK, Implied)
  | 0x50 -> (BVC, Relative)   | 0x70 -> (BVS, Relative)   | 0x18 -> (CLC, Implied)
  | 0xD8 -> (CLD, Implied)    | 0x58 -> (CLI, Implied)    | 0xB8 -> (CLV, Implied)
  | 0xC9 -> (CMP, Immediate)  | 0xC5 -> (CMP, ZeroPage)   | 0xD5 -> (CMP, ZeroPageX)
  | 0xCD -> (CMP, Absolute)   | 0xDD -> (CMP, AbsoluteX)  | 0xD9 -> (CMP, AbsoluteY)
  | 0xC1 -> (CMP, IndirectX)  | 0xD1 -> (CMP, IndirectY)  | 0xE0 -> (CPX, Immediate)
  | 0xE4 -> (CPX, ZeroPage)   | 0xEC -> (CPX, Absolute)   | 0xC0 -> (CPY, Immediate)
  | 0xC4 -> (CPY, ZeroPage)   | 0xCC -> (CPY, Absolute)   | 0xC6 -> (DEC, ZeroPage)
  | 0xD6 -> (DEC, ZeroPageX)  | 0xCE -> (DEC, Absolute)   | 0xDE -> (DEC, AbsoluteX)
  | 0xCA -> (DEX, Implied)    | 0x88 -> (DEY, Implied)    | 0x49 -> (EOR, Immediate)
  | 0x45 -> (EOR, ZeroPage)   | 0x55 -> (EOR, ZeroPageX)  | 0x4D -> (EOR, Absolute)
  | 0x5D -> (EOR, AbsoluteX)  | 0x59 -> (EOR, AbsoluteY)  | 0x41 -> (EOR, IndirectX)
  | 0x51 -> (EOR, IndirectY)  | 0xE6 -> (INC, ZeroPage)   | 0xF6 -> (INC, ZeroPageX)
  | 0xEE -> (INC, Absolute)   | 0xFE -> (INC, AbsoluteX)  | 0xE8 -> (INX, Implied)
  | 0xC8 -> (INY, Implied)    | 0x4C -> (JMP, Absolute)   | 0x6C -> (JMP, AbsoluteInd)
  | 0x20 -> (JSR, Absolute)   | 0xA9 -> (LDA, Immediate)  | 0xA5 -> (LDA, ZeroPage)
  | 0xB5 -> (LDA, ZeroPageX)  | 0xAD -> (LDA, Absolute)   | 0xBD -> (LDA, AbsoluteX)
  | 0xB9 -> (LDA, AbsoluteY)  | 0xA1 -> (LDA, IndirectX)  | 0xB1 -> (LDA, IndirectY)
  | 0xA2 -> (LDX, Immediate)  | 0xA6 -> (LDX, ZeroPage)   | 0xB6 -> (LDX, ZeroPageY)
  | 0xAE -> (LDX, Absolute)   | 0xBE -> (LDX, AbsoluteY)  | 0xA0 -> (LDY, Immediate)
  | 0xA4 -> (LDY, ZeroPage)   | 0xB4 -> (LDY, ZeroPageX)  | 0xAC -> (LDY, Absolute)
  | 0xBC -> (LDY, AbsoluteX)  | 0x4A -> (LSR, Accumulator)| 0x46 -> (LSR, ZeroPage)
  | 0x56 -> (LSR, ZeroPageX)  | 0x4E -> (LSR, Absolute)   | 0x5E -> (LSR, AbsoluteX)
  | 0xEA -> (NOP, Implied)    | 0x09 -> (ORA, Immediate)  | 0x05 -> (ORA, ZeroPage)
  | 0x15 -> (ORA, ZeroPageX)  | 0x0D -> (ORA, Absolute)   | 0x1D -> (ORA, AbsoluteX)
  | 0x19 -> (ORA, AbsoluteY)  | 0x01 -> (ORA, IndirectX)  | 0x11 -> (ORA, IndirectY)
  | 0x48 -> (PHA, Implied)    | 0x08 -> (PHP, Implied)    | 0x68 -> (PLA, Implied)
  | 0x28 -> (PLP, Implied)    | 0x2A -> (ROL, Accumulator)| 0x26 -> (ROL, ZeroPage)
  | 0x36 -> (ROL, ZeroPageX)  | 0x2E -> (ROL, Absolute)   | 0x3E -> (ROL, AbsoluteX)
  | 0x6A -> (ROR, Accumulator)| 0x66 -> (ROR, ZeroPage)   | 0x76 -> (ROR, ZeroPageX)
  | 0x6E -> (ROR, Absolute)   | 0x7E -> (ROR, AbsoluteX)  | 0x40 -> (RTI, Implied)
  | 0x60 -> (RTS, Implied)    | 0xE9 -> (SBC, Immediate)  | 0xE5 -> (SBC, ZeroPage)
  | 0xF5 -> (SBC, ZeroPageX)  | 0xED -> (SBC, Absolute)   | 0xFD -> (SBC, AbsoluteX)
  | 0xF9 -> (SBC, AbsoluteY)  | 0xE1 -> (SBC, IndirectX)  | 0xF1 -> (SBC, IndirectY)
  | 0x38 -> (SEC, Implied)    | 0xF8 -> (SED, Implied)    | 0x78 -> (SEI, Implied)
  | 0x85 -> (STA, ZeroPage)   | 0x95 -> (STA, ZeroPageX)  | 0x8D -> (STA, Absolute)
  | 0x9D -> (STA, AbsoluteX)  | 0x99 -> (STA, AbsoluteY)  | 0x81 -> (STA, IndirectX)
  | 0x91 -> (STA, IndirectY)  | 0x86 -> (STX, ZeroPage)   | 0x96 -> (STX, ZeroPageY)
  | 0x8E -> (STX, Absolute)   | 0x84 -> (STY, ZeroPage)   | 0x94 -> (STY, ZeroPageX)
  | 0x8C -> (STY, Absolute)   | 0xAA -> (TAX, Implied)    | 0xA8 -> (TAY, Implied)
  | 0xBA -> (TSX, Implied)    | 0x8A -> (TXA, Implied)    | 0x9A -> (TXS, Implied)
  | 0x98 -> (TYA, Implied)
  (* unofficial opcode *)
  | 0x02 -> (KIL, Implied)    | 0x12 -> (KIL, Implied)    | 0x22 -> (KIL, Implied)
  | 0x32 -> (KIL, Implied)    | 0x42 -> (KIL, Implied)    | 0x52 -> (KIL, Implied)
  | 0x62 -> (KIL, Implied)    | 0x72 -> (KIL, Implied)    | 0x92 -> (KIL, Implied)
  | 0xB2 -> (KIL, Implied)    | 0xD2 -> (KIL, Implied)    | 0xF2 -> (KIL, Implied)
  | 0x7A -> (NOP, Implied)    | 0x5A -> (NOP, Implied)    | 0x1A -> (NOP, Implied)
  | 0x3A -> (NOP, Implied)    | 0xDA -> (NOP, Implied)    | 0xFA -> (NOP, Implied)
  | 0x80 -> (NOP, Immediate)  | 0x82 -> (NOP, Immediate)  | 0x89 -> (NOP, Immediate)
  | 0xC2 -> (NOP, Immediate)  | 0xE2 -> (NOP, Immediate)  | 0x04 -> (NOP, ZeroPage)
  | 0x64 -> (NOP, ZeroPage)   | 0x44 -> (NOP, ZeroPage)   | 0x0C -> (NOP, Absolute)
  | 0x14 -> (NOP, ZeroPageX)  | 0x34 -> (NOP, ZeroPageX)  | 0x54 -> (NOP, ZeroPageX)
  | 0x74 -> (NOP, ZeroPageX)  | 0xD4 -> (NOP, ZeroPageX)  | 0xF4 -> (NOP, ZeroPageX)
  | 0x1C -> (NOP, AbsoluteX)  | 0x3C -> (NOP, AbsoluteX)  | 0x5C -> (NOP, AbsoluteX)
  | 0x7C -> (NOP, AbsoluteX)  | 0xDC -> (NOP, AbsoluteX)  | 0xFC -> (NOP, AbsoluteX)
  | 0xAB -> (LAX, Immediate)  | 0xA7 -> (LAX, ZeroPage)   | 0xB7 -> (LAX, ZeroPageY)
  | 0xAF -> (LAX, Absolute)   | 0xBF -> (LAX, AbsoluteY)  | 0xA3 -> (LAX, IndirectX)
  | 0xB3 -> (LAX, IndirectY)  | 0x87 -> (SAX, ZeroPage)   | 0x97 -> (SAX, ZeroPageY)
  | 0x8F -> (SAX, Absolute)   | 0x83 -> (SAX, IndirectX)  | 0xEB -> (SBC, Immediate)
  | 0xC7 -> (DCP, ZeroPage)   | 0xD7 -> (DCP, ZeroPageX)  | 0xCF -> (DCP, Absolute)
  | 0xDF -> (DCP, AbsoluteX)  | 0xDB -> (DCP, AbsoluteY)  | 0xC3 -> (DCP, IndirectX)
  | 0xD3 -> (DCP, IndirectY)  | 0xE7 -> (ISC, ZeroPage)   | 0xF7 -> (ISC, ZeroPageX)
  | 0xEF -> (ISC, Absolute)   | 0xFF -> (ISC, AbsoluteX)  | 0xFB -> (ISC, AbsoluteY)
  | 0xE3 -> (ISC, IndirectX)  | 0xF3 -> (ISC, IndirectY)  | 0x27 -> (RLA, ZeroPage)
  | 0x37 -> (RLA, ZeroPageX)  | 0x2F -> (RLA, Absolute)   | 0x3F -> (RLA, AbsoluteX)
  | 0x3B -> (RLA, AbsoluteY)  | 0x23 -> (RLA, IndirectX)  | 0x33 -> (RLA, IndirectY)
  | 0x67 -> (RRA, ZeroPage)   | 0x77 -> (RRA, ZeroPageX)  | 0x6F -> (RRA, Absolute)
  | 0x7F -> (RRA, AbsoluteX)  | 0x7B -> (RRA, AbsoluteY)  | 0x63 -> (RRA, IndirectX)
  | 0x73 -> (RRA, IndirectY)  | 0x07 -> (SLO, ZeroPage)   | 0x17 -> (SLO, ZeroPageX)
  | 0x0F -> (SLO, Absolute)   | 0x1F -> (SLO, AbsoluteX)  | 0x1B -> (SLO, AbsoluteY)
  | 0x03 -> (SLO, IndirectX)  | 0x13 -> (SLO, IndirectY)  | 0x47 -> (SRE, ZeroPage)
  | 0x57 -> (SRE, ZeroPageX)  | 0x4F -> (SRE, Absolute)   | 0x5F -> (SRE, AbsoluteX)
  | 0x5B -> (SRE, AbsoluteY)  | 0x43 -> (SRE, IndirectX)  | 0x53 -> (SRE, IndirectY)
  | 0x0B -> (ANC, Immediate)  | 0x2B -> (ANC, Immediate)  | 0x4B -> (ALR, Immediate)
  | 0x6B -> (ARR, Immediate)  | 0x8B -> (XAA, Immediate)  | 0x93 -> (AHX, IndirectY)
  | 0x9F -> (AHX, AbsoluteY)  | 0x9B -> (TAS, AbsoluteY)  | 0x9E -> (SHX, AbsoluteY)
  | 0x9C -> (SHY, AbsoluteX)  | 0xBB -> (LAS, AbsoluteY)  | 0xCB -> (AXS, Immediate)
  | _ -> invalid_arg "invalid opcode"

let pp_addr fmt m op =
  let err_str = "operand size mismatch" in
  match m with
  | Implied -> begin match op with [] -> Format.fprintf fmt "" | _ -> failwith err_str end
  | Immediate -> begin match op with [o1] -> Format.fprintf fmt " #$%02X" o1 | _ -> failwith err_str end
  | Accumulator -> begin match op with [] -> Format.fprintf fmt " A" | _ -> failwith err_str end
  | ZeroPage | Relative -> begin match op with [o1] -> Format.fprintf fmt " $%02X" o1 | _ -> failwith err_str end
  | ZeroPageX -> begin match op with [o1] -> Format.fprintf fmt " $%02X,X" o1 | _ -> failwith err_str end
  | ZeroPageY -> begin match op with [o1] -> Format.fprintf fmt " $%02X,Y" o1 | _ -> failwith err_str end
  | Absolute -> begin match op with o1::[o2] -> Format.fprintf fmt " $%02X%02X" o2 o1 | _ -> failwith err_str end
  | AbsoluteX -> begin match op with o1::[o2] -> Format.fprintf fmt " $%02X%02X,X" o2 o1 | _ -> failwith err_str end
  | AbsoluteY -> begin match op with o1::[o2] -> Format.fprintf fmt " $%02X%02X,Y" o2 o1 | _ -> failwith err_str end
  | AbsoluteInd -> begin match op with o1::[o2] -> Format.fprintf fmt " ($%02X%02X)" o2 o1 | _ -> failwith err_str end
  | IndirectX -> begin match op with [o1] -> Format.fprintf fmt " ($%02X,X)" o1 | _ -> failwith err_str end
  | IndirectY -> begin match op with [o1] -> Format.fprintf fmt " ($%02X),Y" o1 | _ -> failwith err_str end

type t = u16 * (u16 -> u8)

let pp fmt (pc, read8) =
  let read_i x = (read8 x |> U8.to_int) in
  let opc = read_i pc in
  let (m, a) = decode opc in
  let ops = List.map read_i
  (match operand_len a with
  | 1 -> U16.[pc+one]
  | 2 -> U16.[pc+one; pc+(of_int 2)]
  | _ -> []
  )
  in
  Format.fprintf fmt "%s" (mnemonic_to_string m); pp_addr fmt a ops
