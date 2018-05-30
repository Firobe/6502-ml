(* STATE *)

(* 0x000 to 0xFFFF main memory *)
let memory = Array.make 0x10000 0x00

let general_registers = Array.make 0x10 0x00

(* Registers *)
let program_counter = ref 0x0000
let stack_pointer = ref 0x00FF
let accumulator = ref 0x00
let index_register_x = ref 0x00
let index_register_y = ref 0x00
let processor_status = ref 0x0

(* Utils *)
type addressing_mode =
  | Implicit
  | Accumulator
  | Immediate
  | Zero_Page
  | Zero_Page_X
  | Zero_Page_Y
  | Relative
  | Absolute
  | Absolute_X
  | Absolute_Y
  | Indirect
  | Indexed_Indirect
  | Indirect_Indexed

let addressing_mode_size = function
  | Implicit
  | Accumulator       -> 1
  | Immediate
  | Zero_Page
  | Zero_Page_X
  | Zero_Page_Y
  | Indexed_Indirect
  | Indirect_Indexed
  | Relative          -> 2
  | Absolute
  | Absolute_X
  | Absolute_Y
  | Indirect          -> 3

let get_flag_mask f = match f with
  | `Carry     -> 1 lsl 1
  | `Zero      -> 1 lsl 2
  | `Interrupt -> 1 lsl 3
  | `Decimal   -> 1 lsl 4
  | `Break     -> 1 lsl 5
  | `Overflow  -> 1 lsl 6
  | `Negative  -> 1 lsl 7

let set_flag cond f =
  let mask = get_flag_mask f in
  if cond then
    processor_status := !processor_status lor mask
  else
    processor_status := !processor_status land (lnot mask)

let get_flag f =
  if get_flag_mask f land !processor_status != 0 then 1 else 0

(*
let dump_registers () =
    Printf.printf "PC = %.4X   SP = %.2X\n" !program_counter !stack_pointer ;
    Printf.printf "DT = %.4X   ST = %.4X\n" !delay_timer !sound_timer ;
    Array.iteri (Printf.printf "V%X = %.2X\n") general_registers ;
    Printf.printf "RI = %.4X\n%!" !register_I
*)

let update_zero_flag v =
  set_flag (v = 0) `Zero

let update_negative_flag v =
  let cond = (v lsr 7) land 0x1 = 1 in
  set_flag cond `Negative

(* INSTRUCTIONS *)

(* Load/Store *)
let aux_LD r m =
  r := m ;
  update_zero_flag m ;
  update_negative_flag m

let _LDA = aux_LD accumulator
let _LDX = aux_LD index_register_x
let _LDY = aux_LD index_register_y
let _STA m = m := !accumulator
let _STX m = m := !index_register_x
let _STY m = m := !index_register_y

(* Register transfers *)
let aux_T f t = 
  t := f ;
  update_zero_flag f ;
  update_negative_flag f

let _TAX = aux_T !accumulator index_register_x
let _TAY = aux_T !accumulator index_register_y
let _TXA = aux_T !index_register_x accumulator
let _TYA = aux_T !index_register_y accumulator

(* Stack operations *)
let aux_push v =
  memory.(0x0100 + !stack_pointer) <- v ;
  stack_pointer := !stack_pointer - 1

let aux_pull r =
  stack_pointer := !stack_pointer + 1 ;
  r := memory.(0x0100 + !stack_pointer)

let _TSX = aux_T !stack_pointer index_register_x
let _TXS = stack_pointer := !index_register_x
let _PHA = aux_push !accumulator
let _PHP = aux_push !processor_status
let _PLA =
  aux_pull accumulator ;
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator
let _PLP = aux_pull processor_status

(* Logical *)
let aux_logic f =
  accumulator := f !accumulator;
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator

let _AND m = aux_logic ((land) m)
let _EOR m = aux_logic ((lxor) m)
let _ORA m = aux_logic ((lor) m)
let _BIT m =
  let masked = !accumulator land m in
  update_zero_flag masked ;
  set_flag ((m lsr 7) land 0x1 = 1) `Negative ;
  set_flag ((m lsr 6) land 0x1 = 1) `Overflow

(* Arithmetic *)
let aux_CMP c =
  update_zero_flag c ;
  update_negative_flag c ;
  set_flag (c >= 0) `Carry

let _ADC m =
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator ;
  let sum = !accumulator + m + get_flag `Carry in
  set_flag (sum > 0xFF) `Carry ;
  set_flag (sum > 0xFF) `Overflow ; (* Weird *)
  accumulator := sum

let _SBC m =
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator ;
  let sub = !accumulator - m - (1 - get_flag `Carry) in
  set_flag (sub > !accumulator) `Carry ;
  set_flag (sub > !accumulator) `Overflow ;
  accumulator := sub

let _CMP m = aux_CMP (!accumulator - m)
let _CPX m = aux_CMP (!index_register_x - m)
let _CPY m = aux_CMP (!index_register_y - m)

(* Increments & Decrements *)
let aux_cr v r =
  r := !r + v ;
  update_zero_flag !r ;
  update_negative_flag !r

let _INC = aux_cr 1
let _INX = aux_cr 1 index_register_x
let _INY = aux_cr 1 index_register_y
let _DEC = aux_cr (-1)
let _DEX = aux_cr (-1) index_register_x
let _DEY = aux_cr (-1) index_register_y

(* Shifts *)
let _ASL m =
  update_zero_flag !m ;
  update_negative_flag !m ;
  set_flag ((!m lsr 7) land 0x1 = 1) `Carry ;
  m := !m lsl 1

let _LSR m =
  set_flag (!m land 0x1 = 1) `Carry ;
  m := !m lsr 1 ;
  update_zero_flag !m ;
  update_negative_flag !m

let _ROL m =
  update_zero_flag !m ;
  let new_carry = ((!m lsr 7) land 0x1 = 1) in
  m := !m lsl 1 land (get_flag `Carry) ;
  set_flag new_carry `Carry ;
  update_negative_flag !m

let _ROR m =
  update_zero_flag !m ;
  let new_carry = (!m land 0x1 = 1) in
  m := !m lsr 1 land ((get_flag `Carry) lsl 7);
  set_flag new_carry `Carry ;
  update_negative_flag !m

(* Jump and calls *)
let _JMP addr = program_counter := addr
let _JSR addr = aux_push (!program_counter - 1) ; _JMP addr
let _RTS = aux_pull program_counter ; program_counter := !program_counter + 1

(* Branches *)
let aux_branch f s v =
  if get_flag f = s then
    program_counter := !program_counter + v

let _BCC rel = aux_branch `Carry 0 rel
let _BCS rel = aux_branch `Carry 1 rel
let _BEQ rel = aux_branch `Zero 1 rel
let _BMI rel = aux_branch `Negative 1 rel
let _BNE rel = aux_branch `Zero 0 rel
let _BPL rel = aux_branch `Negative 0 rel
let _BVC rel = aux_branch `Overflow 0 rel
let _BVS rel = aux_branch `Overflow 1 rel

(* Status Flag Changes *)
let _CLC = set_flag false `Carry
let _CLD = set_flag false `Decimal
let _CLI = set_flag false `Interrupt
let _CLV = set_flag false `Overflow
let _SEC = set_flag false `Carry
let _SED = set_flag true `Decimal
let _SEI = set_flag true `Interrupt

(* System functions *)
let _BRK =
  aux_push !program_counter ;
  aux_push !processor_status ;
  set_flag true `Break ;
  let irq_l = memory.(0xFFFE) in
  let irq_h = memory.(0xFFFF) lsl 7 in
  program_counter := irq_l land irq_h

let _NOP = ()

let _RTI =
  aux_pull processor_status ;
  aux_pull program_counter

let shift_and_mask v dec mask =
    let target = v lsr dec in
    target land mask

(* Addressing and instruction dispatch *)
let get_addressing_mode a b c = match b with
  | 0 -> begin match c with
      | 0 -> begin match a with
          | 1 -> Absolute
          | a when a > 4 -> Immediate
          | _ -> Implicit
        end
      | 1 -> Indirect_Indexed (* x, ind *)
      | 2 -> Immediate
      | _ -> assert false
    end
  | 1 -> Zero_Page
  | 2 -> begin match c with
      | 0 -> Implicit
      | 1 -> Immediate
      | 2 -> if a < 4 then Accumulator else Implicit
      | _ -> assert false
    end
  | 3 -> Absolute
  | 4 -> begin match c with
      | 0 -> Relative
      | 1 -> Indexed_Indirect (* ind, y *)
      | _ -> assert false
    end
  | 5 -> Zero_Page_X
  | 6 -> begin match c with
      | 0 -> Implicit
      | 1 -> Absolute_Y
      | 2 -> Implicit
      | _ -> assert false
    end
  | 7 -> if c = 2 && a = 5 then Absolute_Y else Absolute_X
  | _ -> assert false

let next_instr () =
  let opcode = memory.(!program_counter) in
  let a = shift_and_mask opcode 0 0x7 in
  let b = shift_and_mask opcode 3 0x7 in
  let c = shift_and_mask opcode 6 0x3 in
  let addressing_mode = get_addressing_mode a b c in ()
