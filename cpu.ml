(* http://www.obelisk.me.uk/6502/ *)
(* https://wiki.nesdev.com/w/index.php/RTS_Trick *)
(* http://www.masswerk.at/6502/6502_instruction_set.html *)

(* STATE *)

(* 0x000 to 0xFFFF main memory *)
let memory = Array.make 0x10000 0x00

let general_registers = Array.make 0x10 0x00

(* Registers *)
let program_counter = ref 0x0400
let stack_pointer = ref 0x00FF
let accumulator = ref 0x00
let index_register_x = ref 0x00
let index_register_y = ref 0x00
let processor_status = ref 0x0

(* Memory wrappers *)
class virtual memory_wrapper () = object(_)
    method virtual get : unit -> int
    method virtual set : int -> unit
end
class addr_wrapper addr = object(_)
    inherit memory_wrapper ()
    method get () = memory.(addr)
    method set v = memory.(addr) <- v
end
class ref_wrapper r = object(_)
    inherit memory_wrapper ()
    method get () = !r
    method set v = r := v
end
class dummy_wrapper () = object(_)
    inherit memory_wrapper ()
    method get () = assert false
    method set _ = assert false
end

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
let current_addressing_mode = ref Implicit

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

let dump_registers () =
    Printf.printf "PC = %.4X  SP = %.4X\n%!" !program_counter !stack_pointer ;
    Printf.printf "ACC = %.2X  X = %.2X  Y = %.2X\n%!" !accumulator
        !index_register_x !index_register_y ;
    Printf.printf "C=%d Z=%d I=%d D=%d B=%d V=%d N=%d\n%!"
        (get_flag `Carry) (get_flag `Zero) (get_flag `Interrupt) (get_flag `Decimal)
        (get_flag `Break) (get_flag `Overflow) (get_flag `Negative) ;
    Printf.printf "-------------------------------------------------\n"

let update_zero_flag v =
  set_flag (v = 0) `Zero

let update_negative_flag v =
  let cond = (v lsr 7) land 0x1 = 1 in
  set_flag cond `Negative

(* INSTRUCTIONS *)

(* Load/Store *)
let aux_LD r (m : memory_wrapper) =
  let v = m#get () in
  let nv = if !current_addressing_mode = Absolute then
      memory.(v)
  else v in
  r := nv ;
  update_zero_flag nv ;
  update_negative_flag nv

let _LDA = aux_LD accumulator
let _LDX = aux_LD index_register_x
let _LDY = aux_LD index_register_y
let _STA m = m#set !accumulator
let _STX m = m#set !index_register_x
let _STY m = m#set !index_register_y

(* Register transfers *)
let aux_T f t = 
  t := f ;
  update_zero_flag f ;
  update_negative_flag f

let _TAX _ = aux_T !accumulator index_register_x
let _TAY _ = aux_T !accumulator index_register_y
let _TXA _ = aux_T !index_register_x accumulator
let _TYA _ = aux_T !index_register_y accumulator

(* Stack operations *)
let aux_push v =
  memory.(0x0100 + !stack_pointer) <- v ;
  stack_pointer := !stack_pointer - 1

let aux_pull r =
  stack_pointer := !stack_pointer + 1 ;
  r := memory.(0x0100 + !stack_pointer)

let _TSX _ = aux_T !stack_pointer index_register_x
let _TXS _ = stack_pointer := !index_register_x
let _PHA _ = aux_push !accumulator
let _PHP _ = aux_push !processor_status
let _PLA _ =
  aux_pull accumulator ;
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator
let _PLP _ = aux_pull processor_status

(* Logical *)
let aux_logic f =
  accumulator := f !accumulator;
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator

let _AND m = aux_logic ((land) @@ m#get ())
let _EOR m = aux_logic ((lxor) @@ m#get ())
let _ORA m = aux_logic ((lor) @@ m#get ())
let _BIT m =
  let v = m#get () in
  let masked = !accumulator land v in
  update_zero_flag masked ;
  set_flag ((v lsr 7) land 0x1 = 1) `Negative ;
  set_flag ((v lsr 6) land 0x1 = 1) `Overflow

(* Arithmetic *)
let aux_CMP c =
  update_zero_flag c ;
  update_negative_flag c ;
  set_flag (c >= 0) `Carry

let _ADC m =
  let v = m#get () in
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator ;
  let sum = !accumulator + v + get_flag `Carry in
  set_flag (sum > 0xFF) `Carry ;
  set_flag (sum > 0x7F) `Overflow ; (* Weird *)
  accumulator := sum land 0xFF

let _SBC m =
  let v = m#get () in
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator ;
  let sub = !accumulator - v - (1 - get_flag `Carry) in
  set_flag (sub > !accumulator) `Carry ;
  set_flag (sub > !accumulator) `Overflow ;
  accumulator := sub land 0xFF

let _CMP m = aux_CMP (!accumulator - (m#get ()))
let _CPX m = aux_CMP (!index_register_x - (m#get ()))
let _CPY m = aux_CMP (!index_register_y - (m#get ()))

(* Increments & Decrements *)
let aux_cr v r =
  r#set ((r#get () + v) land 0xFF);
  update_zero_flag (r#get ()) ;
  update_negative_flag (r#get ())

let _INC = aux_cr 1
let _INX _ = _INC (new ref_wrapper index_register_x)
let _INY _ = _INC (new ref_wrapper index_register_y)
let _DEC = aux_cr (-1)
let _DEX _ = _DEC (new ref_wrapper index_register_x)
let _DEY _ = _DEC (new ref_wrapper index_register_y)

(* Shifts *)
let _ASL m =
  let v = m#get () in
  update_zero_flag v ;
  update_negative_flag v ;
  set_flag ((v lsr 7) land 0x1 = 1) `Carry ;
  m#set @@ v lsl 1

let _LSR m =
  let v = m#get () in
  set_flag (v land 0x1 = 1) `Carry ;
  m#set @@ v lsr 1 ;
  update_zero_flag (m#get ()) ;
  update_negative_flag (m#get ())

let _ROL m =
  let v = m#get () in
  update_zero_flag v ;
  let new_carry = ((v lsr 7) land 0x1 = 1) in
  m#set @@ v lsl 1 land (get_flag `Carry) ;
  set_flag new_carry `Carry ;
  update_negative_flag @@ m#get ()

let _ROR m =
  let v = m#get () in
  update_zero_flag v ;
  let new_carry = (v land 0x1 = 1) in
  m#set @@ v lsr 1 land ((get_flag `Carry) lsl 7);
  set_flag new_carry `Carry ;
  update_negative_flag @@ m#get ()

(* Jump and calls *)
let _JMP addr = program_counter := addr#get ()
let _JSR addr = aux_push (!program_counter - 1) ; _JMP addr
let _RTS _ = aux_pull program_counter ; program_counter := !program_counter + 1

(* Branches *)
let aux_branch f s v =
  let nv = if v > 0x7F then
      - (((lnot v) + 1) land 0xFF)
      else v
  in
  if get_flag f = s then
    program_counter := !program_counter + nv

let _BCC rel = aux_branch `Carry 0 @@ rel#get ()
let _BCS rel = aux_branch `Carry 1 @@ rel#get ()
let _BEQ rel = aux_branch `Zero 1 @@ rel#get ()
let _BMI rel = aux_branch `Negative 1 @@ rel#get ()
let _BNE rel = aux_branch `Zero 0 @@ rel#get ()
let _BPL rel = aux_branch `Negative 0 @@ rel#get ()
let _BVC rel = aux_branch `Overflow 0 @@ rel#get ()
let _BVS rel = aux_branch `Overflow 1 @@ rel#get ()

(* Status Flag Changes *)
let _CLC _ = set_flag false `Carry
let _CLD _ = set_flag false `Decimal
let _CLI _ = set_flag false `Interrupt
let _CLV _ = set_flag false `Overflow
let _SEC _ = set_flag false `Carry
let _SED _ = set_flag true `Decimal
let _SEI _ = set_flag true `Interrupt

(* System functions *)
let _BRK _ =
  aux_push !program_counter ;
  aux_push !processor_status ;
  set_flag true `Break ;
  let irq_l = memory.(0xFFFE) in
  let irq_h = memory.(0xFFFF) lsl 7 in
  program_counter := irq_l land irq_h

let _NOP _ = ()

let _RTI _ =
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

let get_instruction_fun a b c = match (c, a) with
    | 0, 0 -> List.nth [_BRK; _NOP; _PHP; _NOP; _BPL; _NOP; _CLC] b
    | 0, 1 -> List.nth [_JSR; _BIT; _PLP; _BIT; _BMI; _NOP; _SEC] b
    | 0, 2 -> List.nth [_RTI; _NOP; _PHA; _JMP; _BVC; _NOP; _CLI] b
    | 0, 3 -> List.nth [_RTS; _NOP; _PLA; _JMP; _BVS; _NOP; _SEI] b
    | 0, 4 -> List.nth [_NOP; _STY; _DEY; _STY; _BCC; _STY; _TYA] b
    | 0, 5 -> List.nth [_LDY; _LDY; _TAY; _LDY; _BCS; _LDY; _CLV; _LDY] b
    | 0, 6 -> List.nth [_CPY; _CPY; _INY; _CPY; _BNE; _NOP; _CLD] b
    | 0, 7 -> List.nth [_CPX; _CPX; _INX; _CPX; _BEQ; _NOP; _SED] b
    | 1, 0 -> _ORA | 1, 1 -> _AND | 1, 2 -> _EOR | 1, 3 -> _ADC
    | 1, 4 -> _STA | 1, 5 -> _LDA | 1, 6 -> _CMP | 1, 7 -> _SBC
    | 2, 0 -> _ASL | 2, 1 -> _ROL | 2, 2 -> _LSR | 2, 3 -> _ROR
    | 2, 4 -> List.nth [_NOP; _STX; _TXA; _STX; _NOP; _STX; _TXS] b
    | 2, 5 -> List.nth [_LDX; _LDX; _TAX; _LDX; _NOP; _LDX; _TSX; _LDX] b
    | 2, 6 -> List.nth [_NOP; _DEC; _DEX; _DEC; _NOP; _DEC; _NOP; _DEC] b
    | 2, 7 -> if b = 2 then _NOP else _INC
    | _ -> assert false

let next_instr () =
  let opcode = memory.(!program_counter) in
  let a = shift_and_mask opcode 5 0x7 in
  let b = shift_and_mask opcode 2 0x7 in
  let c = shift_and_mask opcode 0 0x3 in
  let b1 = !program_counter + 1 in
  let b2 = !program_counter + 2 in
  let v1 = memory.(b1) in
  let v2 = memory.(b2) in
  let v12 = v1 lor (v2 lsl 8) in
  Printf.printf "Executing %.2X %.2X %.2X\n" opcode v1 v2;
  Printf.printf "(a, b, c) = (%d, %d, %d)\n%!" a b c ;
  let addr_mode = get_addressing_mode a b c in
  let mode_size = addressing_mode_size addr_mode in
  program_counter := !program_counter + mode_size ;
  let arg = begin match addr_mode with
  | Implicit -> new dummy_wrapper ()
  | Accumulator -> new ref_wrapper accumulator
  | Immediate -> new addr_wrapper b1
  | Zero_Page -> new addr_wrapper v1
  | Zero_Page_X -> new addr_wrapper (v1 + !index_register_x)
  | Zero_Page_Y -> new addr_wrapper (v1 + !index_register_y)
  | Relative -> new addr_wrapper b1
  | Absolute -> new ref_wrapper (ref v12)
  | Absolute_X -> new addr_wrapper (!index_register_x + v12)
  | Absolute_Y -> new addr_wrapper (!index_register_y + v12)
  | Indirect -> new addr_wrapper v12
  | Indexed_Indirect -> new addr_wrapper memory.((v1 + !index_register_x))
  | Indirect_Indexed -> new addr_wrapper (memory.(v1) + !index_register_y)
  end in
  let ins_fun = get_instruction_fun a b c in
  current_addressing_mode := addr_mode ;
  ins_fun arg

let load_rom path =
    let file = open_in_bin path in
    let store = Bytes.create 0x1000 in
    let read = input file store 0 0x1000 in
    let store = Bytes.sub store 0 read in
    Bytes.iteri (fun i el -> Array.set memory (0x0 + i) (int_of_char el)) store ;
    Printf.printf "Loaded %d bytes into the RAM\n" read

let main =
    if Array.length Sys.argv > 1 then
        load_rom Sys.argv.(1)
    ;
    let continue = ref true in
    while !continue do
        dump_registers () ;
        let back = !program_counter in
        next_instr () ;
        if back = !program_counter then
            continue := false
    done ;
    Printf.printf "END : trap encountered at %.4X\n%!" !program_counter
