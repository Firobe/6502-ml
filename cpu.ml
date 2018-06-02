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
let processor_status = ref 0x00 (* Reserved bit always on *)

(* Memory wrappers *)
class virtual memory_wrapper () = object(_)
    method virtual get : unit -> int
    method virtual set : int -> unit
    method virtual self : unit -> int
end
class addr_wrapper addr = object(_)
    inherit memory_wrapper ()
    method get () = memory.(addr)
    method set v = memory.(addr) <- v
    method self () = addr
end
class ref_wrapper r = object(_)
    inherit memory_wrapper ()
    method get () = !r
    method set v = r := v
    method self () = assert false
end
class dummy_wrapper () = object(_)
    inherit memory_wrapper ()
    method get () = assert false
    method set _ = assert false
    method self () = assert false
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
  | `Carry     -> 1 lsl 0
  | `Zero      -> 1 lsl 1
  | `Interrupt -> 1 lsl 2
  | `Decimal   -> 1 lsl 3
  | `Break     -> 1 lsl 4
  | `Reserved  -> 1 lsl 5
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
    Printf.printf "-------------------------------------------------\n" ;
    Printf.printf "PC = %.4X\n%!" !program_counter ;
    Printf.printf "SP = %.4X  ACC = %.2X  X = %.2X  Y = %.2X\n%!" !stack_pointer !accumulator
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
  r := v ;
  update_zero_flag v ;
  update_negative_flag v

let _LDA = aux_LD accumulator
let _LDX = aux_LD index_register_x
let _LDY = aux_LD index_register_y

let aux_ST (m: memory_wrapper) v =
  m#set v

let _STA m = aux_ST m !accumulator
let _STX m = aux_ST m !index_register_x
let _STY m = aux_ST m !index_register_y

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
  stack_pointer := (!stack_pointer - 1) land 0xFF

let aux_pull r =
  stack_pointer := (!stack_pointer + 1) land 0xFF ;
  r := memory.(0x0100 + !stack_pointer)

let _TSX _ = aux_T !stack_pointer index_register_x
let _TXS _ = stack_pointer := !index_register_x
let _PHA _ = aux_push !accumulator
let _PHP _ = aux_push (!processor_status lor get_flag_mask `Break)
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
let aux_CMP r (m : memory_wrapper) =
  let nv = m#get () in
  let c = r - nv in
  update_zero_flag c ;
  update_negative_flag c ;
  set_flag (c >= 0) `Carry

(* 8-bit BCD to/from decimal int *)
let bcd_to_dec b = 
    let lo = b land 0x0F in
    let hi = b lsr 4 in
    lo + hi * 10
let dec_to_bcd d =
    let lo = d mod 10 in
    let hi = d / 10 in
    lo lor (hi lsl 4)

let _ADC m =
  let v = m#get () in
  let decimal = get_flag `Decimal = 1 in
  let pre = if decimal then bcd_to_dec else fun x -> x in
  let post = if decimal then dec_to_bcd else fun x -> x in
  let max = if decimal then 99 else 0xFF in
  let op1 = pre !accumulator in
  let op2 = pre v in
  let c = get_flag `Carry in
  let sum = op1 + op2 + c in
  let nc = sum > max in
  set_flag nc `Carry ;
  let rsum = sum mod (max + 1) in
  let overflow = ((!accumulator lxor rsum) land (v lxor rsum) land 0x80) != 0 in
  set_flag overflow `Overflow ;
  accumulator := post rsum ;
  update_zero_flag !accumulator ;
  update_negative_flag !accumulator

let _SBC m =
  let v = m#get () in
  let c2 = if get_flag `Decimal = 1 then
      dec_to_bcd (100 - (bcd_to_dec v) - 1)
  else
      (v lxor 0xFF) in
  _ADC (new ref_wrapper (ref c2))

let _CMP m = aux_CMP !accumulator m
let _CPX m = aux_CMP !index_register_x m
let _CPY m = aux_CMP !index_register_y m

(* Increments & Decrements *)
let aux_cr v r =
  let nv = r#get () in
  let updated = (nv + v) land 0xFF in
  r#set updated ;
  update_zero_flag updated ;
  update_negative_flag updated

let _INC = aux_cr 1
let _INX _ = _INC (new ref_wrapper index_register_x)
let _INY _ = _INC (new ref_wrapper index_register_y)
let _DEC = aux_cr (-1)
let _DEX _ = _DEC (new ref_wrapper index_register_x)
let _DEY _ = _DEC (new ref_wrapper index_register_y)

(* Shifts *)
let _ASL m =
  let nv = m#get () in
  update_zero_flag nv ;
  set_flag ((nv lsr 7) land 0x1 = 1) `Carry ;
  let updated = (nv lsl 1) land 0xFF in
  update_negative_flag updated ;
  m#set updated

let _LSR m =
  let nv = m#get () in
  set_flag (nv land 0x1 = 1) `Carry ;
  let updated = nv lsr 1 in
  m#set updated ;
  update_zero_flag updated ;
  update_negative_flag updated

let _ROL m =
  let nv = m#get () in
  let new_carry = ((nv lsr 7) land 0x1 = 1) in
  let updated = ((nv lsl 1) lor (get_flag `Carry)) land 0xFF in
  m#set updated ;
  update_zero_flag updated ;
  update_negative_flag updated ;
  set_flag new_carry `Carry

let _ROR m =
  let nv = m#get () in
  let new_carry = (nv land 0x1 = 1) in
  let updated = (nv lsr 1) lor ((get_flag `Carry) lsl 7) in
  m#set updated ;
  update_zero_flag updated ;
  update_negative_flag updated ;
  set_flag new_carry `Carry

(* Jump and calls *)
let _JMP addr = program_counter := addr#self ()
let _JSR addr =
    let r = !program_counter - 1 in
    aux_push @@ r lsr 8 ;
    aux_push @@ r land 0x00FF ;
    _JMP addr
let _RTS _ =
    let hi = ref 0 in
    let lo = ref 0 in
    aux_pull lo ;
    aux_pull hi ;
    program_counter := !lo lor (!hi lsl 8) ;
    program_counter := !program_counter + 1

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
let _SEC _ = set_flag true `Carry
let _SED _ = set_flag true `Decimal
let _SEI _ = set_flag true `Interrupt

(* System functions *)
let _BRK _ =
  let v = !program_counter + 1 in
  aux_push @@ v lsr 8 ;
  aux_push @@ v land 0x00FF ;
  set_flag true `Break ;
  aux_push !processor_status ;
  set_flag true `Interrupt ;
  let irq_l = memory.(0xFFFE) in
  let irq_h = memory.(0xFFFF) lsl 8 in
  program_counter := irq_l lor irq_h

let _NOP _ = ()

let _RTI _ =
  aux_pull processor_status ;
  let lo = ref 0 in
  let hi = ref 0 in
  aux_pull lo ;
  aux_pull hi ;
  program_counter := !lo lor (!hi lsl 8)

let shift_and_mask v dec mask =
    let target = v lsr dec in
    target land mask

let invalid_instruction () =
    Printf.printf "Invalid instruction %.2X\n" memory.(!program_counter) ;
    assert false

(* Addressing and instruction dispatch *)
let get_addressing_mode a b c = match b with
  | 0 -> begin match c with
      | 0 -> begin match a with
          | 1 -> Absolute
          | a when a > 4 -> Immediate
          | _ -> Implicit
        end
      | 1 -> Indexed_Indirect (* x, ind *)
      | 2 -> Immediate
      | _ -> invalid_instruction ()
    end
  | 1 -> Zero_Page
  | 2 -> begin match c with
      | 0 -> Implicit
      | 1 -> Immediate
      | 2 -> if a < 4 then Accumulator else Implicit
      | _ -> invalid_instruction ()
    end
  | 3 -> if a = 3 && c = 0 then Indirect else Absolute
  | 4 -> begin match c with
      | 0 -> Relative
      | 1 -> Indirect_Indexed (* ind, y *)
      | _ -> invalid_instruction ()
    end
  | 5 -> if a < 4 || a > 5 || c != 2 then Zero_Page_X else Zero_Page_Y
  | 6 -> begin match c with
      | 0 -> Implicit
      | 1 -> Absolute_Y
      | 2 -> Implicit
      | _ -> invalid_instruction ()
    end
  | 7 -> if c = 2 && a = 5 then Absolute_Y else Absolute_X
  | _ -> invalid_instruction ()

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

let next_instr ?debug:(debug=false) () =
  let opcode = memory.(!program_counter) in
  let a = shift_and_mask opcode 5 0x7 in
  let b = shift_and_mask opcode 2 0x7 in
  let c = shift_and_mask opcode 0 0x3 in
  let b1 = !program_counter + 1 in
  let b2 = !program_counter + 2 in
  let v1 = memory.(b1) in
  let v2 = memory.(b2) in
  let v12 = v1 lor (v2 lsl 8) in
  if debug then (
      Printf.printf "Executing instruction : " ;
      Printf.printf "%.2X" opcode ;
      Printf.printf " %.2X %.2X\n" v1 v2
  ) ;
  let addr_mode = get_addressing_mode a b c in
  let mode_size = addressing_mode_size addr_mode in
  program_counter := !program_counter + mode_size ;
  let arg = begin match addr_mode with
  | Implicit -> new dummy_wrapper ()
  | Accumulator -> new ref_wrapper accumulator
  | Immediate -> new addr_wrapper b1
  | Zero_Page -> new addr_wrapper v1
  | Zero_Page_X -> new addr_wrapper ((v1 + !index_register_x) land 0xFF)
  | Zero_Page_Y -> new addr_wrapper ((v1 + !index_register_y) land 0xFF)
  | Relative -> new addr_wrapper b1
  | Absolute -> new addr_wrapper v12
  | Absolute_X -> new addr_wrapper (!index_register_x + v12)
  | Absolute_Y -> new addr_wrapper (!index_register_y + v12)
  | Indirect -> new addr_wrapper (memory.(v12) lor (memory.(v12 + 1) lsl 8))
  | Indexed_Indirect (*X*) -> 
          let sto_addr = (v1 + !index_register_x) land 0xFF in
          new addr_wrapper (memory.(sto_addr) lor (memory.(sto_addr + 1) lsl 8))
  | Indirect_Indexed (*Y*) ->
          let sto = memory.(v1) lor (memory.(v1 + 1) lsl 8) in
          new addr_wrapper (sto + !index_register_y)
  end in
  let ins_fun = get_instruction_fun a b c in
  current_addressing_mode := addr_mode ;
  processor_status := !processor_status lor (get_flag_mask `Reserved) ;
  ins_fun arg

