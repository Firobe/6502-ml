(* STATE *)

(* 0x000 to 0xFFFF main memory *)
let memory = Array.make 0x10000 0x00
let enable_decimal = ref true

(* Registers *)
let program_counter = ref 0x0400
let stack_pointer = ref 0x00FF
let acc = ref 0x00
let irx = ref 0x00
let iry = ref 0x00
let processor_status = ref 0x24

let mk_addr lo hi = lo lor (hi lsl 8)
module Stack = struct
    let push v =
      memory.(0x0100 + !stack_pointer) <- v ;
      stack_pointer := (!stack_pointer - 1) land 0xFF

    let push_addr v =
        push (v lsr 8);
        push (v land 0xFF)

    let pull =
      stack_pointer := (!stack_pointer + 1) land 0xFF ;
      memory.(0x0100 + !stack_pointer)

    let pull_addr =
        let lo = pull in
        let hi = pull in
        mk_addr lo hi
end

let cycle_count = ref 0

let reset () =
    Array.fill memory 0 0x10000 0x0 ;
    enable_decimal := true ;
    program_counter := 0x0400 ;
    acc := 0x0 ;
    irx := 0x0 ;
    iry := 0x0 ;
    processor_status := 0x24 ;
    cycle_count := 0

let init_pc () =
    program_counter := (memory.(0xFFFD) lsl 8) lor memory.(0xFFFC)

module Location = struct
    type t =
        | None
        | Ref of int ref
        | Address of int
    let get = function
        | None -> assert false
        | Ref r -> !r
        | Address a -> memory.(a land 0xFFFF)
    let set l v = match l with
        | None -> assert false
        | Ref r -> r := v
        | Address a -> memory.(a land 0xFFFF) <- v
    let ref = function
        | Address a -> a
        | _ -> assert false
end

let ( !! ) = Location.get
let ( <<- ) = Location.set

(* Utils *)
type addressing_mode =
  | Implicit | Accumulator | Immediate | Zero_Page
  | Zero_Page_X | Zero_Page_Y | Relative | Absolute
  | Absolute_X | Absolute_Y | Indirect | Indexed_Indirect
  | Indirect_Indexed

let addressing_mode_size = function
  | Implicit | Accumulator                            -> 1
  | Immediate | Zero_Page | Zero_Page_X | Zero_Page_Y
  | Indexed_Indirect | Indirect_Indexed | Relative    -> 2
  | Absolute | Absolute_X | Absolute_Y | Indirect     -> 3

module Flag = struct
    type t = Mask of int
    let mkflag n = Mask (1 lsl n)
    let carry = mkflag 0
    let zero = mkflag 1
    let interrupt = mkflag 2
    let decimal = mkflag 3
    let break = mkflag 4
    let reserved = mkflag 5
    let overflow = mkflag 6
    let negative = mkflag 7

    let set (Mask m) v =
        processor_status := if v then
            !processor_status lor m
        else !processor_status land (lnot m)

    let get (Mask m) =
        if m land !processor_status != 0 then true else false
    let geti m = if get m then 1 else 0

    let update_zero v = set zero (v = 0)
    let update_neg v = set negative ((v lsr 7) land 0x1 = 1)
    let update_nz v = update_zero v; update_neg v
end

(* List of instructions *)
module Instruction = struct
    type t = Location.t -> unit

    (* Load/Store *)
    let gen_LD r (m : Location.t) =
      r := !!m ; Flag.update_nz !!m
    let _LDA = gen_LD acc
    let _LDX = gen_LD irx
    let _LDY = gen_LD iry

    let _STA m = m <<- !acc
    let _STX m = m <<- !irx
    let _STY m = m <<- !iry
    let _SAX m = m <<- (!acc land !irx)

    (* Register transfers *)
    let gen_T f t  = t := !f; Flag.update_nz !f
    let _TAX _ = gen_T acc irx
    let _TAY _ = gen_T acc iry
    let _TXA _ = gen_T irx acc
    let _TYA _ = gen_T iry acc

    (* Stack operations *)
    let _TSX _ = gen_T stack_pointer irx
    let _TXS _ = stack_pointer := !irx
    let _PHA _ = Stack.push !acc
    let _PHP _ = Stack.push (!processor_status lor Flag.geti Flag.break)
    let _PLA _ = acc := Stack.pull; Flag.update_nz !acc
    let _PLP _ =
        processor_status := Stack.pull;
        Flag.set Flag.break false;
        Flag.set Flag.reserved true

    (* Logical *)
    let gen_OP f m =
        acc := f !!m !acc; Flag.update_nz !acc
    let _AND = gen_OP (land)
    let _EOR = gen_OP (lxor)
    let _ORA = gen_OP (lor)
    let _BIT m =
        let masked = !acc land !!m in
        Flag.update_zero masked;
        Flag.set Flag.negative ((!!m lsr 7) land 0x1 = 1);
        Flag.set Flag.overflow ((!!m lsr 6) land 0x1 = 1)

    (* Arithmetic *)
    let bcd_to_dec b = 
        let lo = b land 0x0F in
        let hi = b lsr 4 in
        lo + hi * 10
    let dec_to_bcd d =
        let lo = d mod 10 in
        let hi = d / 10 in
        lo lor (hi lsl 4)

    (* Addition : binary or decimal *)
    let _ADC m =
      let v = !!m in
      let decimal = Flag.get Flag.decimal && !enable_decimal in
      let pre = if decimal then bcd_to_dec else fun x -> x in
      let post = if decimal then dec_to_bcd else fun x -> x in
      let max = if decimal then 99 else 0xFF in
      let op1 = pre !acc in
      let op2 = pre v in
      let c = Flag.geti Flag.carry in
      let sum = op1 + op2 + c in
      Flag.set Flag.carry (sum > max);
      let rsum = sum mod (max + 1) in
      let overflow = ((!acc lxor rsum) land (v lxor rsum) land 0x80) != 0 in
      Flag.set Flag.overflow overflow;
      acc := post rsum ;
      Flag.update_nz !acc

    (* Subtraction : binary or decimal *)
    let _SBC m =
      let c2 =
          if Flag.get Flag.decimal && !enable_decimal then
              dec_to_bcd (100 - (bcd_to_dec !!m) - 1)
          else (!!m lxor 0xFF) in
      _ADC (Location.Ref (ref c2))

    let gen_CMP r m =
      let c = r - !!m in
      Flag.update_nz c;
      Flag.set Flag.carry (c >= 0)
    let _CMP = gen_CMP !acc
    let _CPX = gen_CMP !irx
    let _CPY = gen_CMP !iry

    (* Increments & Decrements *)
    let gen_CR v m =
      let updated = (!!m + v) land 0xFF in
      m <<- updated ;
      Flag.update_nz updated

    let _INC = gen_CR 1
    let _INX _ = _INC (Location.Ref irx)
    let _INY _ = _INC (Location.Ref iry)
    let _DEC = gen_CR (-1)
    let _DEX _ = _DEC (Location.Ref irx)
    let _DEY _ = _DEC (Location.Ref iry)

    (* Shifts *)
    let gen_SHIFT r f m =
        Flag.set Flag.carry ((!!m lsr r) land 0x1 = 1);
        m <<- f !!m;
        Flag.update_nz !!m

    let _ASL = gen_SHIFT 7 (fun n -> (n * 2) land 0xFF)
    let _LSR = gen_SHIFT 0 (fun n -> n lsr 1)
    let _ROL = gen_SHIFT 7 (fun n -> ((n * 2) lor (Flag.geti Flag.carry)) land 0xFF)
    let _ROR = gen_SHIFT 0 (fun n -> (n / 2) lor (Flag.geti Flag.carry lsl 7))

    (* Jump and calls *)
    let _JMP m = program_counter := Location.ref m
    let _JSR = Stack.push_addr (!program_counter - 1); _JMP
    let _RTS _ = program_counter := Stack.pull_addr + 1

    (* Branches *)
    let gen_BRANCH f s m =
        if Flag.get f = s then (
            let rel = if !!m > 0x7F then
              - (((lnot !!m) + 1) land 0xFF)
              else !!m
            in
            let clip = (!program_counter + rel) land 0xFFFF in
            let cp = if (clip land 0xFF00) != (!program_counter land 0xFF00)
                then 1 else 0
            in
            cycle_count := !cycle_count + 1 + cp ;
            program_counter := clip
        )

    let _BCC = gen_BRANCH Flag.carry false
    let _BCS = gen_BRANCH Flag.carry true
    let _BEQ = gen_BRANCH Flag.zero true
    let _BMI = gen_BRANCH Flag.negative true
    let _BNE = gen_BRANCH Flag.zero false
    let _BPL = gen_BRANCH Flag.negative false
    let _BVC = gen_BRANCH Flag.overflow false
    let _BVS = gen_BRANCH Flag.overflow true

    (* Status Flag Changes *)
    let _CLC _ = Flag.set Flag.carry false
    let _CLD _ = Flag.set Flag.decimal false
    let _CLI _ = Flag.set Flag.interrupt false
    let _CLV _ = Flag.set Flag.overflow false
    let _SEC _ = Flag.set Flag.carry true
    let _SED _ = Flag.set Flag.decimal true
    let _SEI _ = Flag.set Flag.interrupt true

    (* System functions *)
    let _BRK _ =
        Stack.push_addr (!program_counter + 1);
        Stack.push !processor_status;
        Flag.set Flag.break true;
        Flag.set Flag.interrupt true;
        program_counter := mk_addr memory.(0xFFFE) memory.(0xFFFF)

    let _RTI _ =
        processor_status := Stack.pull;
        Flag.set Flag.break false;
        Flag.set Flag.reserved true;
        program_counter := Stack.pull_addr

    let _NOP _ = ()
    let _UNO = _NOP
end

let shift_and_mask v dec mask =
    let target = v lsr dec in
    target land mask

let invalid_instruction a b c =
    Printf.printf "Invalid instruction %.2X %d %d %d\n" memory.(!program_counter)
        a b c ;
    assert false

(* Addressing and instruction dispatch *)
let rec get_addressing_mode a b c =
    if c = 3 then (
        if b = 5 || b = 7 then
            get_addressing_mode a b (c-1)
        else
            get_addressing_mode a b (c-2)
    )
    else match b with
  | 0 -> begin match c with
      | 0 -> begin match a with
          | 1 -> Absolute
          | a when a >= 4 -> Immediate
          | _ -> Implicit
        end
      | 1 -> Indexed_Indirect
      | 2 -> Immediate
      | _ -> invalid_instruction a b c
    end
  | 1 -> Zero_Page
  | 2 -> begin match c with
      | 0 -> Implicit
      | 1 -> Immediate
      | 2 -> if a < 4 then Accumulator else Implicit
      | _ -> invalid_instruction a b c
    end
  | 3 -> if a = 3 && c = 0 then Indirect else Absolute
  | 4 -> begin match c with
      | 0 -> Relative
      | 1 -> Indirect_Indexed
      | _ -> invalid_instruction a b c
    end
  | 5 -> if a < 4 || a > 5 || c != 2 then Zero_Page_X else Zero_Page_Y
  | 6 -> begin match c with
      | 0 -> Implicit
      | 1 -> Absolute_Y
      | 2 -> Implicit
      | _ -> invalid_instruction a b c
    end
  | 7 -> if c = 2 && a = 5 then Absolute_Y else Absolute_X
  | _ -> invalid_instruction a b c

let rec get_instruction_fun a b c = match (c, a) with
    | 0, 0 -> List.nth [_BRK; _UNO; _PHP; _UNO; _BPL; _UNO; _CLC; _UNO] b
    | 0, 1 -> List.nth [_JSR; _BIT; _PLP; _BIT; _BMI; _UNO; _SEC; _UNO] b
    | 0, 2 -> List.nth [_RTI; _UNO; _PHA; _JMP; _BVC; _UNO; _CLI; _UNO] b
    | 0, 3 -> List.nth [_RTS; _UNO; _PLA; _JMP; _BVS; _UNO; _SEI; _UNO] b
    | 0, 4 -> List.nth [_UNO; _STY; _DEY; _STY; _BCC; _STY; _TYA; _UNO] b
    | 0, 5 -> List.nth [_LDY; _LDY; _TAY; _LDY; _BCS; _LDY; _CLV; _LDY] b
    | 0, 6 -> List.nth [_CPY; _CPY; _INY; _CPY; _BNE; _UNO; _CLD; _UNO] b
    | 0, 7 -> List.nth [_CPX; _CPX; _INX; _CPX; _BEQ; _UNO; _SED; _UNO] b
    | 1, 0 -> _ORA
    | 1, 1 -> _AND
    | 1, 2 -> _EOR
    | 1, 3 -> _ADC
    | 1, 4 -> if b = 2 then _NOP else _STA
    | 1, 5 -> _LDA
    | 1, 6 -> _CMP
    | 1, 7 -> _SBC
    | 2, 0 -> List.nth [_NOP; _ASL; _ASL; _ASL; _NOP; _ASL; _NOP; _ASL] b
    | 2, 1 -> List.nth [_NOP; _ROL; _ROL; _ROL; _NOP; _ROL; _NOP; _ROL] b
    | 2, 2 -> List.nth [_NOP; _LSR; _LSR; _LSR; _NOP; _LSR; _NOP; _LSR] b
    | 2, 3 -> List.nth [_NOP; _ROR; _ROR; _ROR; _NOP; _ROR; _NOP; _ROR] b
    | 2, 4 -> List.nth [_NOP; _STX; _TXA; _STX; _NOP; _STX; _TXS; _UNO] b
    | 2, 5 -> List.nth [_LDX; _LDX; _TAX; _LDX; _LDX; _LDX; _TSX; _LDX] b
    | 2, 6 -> List.nth [_NOP; _DEC; _DEX; _DEC; _DEC; _DEC; _NOP; _DEC] b
    | 2, 7 -> List.nth [_NOP; _INC; _NOP; _INC; _NOP; _INC; _NOP; _INC] b
    (* Unofficial *)
    | 3, 4 when b = 0 || b = 1 || b = 3 || b = 5  -> _SAX
    | 3, 0 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 1 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 2 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 3 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 6 when b != 2  && b != 1 -> get_instruction_fun a 1 c
    | 3, 7 when b != 2 && b != 1 -> get_instruction_fun a 1 c
    | 3, _ ->
        let f1 = (get_instruction_fun a b (c-1)) in
        let f2 = (get_instruction_fun a b (c-2)) in
        fun m -> f1 m; f2 m
    | _ -> assert false

let get_instr_length ins mode page_crossed a b c = 
  let sup = ref 0 in
  let rec get_template ins page_crossed a b c = match ins.name with
    (* Impl, Acc, Imm, ZP, ZPX, ZPY, RLT, ABS, ABSX, ABSY, IND, INDX, INDY *)
    | "AND" | "EOR" | "ORA" | "BIT" | "ADC" | "SBC"
    | "CMP" | "CPX" | "CPY"
    | "LDA" | "LDX" | "LDY" ->
      if mode = Absolute_X || mode = Absolute_Y || mode = Indirect_Indexed then
        sup := if page_crossed then 1 else 0 ;
                                [0; 0; 2; 3; 4; 4; 0; 4; 4; 4; 0; 6; 5]
    | "STA" | "STX"
    | "SAX" | "STY" ->          [0; 0; 0; 3; 4; 4; 0; 4; 5; 5; 0; 6; 6]
    | "TAX" | "TAY" | "TXA"
    | "INX" | "INY" | "DEX"
    | "DEY" | "CLC" | "CLD"
    | "CLI" | "CLV" | "SEC"
    | "SED" | "SEI" | "NOP"
    | "TYA" | "TSX" | "TXS" ->  [2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    | "PHA" | "PHP" ->          [3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    | "PLA" | "PLP" ->          [4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    | "JSR" | "RTS" | "RTI" ->  [6; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0]
    | "BRK" ->                  [7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    | "INC" | "DEC" | "ASL"
    | "LSR" | "ROR" | "ROL" ->  [0; 2; 0; 5; 6; 0; 0; 6; 7; 0; 0; 0; 0]
    | "JMP" ->                  [0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 5; 0; 0]
    | "BCC" | "BCS" | "BEQ"
    | "BMI" | "BNE" | "BPL"
    | "BVC" | "BVS" ->          [0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 0; 0]
    | "UNF" when c = 3 &&
        (a >= 6 || a <= 3) ->   [0; 0; 2; 5; 6; 0; 0; 6; 7; 7; 0; 8; 8]
    | "UNF" ->
            let equiv = get_instruction_fun 5 b (if c >= 2 then c - 2 else 0) in
            get_template equiv page_crossed a b c
    | _ -> assert false
  in let template = get_template ins page_crossed a b c in
  let v = match mode with
      | Implicit -> 0 | acc -> 1 | Immediate -> 2
      | Zero_Page -> 3 | Zero_Page_X -> 4 | Zero_Page_Y -> 5
      | Relative -> 6 | Absolute -> 7 | Absolute_X -> 8
      | Absolute_Y -> 9 | Indirect -> 10 | Indexed_Indirect -> 11
      | Indirect_Indexed -> 12
  in (List.nth template v) + !sup

let print_state () =
    let opcode = memory.(!program_counter) in
    let a = shift_and_mask opcode 5 0x7 in
    let b = shift_and_mask opcode 2 0x7 in
    let c = shift_and_mask opcode 0 0x3 in
    let size = addressing_mode_size @@ get_addressing_mode a b c in
    Printf.printf "%.4X  " !program_counter ;
    for i = 0 to size - 1 do Printf.printf "%.2X " memory.(!program_counter + i) done ;
    let name = (get_instruction_fun a b c).name in
    Printf.printf "\t%s" name ;
    Printf.printf "\t\t A:%.2X X:%.2X Y:%.2X P:%.2X SP:%.2X CYC:%3d\n%!"
        !acc !irx !iry !processor_status
        !stack_pointer (!cycle_count*3 mod 341)

let get_page v = v lsr 8

let fetch_instr () =
  let opcode = memory.(!program_counter) in
  let a = shift_and_mask opcode 5 0x7 in
  let b = shift_and_mask opcode 2 0x7 in
  let c = shift_and_mask opcode 0 0x3 in
  let b1 = !program_counter + 1 in
  let b2 = !program_counter + 2 in
  let v1 = memory.(b1) in
  let v2 = memory.(b2) in
  let v12 = v1 lor (v2 lsl 8) in
  let addr_mode = get_addressing_mode a b c in
  let mode_size = addressing_mode_size addr_mode in
  program_counter := !program_counter + mode_size ;
  let page_crossed = ref false in
  let arg = begin match addr_mode with
  | Implicit -> new dummy_wrapper ()
  | Accumulator -> new ref_wrapper acc
  | Immediate -> new addr_wrapper b1
  | Zero_Page -> new addr_wrapper v1
  | Zero_Page_X -> new addr_wrapper ((v1 + !irx) land 0xFF)
  | Zero_Page_Y -> new addr_wrapper ((v1 + !iry) land 0xFF)
  | Relative -> new addr_wrapper b1
  | Absolute -> new addr_wrapper v12
  | Absolute_X -> 
        if get_page v12 != get_page (!irx + v12) then
          page_crossed := true ;
        new addr_wrapper (!irx + v12)
  | Absolute_Y ->
        if get_page v12 != get_page (!iry + v12) then
          page_crossed := true ;
    new addr_wrapper (!iry + v12)
  | Indirect ->
          (* Second byte of target wrap around in page *)
          let sto_addr_hi = ((v12 + 1) land 0xFF) lor (v12 land 0xFF00) in
          new addr_wrapper (memory.(v12) lor (memory.(sto_addr_hi) lsl 8))
  | Indexed_Indirect (*X*) -> 
          let sto_addr = (v1 + !irx) land 0xFF in
          (* Second byte of target wrap around in zero page *)
          let sto_addr_hi = (sto_addr + 1) land 0xFF in
          let sto = memory.(sto_addr) lor (memory.(sto_addr_hi) lsl 8) in
          new addr_wrapper sto
  | Indirect_Indexed (*Y*) ->
          (* Second byte of target wrap around in zero page *)
        let sto_addr_hi = (v1 + 1) land 0xFF in
        let sto = memory.(v1) lor (memory.(sto_addr_hi) lsl 8) in
        if get_page sto != get_page (!iry + sto) then
            page_crossed := true ;
        new addr_wrapper (sto + !iry)
  end in
  let ins_fun = get_instruction_fun a b c in
  let cycles = get_instr_length ins_fun addr_mode !page_crossed a b c in
  cycle_count := !cycle_count + cycles ;
  (* Reserved bit always on *) 
  set_flag true `Reserved ;
  ins_fun.f arg

