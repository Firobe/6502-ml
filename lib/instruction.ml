open Stdint

module Make (M : Cpu0.MemoryMap) = struct
  module CPU = Cpu0.Make (M)

  module Location = struct
    type t =
      | None
      | Immediate of uint8
      | Register of Register.register
      | Address of uint16

    let get (st : CPU.t) = function
      | Register r -> Register.get st.reg r
      | Immediate n -> n
      | Address a -> M.read st.mem a
      | None -> assert false

    let set (st : CPU.t) l v =
      match l with
      | Register r -> Register.set st.reg r v
      | Address a -> M.write st.mem a v
      | _ -> ()

    let ref = function Address a -> a | _ -> assert false
  end

  let ( !! ) (st, a) = Location.get st a
  let ( <<- ) (st, a) = Location.set st a

  module Implementations = struct
    open Uint8
    open Utils
    module Flag = Register.Flag
    module R = Register
    include CPU

    (* Load/Store *)
    let gen_LD st r (m : Location.t) =
      let v = !!(st, m) in
      R.set st.reg r v;
      Flag.update_nz st.reg v

    let _LDA st = gen_LD st `A
    let _LDX st = gen_LD st `X
    let _LDY st = gen_LD st `Y
    let _STA st m = (st, m) <<- R.get st.reg `A
    let _STX st m = (st, m) <<- R.get st.reg `X
    let _STY st m = (st, m) <<- R.get st.reg `Y
    let _SAX st m = (st, m) <<- logand (R.get st.reg `A) (R.get st.reg `X)

    (* Register transfers *)
    let gen_T st f t =
      let fv = R.get st.reg f in
      R.set st.reg t fv;
      Flag.update_nz st.reg fv

    let _TAX st _ = gen_T st `A `X
    let _TAY st _ = gen_T st `A `Y
    let _TXA st _ = gen_T st `X `A
    let _TYA st _ = gen_T st `Y `A

    (* Stack operations *)
    let _TSX st _ = gen_T st `S `X
    let _TXS st _ = R.set st.reg `S (R.get st.reg `X)
    let _PHA st _ = Stack.push st (R.get st.reg `A)

    let _PHP st _ =
      Stack.push st (logor (R.get st.reg `P) (Flag.mask Flag.break))

    let _PLA st _ =
      R.set st.reg `A @@ Stack.pull st;
      Flag.update_nz st.reg (R.get st.reg `A)

    let _PLP st _ =
      R.set st.reg `P @@ Stack.pull st;
      Flag.set st.reg Flag.break false;
      Flag.set st.reg Flag.reserved true

    (* Logical *)
    let gen_OP st f m =
      let v = f !!(st, m) (R.get st.reg `A) in
      R.set st.reg `A v;
      Flag.update_nz st.reg v

    let _AND st = gen_OP st logand
    let _EOR st = gen_OP st logxor
    let _ORA st = gen_OP st logor

    let _BIT st m =
      let v = !!(st, m) in
      let masked = logand (R.get st.reg `A) v in
      Flag.update_zero st.reg masked;
      Flag.update_neg st.reg v;
      Flag.set st.reg Flag.overflow (get_bit v 6)

    (* Arithmetic *)
    let bcd_to_dec (b : uint8) =
      let lo = logand b 0x0Fu in
      let hi = shift_right_logical b 4 in
      lo + (hi * 10u)

    let dec_to_bcd (d : uint8) =
      let lo = rem d 10u in
      let hi = d / 10u in
      logor lo (shift_left hi 4)

    (* Addition : binary or decimal *)
    let _ADC st m =
      let v = !!(st, m) in
      let decimal = Flag.get st.reg Flag.decimal && st.enable_decimal in
      let pre = if decimal then bcd_to_dec else fun x -> x in
      let post = if decimal then dec_to_bcd else fun x -> x in
      let max = if decimal then 99U else 0xFFU in
      (* Convert ops to u16 to detect overflow *)
      let op1 = u16of8 @@ pre @@ R.get st.reg `A in
      let op2 = u16of8 @@ pre v in
      let c = u16of8 @@ Flag.geti st.reg Flag.carry in
      let sum = Uint16.(op1 + op2 + c) in
      Flag.set st.reg Flag.carry (sum > max);
      let rsum = u8of16 @@ Uint16.(rem sum (succ max)) in
      let overflow =
        zero
        <> logand (logand 0x80u (logxor v rsum)) (logxor (R.get st.reg `A) rsum)
      in
      Flag.set st.reg Flag.overflow overflow;
      let v = post rsum in
      R.set st.reg `A v;
      Flag.update_nz st.reg v

    (* Subtraction : binary or decimal *)
    let _SBC st m =
      let c2 =
        if Flag.get st.reg Flag.decimal && st.enable_decimal then
          dec_to_bcd (100u - bcd_to_dec !!(st, m) - one)
        else lognot !!(st, m)
      in
      (* probably a +1 or -1 here ?*)
      _ADC st (Location.Immediate c2)

    let gen_CMP st r m =
      let c = R.get st.reg r - !!(st, m) in
      Flag.update_nz st.reg c;
      Flag.set st.reg Flag.carry (R.get st.reg r >= !!(st, m))

    let _CMP st = gen_CMP st `A
    let _CPX st = gen_CMP st `X
    let _CPY st = gen_CMP st `Y

    (* Increments & Decrements *)
    let gen_CR st op m =
      let updated = op !!(st, m) one in
      (st, m) <<- updated;
      Flag.update_nz st.reg updated

    let _INC st = gen_CR st ( + )
    let _INX st _ = _INC st (Location.Register `X)
    let _INY st _ = _INC st (Location.Register `Y)
    let _DEC st = gen_CR st ( - )
    let _DEX st _ = _DEC st (Location.Register `X)
    let _DEY st _ = _DEC st (Location.Register `Y)

    (* Shifts *)
    let gen_SHIFT st r f m =
      let oldm = !!(st, m) in
      let nv = f oldm in
      (st, m) <<- nv;
      Flag.set st.reg Flag.carry (get_bit oldm r);
      Flag.update_nz st.reg nv

    let _ASL st m = gen_SHIFT st 7 (fun n -> shift_left n 1) m
    let _LSR st m = gen_SHIFT st 0 (fun n -> shift_right_logical n 1) m

    let _ROL st m =
      gen_SHIFT st 7
        (fun n -> logor (shift_left n 1) (Flag.geti st.reg Flag.carry))
        m

    let _ROR st m =
      gen_SHIFT st 0
        (fun n ->
          logor (shift_right_logical n 1)
            (shift_left (Flag.geti st.reg Flag.carry) 7))
        m

    (* Jump and calls *)
    let _JMP st m = PC.set st.pc @@ Location.ref m

    let _JSR st m =
      Stack.push_addr st Uint16.(pred @@ PC.get st.pc);
      _JMP st m

    let _RTS st _ = PC.set st.pc Uint16.(succ @@ Stack.pull_addr st)

    (* Branches *)
    let gen_BRANCH st f s m =
      if Flag.get st.reg f = s then (
        (* Interpret as signed *)
        let v = Int16.of_int8 @@ Int8.of_uint8 @@ !!(st, m) in
        (* Add as signed operation *)
        let next = Int16.(of_uint16 (PC.get st.pc) + v) in
        (* Back to unsigned *)
        let unext = Uint16.of_int16 next in
        let cp = if get_hi unext <> get_hi (PC.get st.pc) then 1 else 0 in
        Stdlib.(st.cycle_count <- st.cycle_count + 1 + cp);
        PC.set st.pc unext)

    let _BCC st = gen_BRANCH st Flag.carry false
    let _BCS st = gen_BRANCH st Flag.carry true
    let _BEQ st = gen_BRANCH st Flag.zero true
    let _BMI st = gen_BRANCH st Flag.negative true
    let _BNE st = gen_BRANCH st Flag.zero false
    let _BPL st = gen_BRANCH st Flag.negative false
    let _BVC st = gen_BRANCH st Flag.overflow false
    let _BVS st = gen_BRANCH st Flag.overflow true

    (* Status Flag Changes *)
    let _CLC st _ = Flag.set st.reg Flag.carry false
    let _CLD st _ = Flag.set st.reg Flag.decimal false
    let _CLI st _ = Flag.set st.reg Flag.interrupt false
    let _CLV st _ = Flag.set st.reg Flag.overflow false
    let _SEC st _ = Flag.set st.reg Flag.carry true
    let _SED st _ = Flag.set st.reg Flag.decimal true
    let _SEI st _ = Flag.set st.reg Flag.interrupt true

    (* System functions *)
    let _BRK st _ =
      Stack.push_addr st (Uint16.succ @@ PC.get st.pc);
      Flag.set st.reg Flag.break true;
      Stack.push st (R.get st.reg `P);
      Flag.set st.reg Flag.interrupt true;
      PC.set st.pc
      @@ mk_addr
           ~lo:(M.read st.mem (u16 0xFFFE))
           ~hi:(M.read st.mem (u16 0xFFFF))

    let _RTI st _ =
      R.set st.reg `P @@ Stack.pull st;
      Flag.set st.reg Flag.break false;
      Flag.set st.reg Flag.reserved true;
      PC.set st.pc @@ Stack.pull_addr st

    let _NOP _ _ = ()
    let _NYI name _ _ = Printf.printf "%s not yet implemented" name

    (* Unofficial instructions *)
    let compose f1 f2 a b =
      f1 a b;
      f2 a b

    (* instruction crashes the CPU *)
    let _JAM _ _ = failwith "Executed JAM instruction"
    let _SLO = compose _ASL _ORA
    let _RLA = compose _ROL _AND
    let _SRE = compose _LSR _EOR
    let _RRA = compose _ROR _ADC
    let _LAX = compose _LDX _LDA
    let _LAS = compose _TSX _LDA
    let _DCP = compose _DEC _CMP
    let _ISB = compose _INC _SBC

    (* aka SAY or SYA, unstable *)
    let _SHY st m =
      let addr = Location.ref m in
      let addr = get_hi addr + 1u in
      let result = logand addr (R.get st.reg `Y) in
      (st, m) <<- result

    (* aka XAS or SXA, unstable *)
    let _SHX st m =
      let addr = Location.ref m in
      let addr = get_hi addr + 1u in
      let result = logand addr (R.get st.reg `X) in
      (st, m) <<- result

    let _ANC st m =
      _AND st m;
      Flag.set st.reg Flag.carry (Flag.get st.reg Flag.negative)

    (* aka ALR *)
    let _ASR st m =
      _AND st m;
      _LSR st (Register `A)

    (* aka AXS, SAX, ASX *)
    let _SBX st m =
      let v = !!(st, m) in
      let x = logand (R.get st.reg `X) (R.get st.reg `A) in
      Flag.(set st.reg carry (x >= v));
      let x = x - v in
      R.set st.reg `X x;
      Flag.update_nz st.reg x

    (* wrong? *)
    let _ARR st m =
      let result = logand !!(st, m) (R.get st.reg `A) in
      let result = shift_right_logical result 1 in
      let result =
        if Flag.get st.reg Flag.carry then logor result 0b10000000u else result
      in
      Flag.(set st.reg negative (get st.reg carry));
      Flag.update_neg st.reg result;
      if Flag.(get st.reg decimal) then
        (* TODO special operations in decimal mode *)
        ()
      else (
        Flag.(set st.reg carry (get_bit result 6));
        let v =
          logor (shift_right_logical result 6) (shift_right_logical result 5)
        in
        Flag.(set st.reg Flag.overflow (get_bit v 0)));
      R.set st.reg `A result

    (* unstable *)
    let _LXA st m =
      _AND st m;
      R.set st.reg `X (R.get st.reg `A)

    (* aka ATX, XAA, unstable, not implemented *)
    let _ANE = _NYI "ANE"

    (* aka SAH, AXA, unstable, not implemented *)
    let _SHA = _NYI "SHA"

    (* aka SSH, TAS, XAS, unstable, not implemented *)
    let _SHS = _NYI "SHS"
  end

  (* Organized like so:
   * https://www.masswerk.at/6502/6502_instruction_set.html#layout *)
  (* Addressed by c, a, b *)
  let layout =
    let open Implementations in
    [|
      [|
        [|_BRK,7; _NOP,0; _PHP,3; _NOP,0; _BPL,9; _NOP,0; _CLC,2; _NOP,0|];
        [|_JSR,5; _BIT,0; _PLP,4; _BIT,0; _BMI,9; _NOP,0; _SEC,2; _NOP,0|];
        [|_RTI,5; _NOP,0; _PHA,3; _JMP,8; _BVC,9; _NOP,0; _CLI,2; _NOP,0|];
        [|_RTS,5; _NOP,0; _PLA,4; _JMP,8; _BVS,9; _NOP,0; _SEI,2; _NOP,0|];
        [|_NOP,0; _STY,1; _DEY,2; _STY,1; _BCC,9; _STY,1; _TYA,2; _SHY,0|];
        [|_LDY,0; _LDY,0; _TAY,2; _LDY,0; _BCS,9; _LDY,0; _CLV,2; _LDY,0|];
        [|_CPY,0; _CPY,0; _INY,2; _CPY,0; _BNE,9; _NOP,0; _CLD,2; _NOP,0|];
        [|_CPX,0; _CPX,0; _INX,2; _CPX,0; _BEQ,9; _NOP,0; _SED,2; _NOP,0|]
      |];
      [|
        [|_ORA,0; _ORA,0; _ORA,0; _ORA,0; _ORA,0; _ORA,0; _ORA,0; _ORA,0|];
        [|_AND,0; _AND,0; _AND,0; _AND,0; _AND,0; _AND,0; _AND,0; _AND,0|];
        [|_EOR,0; _EOR,0; _EOR,0; _EOR,0; _EOR,0; _EOR,0; _EOR,0; _EOR,0|];
        [|_ADC,0; _ADC,0; _ADC,0; _ADC,0; _ADC,0; _ADC,0; _ADC,0; _ADC,0|];
        [|_STA,1; _STA,1; _NOP,2; _STA,1; _STA,1; _STA,1; _STA,1; _STA,1|];
        [|_LDA,0; _LDA,0; _LDA,0; _LDA,0; _LDA,0; _LDA,0; _LDA,0; _LDA,0|];
        [|_CMP,0; _CMP,0; _CMP,0; _CMP,0; _CMP,0; _CMP,0; _CMP,0; _CMP,0|];
        [|_SBC,0; _SBC,0; _SBC,0; _SBC,0; _SBC,0; _SBC,0; _SBC,0; _SBC,0|];
      |];
      [|
        [|_JAM,2; _ASL,7; _ASL,7; _ASL,7; _JAM,2; _ASL,7; _NOP,2; _ASL,7|];
        [|_JAM,2; _ROL,7; _ROL,7; _ROL,7; _JAM,2; _ROL,7; _NOP,2; _ROL,7|];
        [|_JAM,2; _LSR,7; _LSR,7; _LSR,7; _JAM,2; _LSR,7; _NOP,2; _LSR,7|];
        [|_JAM,2; _ROR,7; _ROR,7; _ROR,7; _JAM,2; _ROR,7; _NOP,2; _ROR,7|];
        [|_NOP,2; _STX,1; _TXA,2; _STX,1; _JAM,2; _STX,1; _TXS,2; _SHX,0|];
        [|_LDX,0; _LDX,0; _TAX,2; _LDX,0; _JAM,0; _LDX,0; _TSX,2; _LDX,0|];
        [|_NOP,2; _DEC,7; _DEX,2; _DEC,7; _JAM,7; _DEC,7; _NOP,2; _DEC,7|];
        [|_NOP,2; _INC,7; _NOP,2; _INC,7; _JAM,2; _INC,7; _NOP,2; _INC,7|]
      |];
      [|
        [|_SLO,-1; _SLO,-1; _ANC,02; _SLO,-1; _SLO,-1; _SLO,-1; _SLO,-1; _SLO,-1|];
        [|_RLA,-1; _RLA,-1; _ANC,02; _RLA,-1; _RLA,-1; _RLA,-1; _RLA,-1; _RLA,-1|];
        [|_SRE,-1; _SRE,-1; _ASR,00; _SRE,-1; _SRE,-1; _SRE,-1; _SRE,-1; _SRE,-1|];
        [|_RRA,-1; _RRA,-1; _ARR,00; _RRA,-1; _RRA,-1; _RRA,-1; _RRA,-1; _RRA,-1|];
        [|_SAX,01; _SAX,01; _ANE,00; _SAX,01; _SHA,00; _SAX,01; _SHS,00; _SHA,00|];
        [|_LAX,-1; _LAX,-1; _LXA,02; _LAX,-1; _LAX,-1; _LAX,-1; _LAS,-1; _LAX,-1|];
        [|_DCP,-1; _DCP,-1; _SBX,02; _DCP,-1; _DCP,-1; _DCP,-1; _DCP,-1; _DCP,-1|];
        [|_ISB,-1; _ISB,-1; _SBC,00; _ISB,-1; _ISB,-1; _ISB,-1; _ISB,-1; _ISB,-1|];
      |]
    |][@ocamlformat "disable"]

  (* Get instruction and cycle offset from opcode *)
  let of_opcode_triple (a, b, c) = layout.(c).(a).(b)
end
