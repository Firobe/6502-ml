open Stdint

exception Invalid_instruction of uint16 * uint8

module Int_utils = struct
  let u8 = Uint8.of_int
  let u16 = Uint16.of_int
  let u8of16 = Uint8.of_uint16
  let u16of8 = Uint16.of_uint8
  let pp_u8 fmt u = Format.fprintf fmt "%.2X" (Uint8.to_int u)
  let pp_u16 fmt u = Format.fprintf fmt "%.4X" (Uint16.to_int u)

  let mk_addr ~hi ~lo =
    let lo = u16of8 lo in
    let hi = u16of8 hi in
    Uint16.(logor (shift_left hi 8) lo)

  let get_hi (addr : uint16) = u8of16 Uint16.(shift_right_logical addr 8)
  let get_lo (addr : uint16) = u8of16 addr
  let get_bit (x : uint8) n = Uint8.(one = logand (shift_right_logical x n) one)
end

module type MemoryMap = sig
  type t
  type input

  val create : input -> t
  val read : t -> uint16 -> uint8
  val write : t -> uint16 -> uint8 -> unit
end

module IRQ_collector = struct
  type key = string
  type t = (key, bool) Hashtbl.t

  let is_pulled t = Hashtbl.fold (fun _ x a -> x || a) t false
  let set_pulled t id b = Hashtbl.replace t id b
  let create () = Hashtbl.create 3
end

module NMI = struct
  type t = { mutable flip_flop : bool }

  let create () = { flip_flop = false }
  let pull t = t.flip_flop <- true
  let check t = t.flip_flop
  let clear t = t.flip_flop <- false
end

module type CPU = sig
  type mem
  type input

  module Register : sig
    type register = [ `S | `A | `X | `Y | `P ]
    type t

    val get : t -> register -> uint8
    val set : t -> register -> uint8 -> unit
  end

  module PC : sig
    type t

    val get : t -> uint16
    val set : t -> uint16 -> unit
    val init : t -> mem -> unit
  end

  type t

  val create : ?collector:IRQ_collector.t -> ?nmi:NMI.t -> input -> t
  val pc : t -> PC.t
  val registers : t -> Register.t
  val memory : t -> mem
  val enable_decimal : t -> bool -> unit
  val cycle_count : t -> int
  val next_cycle : t -> unit
  val reset : t -> unit
  val nmi : t -> unit
  val print_state : t -> unit
end

module Make (M : MemoryMap) = struct
  type input = M.input
  type mem = M.t

  module Register = struct
    type t = {
      mutable stack_pointer : uint8;
      mutable acc : uint8;
      mutable irx : uint8;
      mutable iry : uint8;
      mutable processor_status : uint8;
    }

    type register = [ `S | `A | `X | `Y | `P ]

    let create () =
      Uint8.
        {
          stack_pointer = 0xFFu;
          acc = zero;
          irx = zero;
          iry = zero;
          processor_status = 0x24u;
        }

    (* Registers *)
    open Uint8

    let get t = function
      | `S -> t.stack_pointer
      | `A -> t.acc
      | `X -> t.irx
      | `Y -> t.iry
      | `P -> t.processor_status

    let set t r v =
      match r with
      | `S -> t.stack_pointer <- v
      | `A -> t.acc <- v
      | `X -> t.irx <- v
      | `Y -> t.iry <- v
      | `P -> t.processor_status <- v

    let incr t r = set t r (succ (get t r))
    let decr t r = set t r (pred (get t r))
  end

  module R = Register
  open Int_utils

  module PC = struct
    type t = { mutable value : uint16 }

    let create () = { value = 0x0400U }
    let get t = t.value
    let set t v = t.value <- v

    let init t mem =
      t.value <- mk_addr ~hi:(M.read mem 0xFFFDU) ~lo:(M.read mem 0xFFFCU)

    let reset t = t.value <- 0x0400U
  end

  type t = {
    mem : M.t;
    reg : Register.t;
    pc : PC.t;
    irq_collector : IRQ_collector.t;
    nmi : NMI.t;
    mutable enable_decimal : bool;
    mutable cycle_count : int;
  }

  let create ?(collector = IRQ_collector.create ()) ?(nmi = NMI.create ()) rom =
    {
      mem = M.create rom;
      reg = Register.create ();
      pc = PC.create ();
      irq_collector = collector;
      nmi;
      enable_decimal = true;
      cycle_count = 0;
    }

  let pc st = st.pc
  let registers st = st.reg
  let memory st = st.mem
  let enable_decimal st b = st.enable_decimal <- b
  let cycle_count st = st.cycle_count

  module Stack = struct
    let total_addr t = mk_addr ~hi:0x01u ~lo:(R.get t.reg `S)

    let push t v =
      (* Addr = 0x01XX *)
      M.write t.mem (total_addr t) v;
      R.decr t.reg `S

    let push_addr t v =
      push t (get_hi v);
      push t (get_lo v)

    let pull t =
      R.incr t.reg `S;
      M.read t.mem (total_addr t)

    let pull_addr t =
      let lo = pull t in
      let hi = pull t in
      mk_addr ~lo ~hi
  end

  let reset t =
    let open Uint8 in
    for i = 0 to 0xFFFF do
      M.write t.mem (u16 i) zero
    done;
    t.enable_decimal <- true;
    t.cycle_count <- 0;
    PC.reset t.pc;
    R.set t.reg `A zero;
    R.set t.reg `X zero;
    R.set t.reg `Y zero;
    R.set t.reg `P 0x24u

  module Location = struct
    type t =
      | None
      | Immediate of uint8
      | Register of Register.register
      | Address of uint16

    let get st = function
      | Register r -> R.get st.reg r
      | Immediate n -> n
      | Address a -> M.read st.mem a
      | None -> assert false

    let set st l v =
      match l with
      | Register r -> R.set st.reg r v
      | Address a -> M.write st.mem a v
      | _ -> ()

    let ref = function Address a -> a | _ -> assert false
  end

  let ( !! ) (st, a) = Location.get st a
  let ( <<- ) (st, a) = Location.set st a

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
    | Implicit | Accumulator -> 1
    | Immediate | Zero_Page | Zero_Page_X | Zero_Page_Y | Indexed_Indirect
    | Indirect_Indexed | Relative ->
        2
    | Absolute | Absolute_X | Absolute_Y | Indirect -> 3

  module Flag = struct
    open Uint8

    type t = Mask of uint8

    let mkflag n = Mask (shift_left one n)
    let carry = mkflag 0
    let zero = mkflag 1
    let interrupt = mkflag 2 (* DISABLES interrupts when off *)
    let decimal = mkflag 3
    let break = mkflag 4
    let reserved = mkflag 5
    let overflow = mkflag 6
    let negative = mkflag 7
    let mask (Mask m) = m

    let set st (Mask m) v =
      let p = R.get st.reg `P in
      let f = if v then logor p m else logand p (lognot m) in
      R.set st.reg `P f

    let get st (Mask m) =
      if logand m (R.get st.reg `P) <> Uint8.zero then true else false

    let geti st m = if get st m then one else Uint8.zero
    let update_zero t v = set t zero (v = Uint8.zero)
    let update_neg t v = set t negative (get_bit v 7)

    let update_nz t v =
      update_zero t v;
      update_neg t v
  end

  (* List of instructions *)
  module Instruction = struct
    open Uint8

    (* Load/Store *)
    let gen_LD st r (m : Location.t) =
      let v = !!(st, m) in
      R.set st.reg r v;
      Flag.update_nz st v

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
      Flag.update_nz st fv

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
      Flag.update_nz st (R.get st.reg `A)

    let _PLP st _ =
      R.set st.reg `P @@ Stack.pull st;
      Flag.set st Flag.break false;
      Flag.set st Flag.reserved true

    (* Logical *)
    let gen_OP st f m =
      let v = f !!(st, m) (R.get st.reg `A) in
      R.set st.reg `A v;
      Flag.update_nz st v

    let _AND st = gen_OP st logand
    let _EOR st = gen_OP st logxor
    let _ORA st = gen_OP st logor

    let _BIT st m =
      let v = !!(st, m) in
      let masked = logand (R.get st.reg `A) v in
      Flag.update_zero st masked;
      Flag.update_neg st v;
      Flag.set st Flag.overflow (get_bit v 6)

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
      let decimal = Flag.get st Flag.decimal && st.enable_decimal in
      let pre = if decimal then bcd_to_dec else fun x -> x in
      let post = if decimal then dec_to_bcd else fun x -> x in
      let max = if decimal then 99U else 0xFFU in
      (* Convert ops to u16 to detect overflow *)
      let op1 = u16of8 @@ pre @@ R.get st.reg `A in
      let op2 = u16of8 @@ pre v in
      let c = u16of8 @@ Flag.geti st Flag.carry in
      let sum = Uint16.(op1 + op2 + c) in
      Flag.set st Flag.carry (sum > max);
      let rsum = u8of16 @@ Uint16.(rem sum (succ max)) in
      let overflow =
        zero
        <> logand (logand 0x80u (logxor v rsum)) (logxor (R.get st.reg `A) rsum)
      in
      Flag.set st Flag.overflow overflow;
      let v = post rsum in
      R.set st.reg `A v;
      Flag.update_nz st v

    (* Subtraction : binary or decimal *)
    let _SBC st m =
      let c2 =
        if Flag.get st Flag.decimal && st.enable_decimal then
          dec_to_bcd (100u - bcd_to_dec !!(st, m) - one)
        else lognot !!(st, m)
      in
      (* probably a +1 or -1 here ?*)
      _ADC st (Location.Immediate c2)

    let gen_CMP st r m =
      let c = R.get st.reg r - !!(st, m) in
      Flag.update_nz st c;
      Flag.set st Flag.carry (R.get st.reg r >= !!(st, m))

    let _CMP st = gen_CMP st `A
    let _CPX st = gen_CMP st `X
    let _CPY st = gen_CMP st `Y

    (* Increments & Decrements *)
    let gen_CR st op m =
      let updated = op !!(st, m) one in
      (st, m) <<- updated;
      Flag.update_nz st updated

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
      Flag.set st Flag.carry (get_bit oldm r);
      Flag.update_nz st nv

    let _ASL st m = gen_SHIFT st 7 (fun n -> shift_left n 1) m
    let _LSR st m = gen_SHIFT st 0 (fun n -> shift_right_logical n 1) m

    let _ROL st m =
      gen_SHIFT st 7
        (fun n -> logor (shift_left n 1) (Flag.geti st Flag.carry))
        m

    let _ROR st m =
      gen_SHIFT st 0
        (fun n ->
          logor (shift_right_logical n 1)
            (shift_left (Flag.geti st Flag.carry) 7))
        m

    (* Jump and calls *)
    let _JMP st m = PC.set st.pc @@ Location.ref m

    let _JSR st m =
      Stack.push_addr st Uint16.(pred @@ PC.get st.pc);
      _JMP st m

    let _RTS st _ = PC.set st.pc Uint16.(succ @@ Stack.pull_addr st)

    (* Branches *)
    let gen_BRANCH st f s m =
      if Flag.get st f = s then (
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
    let _CLC st _ = Flag.set st Flag.carry false
    let _CLD st _ = Flag.set st Flag.decimal false
    let _CLI st _ = Flag.set st Flag.interrupt false
    let _CLV st _ = Flag.set st Flag.overflow false
    let _SEC st _ = Flag.set st Flag.carry true
    let _SED st _ = Flag.set st Flag.decimal true
    let _SEI st _ = Flag.set st Flag.interrupt true

    (* System functions *)
    let _BRK st _ =
      Stack.push_addr st (Uint16.succ @@ PC.get st.pc);
      Flag.set st Flag.break true;
      Stack.push st (R.get st.reg `P);
      Flag.set st Flag.interrupt true;
      PC.set st.pc
      @@ mk_addr
           ~lo:(M.read st.mem (u16 0xFFFE))
           ~hi:(M.read st.mem (u16 0xFFFF))

    let _RTI st _ =
      R.set st.reg `P @@ Stack.pull st;
      Flag.set st Flag.break false;
      Flag.set st Flag.reserved true;
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
      Flag.set st Flag.carry (Flag.get st Flag.negative)

    (* aka ALR *)
    let _ASR st m =
      _AND st m;
      _LSR st (Register `A)

    (* aka AXS, SAX, ASX *)
    let _SBX st m =
      let v = !!(st, m) in
      let x = logand (R.get st.reg `X) (R.get st.reg `A) in
      Flag.(set st carry (x >= v));
      let x = x - v in
      R.set st.reg `X x;
      Flag.update_nz st x

    (* wrong? *)
    let _ARR st m =
      let result = logand !!(st, m) (R.get st.reg `A) in
      let result = shift_right_logical result 1 in
      let result =
        if Flag.get st Flag.carry then logor result 0b10000000u else result
      in
      Flag.(set st negative (get st carry));
      Flag.update_neg st result;
      if Flag.(get st decimal) then
        (* TODO special operations in decimal mode *)
        ()
      else (
        Flag.(set st carry (get_bit result 6));
        let v =
          logor (shift_right_logical result 6) (shift_right_logical result 5)
        in
        Flag.(set st Flag.overflow (get_bit v 0)));
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

    module Decoding = struct
      (* Return (a, b, c) from the opcode aaabbbcc *)
      let triple (opcode : uint8) =
        let open Uint8 in
        let ib = shift_right_logical opcode 2 in
        let ia = shift_right_logical opcode 5 in
        ( to_int @@ logand ia 7u,
          to_int @@ logand ib 7u,
          to_int @@ logand opcode 3u )

      open Stdlib

      (* Addressed by b, c, a *)
      let am_table =
        let impl = Implicit in
        let zpg_ = Zero_Page in
        let abs_ = Absolute in
        let rel_ = Relative in
        let zpgx = Zero_Page_X in
        let zpgy = Zero_Page_Y in
        let absx = Absolute_X in
        let absy = Absolute_Y in
        let imm_ = Immediate in
        let xind = Indexed_Indirect in
        let ind_ = Indirect in
        let indy = Indirect_Indexed in
        let jam_ = Implicit in
        let acc_ = Accumulator in
        [|
          [|
            [|impl; abs_; impl; impl; imm_; imm_; imm_; imm_|];
            [|xind; xind; xind; xind; xind; xind; xind; xind|];
            [|jam_; jam_; jam_; jam_; imm_; imm_; imm_; imm_|];
            [|xind; xind; xind; xind; xind; xind; xind; xind|]
          |];
          [|
            [|zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_|];
            [|zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_|];
            [|zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_|];
            [|zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_|]
          |];
          [|
            [|impl; impl; impl; impl; impl; impl; impl; impl|];
            [|imm_; imm_; imm_; imm_; imm_; imm_; imm_; imm_|];
            [|acc_; acc_; acc_; acc_; impl; impl; impl; impl|];
            [|imm_; imm_; imm_; imm_; imm_; imm_; imm_; imm_|]
          |];
          [|
            [|abs_; abs_; abs_; ind_; abs_; abs_; abs_; abs_|];
            [|abs_; abs_; abs_; abs_; abs_; abs_; abs_; abs_|];
            [|abs_; abs_; abs_; abs_; abs_; abs_; abs_; abs_|];
            [|abs_; abs_; abs_; abs_; abs_; abs_; abs_; abs_|]
          |];
          [|
            [|rel_; rel_; rel_; rel_; rel_; rel_; rel_; rel_|];
            [|indy; indy; indy; indy; indy; indy; indy; indy|];
            [|jam_; jam_; jam_; jam_; jam_; jam_; jam_; jam_|];
            [|indy; indy; indy; indy; indy; indy; indy; indy|]
          |];
          [|
            [|zpgx; zpgx; zpgx; zpgx; zpgx; zpgx; zpgx; zpgx|];
            [|zpgx; zpgx; zpgx; zpgx; zpgx; zpgx; zpgx; zpgx|];
            [|zpgx; zpgx; zpgx; zpgx; zpgy; zpgy; zpgx; zpgx|];
            [|zpgx; zpgx; zpgx; zpgx; zpgy; zpgy; zpgx; zpgx|]
          |];
          [|
            [|impl; impl; impl; impl; impl; impl; impl; impl|];
            [|absy; absy; absy; absy; absy; absy; absy; absy|];
            [|impl; impl; impl; impl; impl; impl; impl; impl|];
            [|absy; absy; absy; absy; absy; absy; absy; absy|]
          |];
          [|
            [|absx; absx; absx; absx; absx; absx; absx; absx|];
            [|absx; absx; absx; absx; absx; absx; absx; absx|];
            [|absx; absx; absx; absx; absy; absy; absx; absx|];
            [|absx; absx; absx; absx; absy; absy; absx; absx|]
          |]
        |][@ocamlformat "disable"]

      (* Addressing and instruction dispatch *)
      let get_am (a, b, c) = am_table.(b).(c).(a)

      (* Organized like so:
       * https://www.masswerk.at/6502/6502_instruction_set.html#layout *)
      (* Addressed by c, a, b *)
      let instr_table =
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
      let get_fun (a, b, c) = instr_table.(c).(a).(b)

      (* Precompute number of cycles taken, official *)
      let cycle_functions =
        let c0 pc = function
          | Immediate -> 2
          | Zero_Page -> 3
          | Zero_Page_X | Zero_Page_Y | Absolute -> 4
          | Absolute_X | Absolute_Y -> if pc then 5 else 4
          | Indirect_Indexed -> if pc then 6 else 5
          | _ -> 6
        in
        let c1 _ = function
          | Absolute_X | Absolute_Y -> 5
          | Indirect_Indexed -> 6
          | o -> c0 false o
        in
        let c2 _ _ = 2 in
        let c3 _ _ = 3 in
        let c4 _ _ = 4 in
        let c5 _ _ = 6 in
        let c6 _ _ = 7 in
        let c7 _ = function
          | Accumulator -> 2
          | Zero_Page -> 5
          | Zero_Page_X | Absolute -> 6
          | _ -> 7
        in
        let c8 _ = function Absolute -> 3 | _ -> 5 in
        let c9 _ _ = 2 in
        [| c0; c1; c2; c3; c4; c5; c6; c7; c8; c9 |]

      (* Simulate cycles from unofficial opcodes *)
      let unofCycles (a, b, c) pc am =
        if (c = 3 && a >= 6) || a <= 3 then
          match am with
          | Immediate -> 2
          | Zero_Page -> 5
          | Zero_Page_X | Absolute -> 6
          | Absolute_X | Absolute_Y -> 7
          | _ -> 8
        else
          let _, cfi = get_fun (5, b, if c >= 2 then c - 2 else 0) in
          cycle_functions.(cfi) pc am

      (* Return instruction operation, cycle function and addressing mode of
       * opcode *)
      let decode (opcode : uint8) =
        let triple = triple opcode in
        let f, cfi = get_fun triple in
        let cf =
          if cfi = -1 then unofCycles triple else cycle_functions.(cfi)
        in
        let am = get_am triple in
        (f, cf, am)
    end
  end

  (* Returns packaged location and if page was crossed *)
  let package_arg st am =
    let get_page = get_hi in
    let pc = PC.get st.pc in
    let b1 = Uint16.(pc + 1U) in
    let b2 = Uint16.(pc + 2U) in
    let v1 = M.read st.mem b1 in
    let v2 = M.read st.mem b2 in
    let v12 = mk_addr ~hi:v2 ~lo:v1 in
    let absolute r =
      let v = R.get st.reg r in
      let total = Uint16.(of_uint8 v + v12) in
      (Location.Address total, get_page v12 <> get_page total)
    in
    match am with
    | Implicit -> (Location.None, false)
    | Accumulator -> (Location.Register `A, false)
    | Immediate -> (Location.Immediate v1, false)
    | Zero_Page -> (Location.Address (u16of8 v1), false)
    | Zero_Page_X ->
        (Location.Address (u16of8 Uint8.(v1 + R.get st.reg `X)), false)
    | Zero_Page_Y ->
        (Location.Address (u16of8 Uint8.(v1 + R.get st.reg `Y)), false)
    | Relative -> (Location.Immediate v1, false)
    | Absolute -> (Location.Address v12, false)
    | Absolute_X -> absolute `X
    | Absolute_Y -> absolute `Y
    | Indirect ->
        (* Second byte of target wrap around in page *)
        let sto_addr_hi =
          mk_addr ~hi:(get_hi v12) ~lo:Uint8.(succ @@ get_lo v12)
        in
        ( Location.Address
            (mk_addr ~hi:(M.read st.mem sto_addr_hi) ~lo:(M.read st.mem v12)),
          false )
    | Indexed_Indirect (*X*) ->
        let sto_addr = Uint8.(v1 + R.get st.reg `X) in
        (* Second byte of target wrap around in zero page *)
        let sto_addr_hi = u16of8 @@ Uint8.(succ sto_addr) in
        ( Location.Address
            (mk_addr
               ~hi:(M.read st.mem sto_addr_hi)
               ~lo:(M.read st.mem @@ u16of8 sto_addr)),
          false )
    | Indirect_Indexed (*Y*) ->
        (* Second byte of target wrap around in zero page *)
        let sto_addr_hi = u16of8 @@ Uint8.(succ v1) in
        let sto =
          mk_addr
            ~hi:(M.read st.mem sto_addr_hi)
            ~lo:(M.read st.mem @@ u16of8 v1)
        in
        let addr = Uint16.(sto + of_uint8 (R.get st.reg `Y)) in
        (Location.Address addr, get_page sto <> get_page addr)

  let nmi st =
    Stack.push_addr st (PC.get st.pc);
    Stack.push st (R.get st.reg `P);
    Flag.set st Flag.interrupt true;
    PC.set st.pc
    @@ mk_addr ~lo:(M.read st.mem 0xFFFAU) ~hi:(M.read st.mem 0xFFFBU)

  let next_cycle st =
    (* Check for interrupts *)
    (* NMI *)
    if NMI.check st.nmi then (
      nmi st;
      NMI.clear st.nmi (* IRQ *))
    else if
      (not @@ Flag.get st Flag.interrupt)
      && IRQ_collector.is_pulled st.irq_collector
    then (
      Stack.push_addr st (PC.get st.pc);
      Stack.push st (R.get st.reg `P);
      Flag.set st Flag.interrupt true;
      PC.set st.pc
      @@ mk_addr
           ~lo:(M.read st.mem (u16 0xFFFE))
           ~hi:(M.read st.mem (u16 0xFFFF)));
    (* Continue as normal *)
    let opcode = M.read st.mem @@ PC.get st.pc in
    let f, cf, am = Instruction.Decoding.decode opcode in
    let arg, page_crossed = package_arg st am in
    let mode_size = addressing_mode_size am in
    PC.set st.pc Uint16.(PC.get st.pc + u16 mode_size);
    let cycles_elapsed = cf page_crossed am in
    st.cycle_count <- st.cycle_count + cycles_elapsed;
    (* Reserved bit always on *)
    Flag.set st Flag.reserved true;
    f st arg

  let nmi st = NMI.pull st.nmi

  let print_state st =
    let pc = PC.get st.pc in
    let opcode = M.read st.mem pc in
    let _, _, am = Instruction.Decoding.decode opcode in
    let size = addressing_mode_size am in
    Format.printf "%a  " pp_u16 pc;
    for i = 0 to size - 1 do
      Format.printf "%a " pp_u8 (M.read st.mem Uint16.(pc + u16 i))
    done;
    Format.printf "\t\t A:%a X:%a Y:%a P:%a SP:%a CYC:%3d\n%!" pp_u8
      (R.get st.reg `A) pp_u8 (R.get st.reg `X) pp_u8 (R.get st.reg `Y) pp_u8
      (R.get st.reg `P) pp_u8 (R.get st.reg `S)
      Stdlib.(st.cycle_count * 3 mod 341)
end
