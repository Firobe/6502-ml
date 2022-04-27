open Stdint
exception Invalid_instruction of uint16 * uint8

module Int_utils = struct
  let u8 = Uint8.of_int
  let u16 = Uint16.of_int
  let u8of16 = Uint8.of_uint16
  let u16of8 = Uint16.of_uint8
  let pp_u8 fmt u =
    Format.fprintf fmt "%.2X" (Uint8.to_int u)
  let pp_u16 fmt u =
    Format.fprintf fmt "%.4X" (Uint16.to_int u)
  let mk_addr ~hi ~lo =
    let lo = u16of8 lo in
    let hi = u16of8 hi in
    Uint16.(logor (shift_left hi 8) lo)
  let get_hi (addr : uint16) = u8of16 Uint16.(shift_right_logical addr 8)
  let get_lo (addr : uint16) = u8of16 addr
  let get_bit (x : uint8) n =
    Uint8.(one = (logand (shift_right_logical x n) one))
end

module type MemoryMap = sig
  type t
  type input
  val create : input -> t
  val read : t -> uint16 -> uint8
  val write : t -> uint16 -> uint8 -> unit
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
  val create : input -> t
  val pc : t -> PC.t
  val registers : t -> Register.t
  val memory : t -> mem
  val enable_decimal : t -> bool -> unit
  val cycle_count : t -> int
  val fetch_instr : t -> unit
  val reset : t -> unit
  val interrupt : t -> unit
  val print_state : t -> unit
end

module Make (M : MemoryMap) = struct
  module Register = struct
    type t = {
      mutable stack_pointer : uint8;
      mutable acc : uint8;
      mutable irx : uint8;
      mutable iry : uint8;
      mutable processor_status : uint8
    }

    type register = [`S | `A | `X | `Y | `P]

    let create () = Uint8.{
        stack_pointer = 0xFFu;
        acc = zero;
        irx = zero;
        iry = zero;
        processor_status = 0x24u
    }

    (* Registers *)
    open Uint8

    let get t = function
      | `S -> t.stack_pointer
      | `A -> t.acc
      | `X -> t.irx
      | `Y -> t.iry
      | `P -> t.processor_status
    let set t r v = match r with
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
    type t = {mutable value : uint16}

    let create () = {value = 0x0400U}

    let get t = t.value
    let set t v = t.value <- v

    let init t mem =
      t.value <-
        mk_addr ~hi:(M.read mem 0xFFFDU) ~lo:(M.read mem 0xFFFCU)

    let reset t = t.value <- 0x0400U
  end

  type t = {
    mem : M.t;
    reg : Register.t;
    pc : PC.t;
    mutable enable_decimal : bool;
    mutable cycle_count : int
  }

  let create rom = {
    mem = M.create rom;
    reg = Register.create ();
    pc = PC.create ();
    enable_decimal = true;
    cycle_count = 0
  }

  let pc st = st.pc
  let registers st = st.reg
  let memory st = st.mem

  let enable_decimal st b = st.enable_decimal <- b
  let cycle_count st = st.cycle_count

  module Stack = struct
    let total_addr t =
      mk_addr ~hi:0x01u ~lo:(R.get t.reg `S)

    let push t v =
      (* Addr = 0x01XX *)
      M.write t.mem (total_addr t) v ;
      R.decr t.reg `S

    let push_addr t v =
      push t (get_hi v) ;
      push t (get_lo v)

    let pull t =
      R.incr t.reg `S ;
      M.read t.mem (total_addr t)

    let pull_addr t =
      let lo = pull t in
      let hi = pull t in
      mk_addr ~lo ~hi
  end

  let reset t =
    let open Uint8 in
    for i = 0 to 0xFFFF do M.write t.mem (u16 i) zero done ;
    t.enable_decimal <- true ;
    t.cycle_count <- 0 ;
    PC.reset t.pc ;
    R.set t.reg `A zero ;
    R.set t.reg `X zero ;
    R.set t.reg `Y zero ;
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
    let set st l v = match l with
      | Register r -> R.set st.reg r v
      | Address a -> M.write st.mem a v
      | _ -> ()
    let ref = function
      | Address a -> a
      | _ -> assert false
  end

  let ( !! ) (st, a) = Location.get st a
  let ( <<- ) (st, a) = Location.set st a

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
    open Uint8
    type t = Mask of uint8
    let mkflag n = Mask (shift_left one n)
    let carry = mkflag 0
    let zero = mkflag 1
    let interrupt = mkflag 2
    let decimal = mkflag 3
    let break = mkflag 4
    let reserved = mkflag 5
    let overflow = mkflag 6
    let negative = mkflag 7

    let mask (Mask m) = m

    let set st (Mask m) v =
      let p = R.get st.reg `P in
      let f = if v then (logor p m)
        else (logand p (lognot m))
      in R.set st.reg `P f

    let get st (Mask m) =
      if (logand m (R.get st.reg `P)) <> Uint8.zero then true else false
    let geti st m = if get st m then one else Uint8.zero

    let update_zero t v = set t zero (v = Uint8.zero)
    let update_neg t v = set t negative (get_bit v 7)
    let update_nz t v = update_zero t v; update_neg t v
  end

  (* List of instructions *)
  module Instruction = struct
    open Uint8

    (* Load/Store *)
    let gen_LD st r (m : Location.t) =
      let v = !!(st,m) in
      R.set st.reg r v ;
      Flag.update_nz st v

    let _LDA st = gen_LD st `A
    let _LDX st = gen_LD st `X
    let _LDY st = gen_LD st `Y

    let _STA st m = (st,m) <<- R.get st.reg `A
    let _STX st m = (st,m) <<- R.get st.reg `X
    let _STY st m = (st,m) <<- R.get st.reg `Y
    let _SAX st m = (st,m) <<- logand (R.get st.reg `A) (R.get st.reg `X)

    (* Register transfers *)
    let gen_T st f t =
      let fv = R.get st.reg f in
      R.set st.reg t fv ;
      Flag.update_nz st fv

    let _TAX st _ = gen_T st `A `X
    let _TAY st _ = gen_T st `A `Y
    let _TXA st _ = gen_T st `X `A
    let _TYA st _ = gen_T st `Y `A

    (* Stack operations *)
    let _TSX st _ = gen_T st `S `X
    let _TXS st _ = R.set st.reg `S (R.get st.reg `X)
    let _PHA st _ = Stack.push st (R.get st.reg `A)
    let _PHP st _ = Stack.push st (logor (R.get st.reg `P) (Flag.mask Flag.break))
    let _PLA st _ =
      R.set st.reg `A @@ Stack.pull st;
      Flag.update_nz st (R.get st.reg `A)
    let _PLP st _ =
      R.set st.reg `P @@ Stack.pull st;
      Flag.set st Flag.break false;
      Flag.set st Flag.reserved true

    (* Logical *)
    let gen_OP st f m =
      let v = f !!(st,m) (R.get st.reg `A) in
      R.set st.reg `A v;
      Flag.update_nz st v

    let _AND st = gen_OP st logand
    let _EOR st = gen_OP st logxor
    let _ORA st = gen_OP st logor
    let _BIT st m =
      let v = !!(st,m) in
      let masked = logand (R.get st.reg `A) v in
      Flag.update_zero st masked;
      Flag.update_neg st v;
      Flag.set st Flag.overflow (get_bit v 6)

    (* Arithmetic *)
    let bcd_to_dec (b : uint8) = 
      let lo = logand b 0x0Fu in
      let hi = shift_right_logical b 4 in
      lo + hi * 10u
    let dec_to_bcd (d : uint8) =
      let lo = rem d 10u in
      let hi = d / 10u in
      logor lo (shift_left hi 4)

    (* Addition : binary or decimal *)
    let _ADC st m =
      let v = !!(st,m) in
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
      let overflow = zero <>
                     (logand (logand 0x80u (logxor v rsum))
                        (logxor (R.get st.reg `A) rsum)) in
      Flag.set st Flag.overflow overflow;
      let v = post rsum in
      R.set st.reg `A v ;
      Flag.update_nz st v

    (* Subtraction : binary or decimal *)
    let _SBC st m =
      let c2 =
        if Flag.get st Flag.decimal && st.enable_decimal then
          dec_to_bcd (100u - (bcd_to_dec !!(st,m)) - one)
        else (lognot !!(st,m)) in (* probably a +1 or -1 here ?*)
      _ADC st (Location.Immediate c2)

    let gen_CMP st r m =
      let c = R.get st.reg r - !!(st,m) in
      Flag.update_nz st c;
      Flag.set st Flag.carry (R.get st.reg r >= !!(st,m))
    let _CMP st = gen_CMP st `A
    let _CPX st = gen_CMP st `X
    let _CPY st = gen_CMP st `Y

    (* Increments & Decrements *)
    let gen_CR st op m =
      let updated = op !!(st,m) one in
      (st,m) <<- updated ;
      Flag.update_nz st updated

    let _INC st = gen_CR st (+)
    let _INX st _ = _INC st (Location.Register `X)
    let _INY st _ = _INC st (Location.Register `Y)
    let _DEC st = gen_CR st (-)
    let _DEX st _ = _DEC st (Location.Register `X)
    let _DEY st _ = _DEC st (Location.Register `Y)

    (* Shifts *)
    let gen_SHIFT st r f m =
      let oldm = !!(st,m) in
      let nv = f oldm in
      (st,m) <<- nv;
      Flag.set st Flag.carry (get_bit oldm r) ;
      Flag.update_nz st nv

    let _ASL st m = gen_SHIFT st 7 (fun n -> shift_left n 1) m
    let _LSR st m = gen_SHIFT st 0 (fun n -> shift_right_logical n 1) m
    let _ROL st m = gen_SHIFT st 7 (fun n ->
        logor (shift_left n 1) (Flag.geti st Flag.carry)) m
    let _ROR st m = gen_SHIFT st 0 (fun n ->
        logor (shift_right_logical n 1) (shift_left (Flag.geti st Flag.carry) 7)) m

    (* Jump and calls *)
    let _JMP st m =
      PC.set st.pc @@ Location.ref m
    let _JSR st m =
      Stack.push_addr st Uint16.(pred @@ PC.get st.pc);
      _JMP st m
    let _RTS st _ = PC.set st.pc Uint16.(succ @@ Stack.pull_addr st)

    (* Branches *)
    let gen_BRANCH st f s m =
      if Flag.get st f = s then begin
        (* Interpret as signed *)
        let v = Int16.of_int8 @@ Int8.of_uint8 @@ !!(st,m) in
        (* Add as signed operation *)
        let next = Int16.((of_uint16 (PC.get st.pc)) + v) in
        (* Back to unsigned *)
        let unext = Uint16.of_int16 next in
        let cp = if (get_hi unext) <> (get_hi (PC.get st.pc)) then 1 else 0 in
        Stdlib.(st.cycle_count <- st.cycle_count + 1 + cp);
        PC.set st.pc unext
      end

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
      Stack.push_addr st (Uint16.succ @@ PC.get st.pc) ;
      Flag.set st Flag.break true;
      Stack.push st (R.get st.reg `P) ;
      Flag.set st Flag.interrupt true;
      PC.set st.pc @@
      mk_addr ~lo:(M.read st.mem (u16 0xFFFE)) ~hi:(M.read st.mem (u16 0xFFFF))

    let _RTI st _ =
      R.set st.reg `P @@ Stack.pull st;
      Flag.set st Flag.break false;
      Flag.set st Flag.reserved true;
      PC.set st.pc @@ Stack.pull_addr st

    let _NOP _ _ = ()
    let _UNO = _NOP

    module Decoding = struct
      let triple (opcode : uint8) =
        let open Uint8 in
        let ib = shift_right_logical opcode 2 in
        let ia = shift_right_logical opcode 5 in
        (logand ia 7u, logand ib 7u, logand opcode 3u)

      let invalid_instruction st =
        let addr = PC.get st.pc in
        let opcode = M.read st.mem addr in
        raise (Invalid_instruction (addr, opcode))

      open Stdlib

      let c0 pc = function
        | Immediate -> 2 | Zero_Page -> 3
        | Zero_Page_X | Zero_Page_Y | Absolute -> 4
        | Absolute_X | Absolute_Y -> if pc then 5 else 4
        | Indirect_Indexed -> if pc then 6 else 5
        | _ -> 6
      let c1 _ = function
        | Absolute_X | Absolute_Y -> 5 | Indirect_Indexed -> 6
        | o -> c0 false o
      let c2 _ _ = 2 let c3 _ _ = 3 let c4 _ _ = 4
      let c5 _ _ = 6 let c6 _ _ = 7 let c9 _ _ = 2
      let c7 _ = function
        | Accumulator -> 2 | Zero_Page -> 5
        | Zero_Page_X | Absolute -> 6 | _ -> 7
      let c8 _ = function Absolute -> 3 | _ -> 5
      let cycFuns = [|c0; c1; c2; c3; c4; c5; c6; c7; c8; c9|]
      let t0 = [| [|_BRK,7; _UNO,0; _PHP,3; _UNO,0; _BPL,9; _UNO,0; _CLC,2; _UNO,0|];
                  [|_JSR,5; _BIT,0; _PLP,4; _BIT,0; _BMI,9; _UNO,0; _SEC,2; _UNO,0|];
                  [|_RTI,5; _UNO,0; _PHA,3; _JMP,8; _BVC,9; _UNO,0; _CLI,2; _UNO,0|];
                  [|_RTS,5; _UNO,0; _PLA,4; _JMP,8; _BVS,9; _UNO,0; _SEI,2; _UNO,0|];
                  [|_UNO,0; _STY,1; _DEY,2; _STY,1; _BCC,9; _STY,1; _TYA,2; _UNO,0|];
                  [|_LDY,0; _LDY,0; _TAY,2; _LDY,0; _BCS,9; _LDY,0; _CLV,2; _LDY,0|];
                  [|_CPY,0; _CPY,0; _INY,2; _CPY,0; _BNE,9; _UNO,0; _CLD,2; _UNO,0|];
                  [|_CPX,0; _CPX,0; _INX,2; _CPX,0; _BEQ,9; _UNO,0; _SED,2; _UNO,0|] |]
      let t1 = [|_ORA,0; _AND,0; _EOR,0; _ADC,0; _STA,1; _LDA,0; _CMP,0; _SBC,0|]
      let t2 = [| [|_NOP,2; _ASL,7; _ASL,7; _ASL,7; _NOP,2; _ASL,7; _NOP,2; _ASL,7|];
                  [|_NOP,2; _ROL,7; _ROL,7; _ROL,7; _NOP,2; _ROL,7; _NOP,2; _ROL,7|];
                  [|_NOP,2; _LSR,7; _LSR,7; _LSR,7; _NOP,2; _LSR,7; _NOP,2; _LSR,7|];
                  [|_NOP,2; _ROR,7; _ROR,7; _ROR,7; _NOP,2; _ROR,7; _NOP,2; _ROR,7|];
                  [|_NOP,2; _STX,1; _TXA,2; _STX,1; _NOP,2; _STX,1; _TXS,2; _UNO,0|];
                  [|_LDX,0; _LDX,0; _TAX,2; _LDX,0; _LDX,0; _LDX,0; _TSX,2; _LDX,0|];
                  [|_NOP,2; _DEC,7; _DEX,2; _DEC,7; _DEC,7; _DEC,7; _NOP,2; _DEC,7|];
                  [|_NOP,2; _INC,7; _NOP,2; _INC,7; _NOP,2; _INC,7; _NOP,2; _INC,7|] |]
      let rec get_fun a b c = (* 1 3 0 *)
        match c with (* 6 3 1*)
        | 0 -> t0.(a).(b)
        | 1 when a = 4 -> if b = 2 then (_NOP,2) else (_STA,1)
        | 1 -> t1.(a)
        | 2 -> t2.(a).(b)
        (* Unofficial *)
        | 3 when a = 4 && (b = 0 || b = 1 || b = 3 || b = 5)  -> (_SAX,1)
        | 3 when a <> 5 && b <> 2 && b <> 1 -> (fst (get_fun a 1 c), -1)
        | _ ->
          let f1, _ = (get_fun a b (c-1)) in
          let f2, _ = (get_fun a b (c-2)) in
          (fun st m -> f1 st m; f2 st m), -1

      let unofCycles a b c pc am =
        if c = 3 && a >= 6 || a <= 3 then match am with
          | Immediate -> 2 | Zero_Page -> 5 | Zero_Page_X | Absolute -> 6
          | Absolute_X | Absolute_Y -> 7 | _ -> 8
        else
          let _, cfi = get_fun 5 b (if c >= 2 then c - 2 else 0) in
          cycFuns.(cfi) pc am

      (* Addressing and instruction dispatch *)
      let rec get_am st a b c = match b, c, a with
        | (5|7), 3, _ -> get_am st a b (c - 1)
        | _, 3, _ -> get_am st a b (c - 2)
        | 0, 0, 1 -> Absolute
        | 0, 0, a when a >= 4 -> Immediate
        | 0, 0, _ -> Implicit
        | 0, 1, _ -> Indexed_Indirect
        | 0, _, _ -> Immediate
        | 1, _, _ -> Zero_Page
        | 2, 0, _ -> Implicit
        | 2, 1, _ -> Immediate
        | 2, _, a when a < 4 -> Accumulator
        | 2, _, _ -> Implicit
        | 3, 0, 3 -> Indirect
        | 3, _, _ -> Absolute
        | 4, 0, _ -> Relative
        | 4, 1, _ -> Indirect_Indexed
        | 4, _, _ -> invalid_instruction st
        | 5, c, a when a < 4 || a > 5 || c <> 2 -> Zero_Page_X
        | 5, _, _ -> Zero_Page_Y
        | 6, 1, _ -> Absolute_Y
        | 6, _, _ -> Implicit
        | _, 2, 5 -> Absolute_Y
        | _, _, _ -> Absolute_X

      let decode st (opcode : uint8) =
        let (a, b, c) = triple opcode in
        let (a, b, c) = Uint8.(to_int a, to_int b, to_int c) in
        let f, cfi = get_fun a b c in
        let cf = if cfi = -1 then unofCycles a b c else cycFuns.(cfi) in
        let am = get_am st a b c in
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
      let total = Uint16.((of_uint8 v) + v12) in
      Location.Address total, get_page v12 <> get_page total
    in match am with
    | Implicit -> Location.None, false
    | Accumulator -> Location.Register `A, false
    | Immediate -> Location.Immediate v1, false
    | Zero_Page -> Location.Address (u16of8 v1), false
    | Zero_Page_X ->
      Location.Address (u16of8 (Uint8.(v1 + R.get st.reg `X))), false
    | Zero_Page_Y ->
      Location.Address (u16of8 (Uint8.(v1 + R.get st.reg `Y))), false
    | Relative -> Location.Immediate v1, false
    | Absolute -> Location.Address v12, false
    | Absolute_X -> absolute `X
    | Absolute_Y -> absolute `Y
    | Indirect ->
      (* Second byte of target wrap around in page *)
      let sto_addr_hi = mk_addr ~hi:(get_hi v12) ~lo:Uint8.(succ @@ get_lo v12) in
      Location.Address (
        mk_addr ~hi:(M.read st.mem sto_addr_hi) ~lo:(M.read st.mem v12)), false
    | Indexed_Indirect (*X*) -> 
      let sto_addr = Uint8.(v1 + R.get st.reg `X) in
      (* Second byte of target wrap around in zero page *)
      let sto_addr_hi = u16of8 @@ Uint8.(succ sto_addr) in
      Location.Address (mk_addr ~hi:(M.read st.mem sto_addr_hi) 
                          ~lo:(M.read st.mem @@ u16of8 sto_addr)), false
    | Indirect_Indexed (*Y*) ->
      (* Second byte of target wrap around in zero page *)
      let sto_addr_hi = u16of8 @@ Uint8.(succ v1) in
      let sto = mk_addr ~hi:(M.read st.mem sto_addr_hi)
          ~lo:(M.read st.mem @@ u16of8 v1) in
      let addr = Uint16.(sto + of_uint8 (R.get st.reg `Y)) in
      Location.Address addr, get_page sto <> get_page addr

  let fetch_instr st =
    let opcode = M.read st.mem @@ PC.get st.pc in
    let (f, cf, am) = Instruction.Decoding.decode st opcode in
    let (arg, page_crossed) = package_arg st am in
    let mode_size = addressing_mode_size am in
    PC.set st.pc Uint16.(PC.get st.pc + (u16 mode_size)) ;
    let cycles_elapsed = cf page_crossed am in
    st.cycle_count <- st.cycle_count + cycles_elapsed ;
    (* Reserved bit always on *) 
    Flag.set st Flag.reserved true;
    f st arg

  let interrupt st =
    Stack.push_addr st (PC.get st.pc) ;
    Stack.push st (R.get st.reg `P) ;
    Flag.set st Flag.interrupt true;
    PC.set st.pc @@ mk_addr ~lo:(M.read st.mem 0xFFFAU)
      ~hi:(M.read st.mem 0xFFFBU)

  let print_state st =
    let pc = PC.get st.pc in
    let opcode = M.read st.mem pc in
    let (_, _, am) = Instruction.Decoding.decode st opcode in
    let size = addressing_mode_size am in
    Format.printf "%a  " pp_u16 pc ;
    for i = 0 to size - 1 do
      Format.printf "%a " pp_u8 (M.read st.mem Uint16.(pc + (u16 i)))
    done ;
    Format.printf "\t\t A:%a X:%a Y:%a P:%a SP:%a CYC:%3d\n%!"
     pp_u8 (R.get st.reg `A) pp_u8 (R.get st.reg `X) pp_u8 (R.get st.reg`Y)
     pp_u8 (R.get st.reg `P) pp_u8 (R.get st.reg `S)
     Stdlib.(st.cycle_count * 3 mod 341)
end
