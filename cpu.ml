module type Mmap = sig
  val read : int -> int
  val write : int -> int -> unit
end

module type Full = sig
  module M : Mmap

  module Register : sig
    type t = [`PC | `S | `A | `X | `Y | `P]

    val get : t -> int
    val set : t -> int -> unit
  end

  val enable_decimal : bool ref
  val cycle_count : int ref
  val fetch_instr : unit -> unit
  val print_state : unit -> unit
  val reset : unit -> unit
  val init_pc : unit -> unit
  val interrupt : unit -> unit
end

module Make (M : Mmap) = struct

  let cycle_count = ref 0
  let enable_decimal = ref true

  module Register = struct
    type t = [`PC | `S | `A | `X | `Y | `P]

    (* Registers *)
    let program_counter = ref 0x0400
    let stack_pointer = ref 0x00FF
    let acc = ref 0x00
    let irx = ref 0x00
    let iry = ref 0x00
    let processor_status = ref 0x24

    let map = function
      | `PC -> program_counter
      | `S -> stack_pointer
      | `A -> acc
      | `X -> irx
      | `Y -> iry
      | `P -> processor_status

    let get r = !(map r)
    let set r v = (map r) := v
  end
  module R = Register


  let mk_addr lo hi = lo lor (hi lsl 8)
  module Stack = struct
    let push v =
      M.write (0x0100 + R.get `S) v ;
      R.set `S @@ (R.get `S - 1) land 0xFF

    let push_addr v =
      push (v lsr 8);
      push (v land 0xFF)

    let pull () =
      R.set `S @@ (R.get `S + 1) land 0xFF ;
      M.read (0x0100 + R.get `S)

    let pull_addr () =
      let lo = pull () in
      let hi = pull () in
      mk_addr lo hi
  end

  let reset () =
    for i = 0 to 0xFFFF do M.write i 0x0 done ;
    enable_decimal := true ;
    cycle_count := 0 ;
    R.set `PC 0x0400 ;
    R.set `A 0x0 ;
    R.set `X 0x0 ;
    R.set `Y 0x0 ;
    R.set `P 0x24

  let init_pc () =
    R.set `PC @@ (M.read 0xFFFD lsl 8) lor (M.read 0xFFFC)

  module Location = struct
    type t =
      | None
      | Immediate of int
      | Register of Register.t
      | Address of int
    let get = function
      | Register r -> R.get r
      | Immediate n -> n
      | Address a -> M.read (a land 0xFFFF)
      | None -> assert false
    let set l v = match l with
      | Register r -> R.set r v
      | Address a -> M.write (a land 0xFFFF) v
      | _ -> ()
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

    let mask (Mask m) = m

    let set (Mask m) v =
      let f = if v then R.get `P lor m
        else R.get `P land (lnot m)
      in R.set `P f

    let get (Mask m) =
      if m land R.get `P <> 0 then true else false
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
      let v = !!m in
      R.set r v ;
      Flag.update_nz v

    let _LDA = gen_LD `A
    let _LDX = gen_LD `X
    let _LDY = gen_LD `Y

    let _STA m = m <<- R.get `A
    let _STX m = m <<- R.get `X
    let _STY m = m <<- R.get `Y
    let _SAX m = m <<- (R.get `A land R.get `X)

    (* Register transfers *)
    let gen_T f t =
      let fv = R.get f in
      R.set t fv ;
      Flag.update_nz fv

    let _TAX _ = gen_T `A `X
    let _TAY _ = gen_T `A `Y
    let _TXA _ = gen_T `X `A
    let _TYA _ = gen_T `Y `A

    (* Stack operations *)
    let _TSX _ = gen_T `S `X
    let _TXS _ = R.set `S (R.get `X)
    let _PHA _ = Stack.push (R.get `A)
    let _PHP _ = Stack.push (R.get `P lor (Flag.mask Flag.break))
    let _PLA _ =
      R.set `A @@ Stack.pull ();
      Flag.update_nz (R.get `A)
    let _PLP _ =
      R.set `P @@ Stack.pull ();
      Flag.set Flag.break false;
      Flag.set Flag.reserved true

    (* Logical *)
    let gen_OP f m =
      let v = f !!m (R.get `A) in
      R.set `A v;
      Flag.update_nz v

    let _AND = gen_OP (land)
    let _EOR = gen_OP (lxor)
    let _ORA = gen_OP (lor)
    let _BIT m =
      let v = !!m in
      let masked = R.get `A land v in
      Flag.update_zero masked;
      Flag.set Flag.negative ((v lsr 7) land 0x1 = 1);
      Flag.set Flag.overflow ((v lsr 6) land 0x1 = 1)

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
      let op1 = pre @@ R.get `A in
      let op2 = pre v in
      let c = Flag.geti Flag.carry in
      let sum = op1 + op2 + c in
      Flag.set Flag.carry (sum > max);
      let rsum = sum mod (max + 1) in
      let overflow = ((R.get `A lxor rsum) land (v lxor rsum) land 0x80) <> 0 in
      Flag.set Flag.overflow overflow;
      let v = post rsum in
      R.set `A v ;
      Flag.update_nz v

    (* Subtraction : binary or decimal *)
    let _SBC m =
      let c2 =
        if Flag.get Flag.decimal && !enable_decimal then
          dec_to_bcd (100 - (bcd_to_dec !!m) - 1)
        else (!!m lxor 0xFF) in
      _ADC (Location.Immediate c2)

    let gen_CMP r m =
      let c = R.get r - !!m in
      Flag.update_nz c;
      Flag.set Flag.carry (c >= 0)
    let _CMP = gen_CMP `A
    let _CPX = gen_CMP `X
    let _CPY = gen_CMP `Y

    (* Increments & Decrements *)
    let gen_CR v m =
      let updated = (!!m + v) land 0xFF in
      m <<- updated ;
      Flag.update_nz updated

    let _INC = gen_CR 1
    let _INX _ = _INC (Location.Register `X)
    let _INY _ = _INC (Location.Register `Y)
    let _DEC = gen_CR (-1)
    let _DEX _ = _DEC (Location.Register `X)
    let _DEY _ = _DEC (Location.Register `Y)

    (* Shifts *)
    let gen_SHIFT r f m =
      let oldm = !!m in
      let nv = f oldm in
      m <<- nv;
      Flag.set Flag.carry ((oldm lsr r) land 0x1 = 1);
      Flag.update_nz nv

    let _ASL m = gen_SHIFT 7 (fun n -> (n lsl 1) land 0xFF) m
    let _LSR m = gen_SHIFT 0 (fun n -> n lsr 1) m
    let _ROL m = gen_SHIFT 7 (fun n -> ((n lsl 1) lor (Flag.geti Flag.carry)) land 0xFF) m
    let _ROR m = gen_SHIFT 0 (fun n -> (n lsr 1) lor (Flag.geti Flag.carry lsl 7)) m

    (* Jump and calls *)
    let _JMP m = R.set `PC @@ Location.ref m
    let _JSR m =
      Stack.push_addr (R.get `PC - 1);
      _JMP m
    let _RTS _ = R.set `PC (Stack.pull_addr () + 1)

    (* Branches *)
    let gen_BRANCH f s m =
      if Flag.get f = s then begin
        let v = !!m in
        let rel = if v > 0x7F 
          then - (((lnot v) + 1) land 0xFF) else v in
        let clip = (R.get `PC + rel) land 0xFFFF in
        let cp = if (clip land 0xFF00) <> (R.get `PC land 0xFF00)
          then 1 else 0 in
        cycle_count := !cycle_count + 1 + cp ;
        R.set `PC clip
      end

    let _BCC : t = gen_BRANCH Flag.carry false
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
      Stack.push_addr (R.get `PC + 1);
      Flag.set Flag.break true;
      Stack.push (R.get `P) ;
      Flag.set Flag.interrupt true;
      R.set `PC @@ mk_addr (M.read 0xFFFE) (M.read 0xFFFF)

    let _RTI _ =
      R.set `P @@ Stack.pull ();
      Flag.set Flag.break false;
      Flag.set Flag.reserved true;
      R.set `PC @@ Stack.pull_addr ()

    let _NOP _ = ()
    let _UNO = _NOP

    module Decoding = struct
      let triple opcode =
        let ib = opcode lsr 2 in
        let ia = ib lsr 3 in
        (ia land 0x7, ib land 0x7, opcode land 0x3)

      let invalid_instruction a b c =
        Printf.printf "Invalid instruction %.2X %d %d %d\n"
          (M.read @@ R.get `PC) a b c ;
        assert false

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
      let rec get_fun a b c = match c with (* 6 3 1*)
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
          (fun m -> f1 m; f2 m), -1

      let unofCycles a b c pc am =
        if c = 3 && a >= 6 || a <= 3 then match am with
          | Immediate -> 2 | Zero_Page -> 5 | Zero_Page_X | Absolute -> 6
          | Absolute_X | Absolute_Y -> 7 | _ -> 8
        else
          let _, cfi = get_fun 5 b (if c >= 2 then c - 2 else 0) in
          cycFuns.(cfi) pc am

      (* Addressing and instruction dispatch *)
      let rec get_am a b c = match b, c, a with
        | (5|7), 3, _ -> get_am a b (c - 1)
        | _, 3, _ -> get_am a b (c - 2)
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
        | 4, _, _ -> invalid_instruction a b c
        | 5, c, a when a < 4 || a > 5 || c <> 2 -> Zero_Page_X
        | 5, _, _ -> Zero_Page_Y
        | 6, 1, _ -> Absolute_Y
        | 6, _, _ -> Implicit
        | _, 2, 5 -> Absolute_Y
        | _, _, _ -> Absolute_X

      let decode opcode =
        let (a, b, c) = triple opcode in
        let f, cfi = get_fun a b c in
        let cf = if cfi = -1 then unofCycles a b c else cycFuns.(cfi) in
        let am = get_am a b c in
        (f, cf, am)
    end
  end

  let print_state () =
    let pc = R.get `PC in
    let opcode = M.read pc in
    let (_, _, am) = Instruction.Decoding.decode opcode in
    let size = addressing_mode_size am in
    Printf.printf "%.4X  " pc ;
    for i = 0 to size - 1 do Printf.printf "%.2X " (M.read (pc + i)) done ;
    Printf.printf "\t\t A:%.2X X:%.2X Y:%.2X P:%.2X SP:%.2X CYC:%3d\n%!"
      (R.get `A) (R.get `X) (R.get `Y) (R.get `P) (R.get `S)
      (!cycle_count * 3 mod 341)

  let get_page v = v lsr 8
  let package_arg am =
    let b1 = R.get `PC + 1 in
    let b2 = R.get `PC + 2 in
    let v1 = M.read b1 in
    let v2 = M.read b2 in
    let v12 = v1 lor (v2 lsl 8) in
    let absolute r =
      let v = R.get r in
      Location.Address (v + v12), get_page v12 <> get_page (v + v12)
    in match am with
    | Implicit -> Location.None, false
    | Accumulator -> Location.Register `A, false
    | Immediate -> Location.Immediate v1, false
    | Zero_Page -> Location.Address v1, false
    | Zero_Page_X ->
      Location.Address ((v1 + R.get `X) land 0xFF), false
    | Zero_Page_Y ->
      Location.Address ((v1 + R.get `Y) land 0xFF), false
    | Relative -> Location.Immediate v1, false
    | Absolute -> Location.Address v12, false
    | Absolute_X -> absolute `X
    | Absolute_Y -> absolute `Y
    | Indirect ->
      (* Second byte of target wrap around in page *)
      let sto_addr_hi = ((v12 + 1) land 0xFF) lor (v12 land 0xFF00) in
      Location.Address (M.read v12 lor (M.read sto_addr_hi lsl 8)), false
    | Indexed_Indirect (*X*) -> 
      let sto_addr = (v1 + R.get `X) land 0xFF in
      (* Second byte of target wrap around in zero page *)
      let sto_addr_hi = (sto_addr + 1) land 0xFF in
      Location.Address (M.read sto_addr lor (M.read sto_addr_hi lsl 8)), false
    | Indirect_Indexed (*Y*) ->
      (* Second byte of target wrap around in zero page *)
      let sto_addr_hi = (v1 + 1) land 0xFF in
      let sto = M.read v1 lor (M.read sto_addr_hi lsl 8) in
      Location.Address (sto + R.get `Y),
      get_page sto <> get_page (R.get `Y + sto)

  let fetch_instr () =
    let opcode = M.read @@ R.get `PC in
    let (f, cf, am) = Instruction.Decoding.decode opcode in
    let (arg, pc) = package_arg am in
    let mode_size = addressing_mode_size am in
    R.set `PC (R.get `PC + mode_size) ;
    let cycles_elapsed = cf pc am in
    cycle_count := !cycle_count + cycles_elapsed ;
    (* Reserved bit always on *) 
    Flag.set Flag.reserved true;
    f arg

  let interrupt () =
    Stack.push_addr (R.get `PC) ;
    Stack.push (R.get `P) ;
    Flag.set Flag.interrupt true;
    R.set `PC @@ mk_addr (M.read 0xFFFA) (M.read 0xFFFB)

  module M = M
end
