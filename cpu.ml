open Stdint

let u8 = Uint8.of_int
let u16 = Uint16.of_int
let pp_u8 fmt u =
  Format.fprintf fmt "%.2X" (Uint8.to_int u)
let pp_u16 fmt u =
  Format.fprintf fmt "%.4X" (Uint16.to_int u)

module type Mmap = sig
  val read : uint16 -> uint8
  val write : uint16 -> uint8 -> unit
end

module type Full = sig
  module M : Mmap

  module Register : sig
    type t = [`S | `A | `X | `Y | `P]

    val get : t -> uint8
    val set : t -> uint8 -> unit
  end
  module PC : sig
    val get : unit -> uint16
    val set : uint16 -> unit
    val init : unit -> unit
  end


  val enable_decimal : bool ref
  val cycle_count : int ref
  val fetch_instr : unit -> unit
  val print_state : unit -> unit
  val reset : unit -> unit
  val interrupt : unit -> unit
end

let mk_addr ~hi ~lo =
  let lo = Uint16.of_uint8 lo in
  let hi = Uint16.of_uint8 hi in
  Uint16.(logor (shift_left hi 8) lo)
let get_hi (addr : uint16) =
  Uint16.(to_uint8 @@ shift_right_logical addr 8)
let get_lo (addr : uint16) =
  Uint16.(to_uint8 addr)
let get_bit (x : uint8) n =
    Uint8.(one = (logand (shift_right_logical x n) one))

module Make (M : Mmap) = struct

  let cycle_count = ref 0
  let enable_decimal = ref true

  module Register = struct
    type t = [`S | `A | `X | `Y | `P]

    (* Registers *)
    open Uint8
    let stack_pointer = ref (u8 0xFF)
    let acc = ref zero
    let irx = ref zero
    let iry = ref zero
    let processor_status = ref (u8 0x24)

    let map = function
      | `S -> stack_pointer
      | `A -> acc
      | `X -> irx
      | `Y -> iry
      | `P -> processor_status

    let get r = !(map r)
    let set r v = (map r) := v
    let incr r = (map r) := (succ !(map r))
    let decr r = (map r) := (pred !(map r))
  end
  module R = Register
  module PC = struct
    let program_counter = ref (u16 0x0400)

    let get () = !program_counter
    let set v = program_counter := v

    let init () =
      program_counter :=
        mk_addr ~hi:(M.read (u16 0xFFFD)) ~lo:(M.read (u16 0xFFFC))

    let reset () = set (u16 0x0400)
  end


  module Stack = struct
    let total_addr () =
      mk_addr ~hi:(u8 0x01) ~lo:(R.get `S)

    let push v =
      (* Addr = 0x01XX *)
      M.write (total_addr ()) v ;
      R.decr `S

    let push_addr v =
      push (get_hi v) ;
      push (get_lo v)

    let pull () =
      R.incr `S ;
      M.read (total_addr ())

    let pull_addr () =
      let lo = pull () in
      let hi = pull () in
      mk_addr ~lo ~hi
  end

  let reset () =
    let open Uint8 in
    for i = 0 to 0xFFFF do M.write (u16 i) zero done ;
    enable_decimal := true ;
    cycle_count := 0 ;
    PC.reset () ;
    R.set `A zero ;
    R.set `X zero ;
    R.set `Y zero ;
    R.set `P (u8 0x24)

  module Location = struct
    type t =
      | None
      | Immediate of uint8
      | Register of Register.t
      | Address of uint16
    let get = function
      | Register r -> R.get r
      | Immediate n -> n
      | Address a -> M.read a
      | None -> assert false
    let set l v = match l with
      | Register r -> R.set r v
      | Address a -> M.write a v
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

    let set (Mask m) v =
      let p = R.get `P in
      let f = if v then (logor p m)
        else (logand p (lognot m))
      in R.set `P f

    let get (Mask m) =
      if (logand m (R.get `P)) <> Uint8.zero then true else false
    let geti m = if get m then one else Uint8.zero

    let update_zero v = set zero (v = Uint8.zero)
    let update_neg v = set negative (get_bit v 7)
    let update_nz v = update_zero v; update_neg v
  end

  (* List of instructions *)
  module Instruction = struct
    open Uint8
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
    let _SAX m = m <<- logand (R.get `A) (R.get `X)

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
    let _PHP _ = Stack.push (logor (R.get `P) (Flag.mask Flag.break))
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

    let _AND = gen_OP logand
    let _EOR = gen_OP logxor
    let _ORA = gen_OP logor
    let _BIT m =
      let v = !!m in
      let masked = logand (R.get `A) v in
      Flag.update_zero masked;
      Flag.update_neg v;
      Flag.set Flag.overflow (get_bit v 6)

    (* Arithmetic *)
    let bcd_to_dec (b : uint8) = 
      let lo = logand b (u8 0x0F) in
      let hi = shift_right_logical b 4 in
      lo + hi * (u8 10)
    let dec_to_bcd (d : uint8) =
      let lo = rem d (u8 10) in
      let hi = d / (u8 10) in
      logor lo (shift_left hi 4)

    (* Addition : binary or decimal *)
    let _ADC m =
      let v = !!m in
      let decimal = Flag.get Flag.decimal && !enable_decimal in
      let pre = if decimal then bcd_to_dec else fun x -> x in
      let post = if decimal then dec_to_bcd else fun x -> x in
      let max = if decimal then (u16 99) else (u16 0xFF) in
      (* Convert ops to u16 to detect overflow *)
      let op1 = Uint16.of_uint8 @@ pre @@ R.get `A in
      let op2 = Uint16.of_uint8 @@ pre v in
      let c = Uint16.of_uint8 @@ Flag.geti Flag.carry in
      let sum = Uint16.(op1 + op2 + c) in
      Flag.set Flag.carry (sum > max);
      let rsum = Uint8.of_uint16 @@ Uint16.(rem sum (succ max)) in
      let overflow = zero <>
                     (logand (logand (u8 0x80) (logxor v rsum))
                        (logxor (R.get `A) rsum)) in
      Flag.set Flag.overflow overflow;
      let v = post rsum in
      R.set `A v ;
      Flag.update_nz v

    (* Subtraction : binary or decimal *)
    let _SBC m =
      let c2 =
        if Flag.get Flag.decimal && !enable_decimal then
          dec_to_bcd ((u8 100) - (bcd_to_dec !!m) - one)
        else (lognot !!m) in (* probably a +1 or -1 here ?*)
      _ADC (Location.Immediate c2)

    let gen_CMP r m =
      let c = R.get r - !!m in
      Flag.update_nz c;
      Flag.set Flag.carry (R.get r >= !!m)
    let _CMP = gen_CMP `A
    let _CPX = gen_CMP `X
    let _CPY = gen_CMP `Y

    (* Increments & Decrements *)
    let gen_CR op m =
      let updated = op !!m one in
      m <<- updated ;
      Flag.update_nz updated

    let _INC = gen_CR (+)
    let _INX _ = _INC (Location.Register `X)
    let _INY _ = _INC (Location.Register `Y)
    let _DEC = gen_CR (-)
    let _DEX _ = _DEC (Location.Register `X)
    let _DEY _ = _DEC (Location.Register `Y)

    (* Shifts *)
    let gen_SHIFT r f m =
      let oldm = !!m in
      let nv = f oldm in
      m <<- nv;
      Flag.set Flag.carry (get_bit oldm r) ;
      Flag.update_nz nv

    let _ASL m = gen_SHIFT 7 (fun n -> shift_left n 1) m
    let _LSR m = gen_SHIFT 0 (fun n -> shift_right_logical n 1) m
    let _ROL m = gen_SHIFT 7 (fun n -> logor (shift_left n 1) (Flag.geti Flag.carry)) m
    let _ROR m = gen_SHIFT 0 (fun n ->
        logor (shift_right_logical n 1) (shift_left (Flag.geti Flag.carry) 7)) m

    (* Jump and calls *)
    let _JMP m =
      PC.set @@ Location.ref m
    let _JSR m =
      Stack.push_addr Uint16.(pred @@ PC.get ());
      _JMP m
    let _RTS _ = PC.set Uint16.(succ @@ Stack.pull_addr ())

    (* Branches *)
    let gen_BRANCH f s m =
      if Flag.get f = s then begin
        (* Interpret as signed *)
        let v = Int16.of_int8 @@ Int8.of_uint8 @@ !!m in
        (* Add as signed operation *)
        let next = Int16.((of_uint16 (PC.get ())) + v) in
        (* Back to unsigned *)
        let unext = Uint16.of_int16 next in
        let cp = if (get_hi unext) <> (get_hi (PC.get ())) then 1 else 0 in
        Stdlib.(cycle_count := !cycle_count + 1 + cp) ;
        PC.set unext
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
      Stack.push_addr (Uint16.succ @@ PC.get ()) ;
      Flag.set Flag.break true;
      Stack.push (R.get `P) ;
      Flag.set Flag.interrupt true;
      PC.set @@ mk_addr ~lo:(M.read (u16 0xFFFE)) ~hi:(M.read (u16 0xFFFF))

    let _RTI _ =
      R.set `P @@ Stack.pull ();
      Flag.set Flag.break false;
      Flag.set Flag.reserved true;
      PC.set @@ Stack.pull_addr ()

    let _NOP _ = ()
    let _UNO = _NOP

    module Decoding = struct
      let triple (opcode : uint8) =
        let open Uint8 in
        let ib = shift_right_logical opcode 2 in
        let ia = shift_right_logical opcode 5 in
        (logand ia (u8 0x7),
         logand ib (u8 0x7),
         logand opcode (u8 0x3))

      let invalid_instruction a b c =
        Format.printf "Invalid instruction %a %d %d %d\n"
          pp_u8 (M.read @@ PC.get ()) a b c ;
        assert false

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

      let decode (opcode : uint8) =
        let (a, b, c) = triple opcode in
        let (a, b, c) = Uint8.(to_int a, to_int b, to_int c) in
        let f, cfi = get_fun a b c in
        let cf = if cfi = -1 then unofCycles a b c else cycFuns.(cfi) in
        let am = get_am a b c in
        (f, cf, am)
    end
  end

  let print_state () =
    let pc = PC.get () in
    let opcode = M.read pc in
    let (_, _, am) = Instruction.Decoding.decode opcode in
    let size = addressing_mode_size am in
    Format.printf "%a  " pp_u16 pc ;
    for i = 0 to size - 1 do
      Format.printf "%a " pp_u8 (M.read Uint16.(pc + (u16 i)))
    done ;
    Format.printf "\t\t A:%a X:%a Y:%a P:%a SP:%a CYC:%3d\n%!"
     pp_u8 (R.get `A) pp_u8 (R.get `X) pp_u8 (R.get `Y) pp_u8 (R.get `P)
     pp_u8 (R.get `S) Stdlib.(!cycle_count * 3 mod 341)

  (* Returns packaged location and if page was crossed *)
  let package_arg am =
    let get_page = get_hi in
    let pc = PC.get () in
    let b1 = Uint16.(u16 1 + pc) in
    let b2 = Uint16.(u16 2 + pc) in
    let v1 = M.read b1 in
    let v2 = M.read b2 in
    let v12 = mk_addr ~hi:v2 ~lo:v1 in
    let absolute r =
      let v = R.get r in
      let total = Uint16.((of_uint8 v) + v12) in
      Location.Address total, get_page v12 <> get_page total
    in match am with
    | Implicit -> Location.None, false
    | Accumulator -> Location.Register `A, false
    | Immediate -> Location.Immediate v1, false
    | Zero_Page -> Location.Address (Uint16.of_uint8 v1), false
    | Zero_Page_X ->
      Location.Address (Uint16.of_uint8 (Uint8.(v1 + R.get `X))), false
    | Zero_Page_Y ->
      Location.Address (Uint16.of_uint8 (Uint8.(v1 + R.get `Y))), false
    | Relative -> Location.Immediate v1, false
    | Absolute -> Location.Address v12, false
    | Absolute_X -> absolute `X
    | Absolute_Y -> absolute `Y
    | Indirect ->
      (* Second byte of target wrap around in page *)
      let sto_addr_hi = mk_addr ~hi:(get_hi v12) ~lo:Uint8.(succ @@ get_lo v12) in
      Location.Address (mk_addr ~hi:(M.read sto_addr_hi) ~lo:(M.read v12)), false
    | Indexed_Indirect (*X*) -> 
      let sto_addr = Uint8.(v1 + R.get `X) in
      (* Second byte of target wrap around in zero page *)
      let sto_addr_hi = Uint16.of_uint8 @@ Uint8.(succ sto_addr) in
      Location.Address (mk_addr ~hi:(M.read sto_addr_hi) 
                          ~lo:(M.read @@ Uint16.of_uint8 sto_addr)), false
    | Indirect_Indexed (*Y*) ->
      (* Second byte of target wrap around in zero page *)
      let sto_addr_hi = Uint16.of_uint8 @@ Uint8.(succ v1) in
      let sto = mk_addr ~hi:(M.read sto_addr_hi) ~lo:(M.read @@ Uint16.of_uint8 v1) in
      let addr = Uint16.(sto + of_uint8 (R.get `Y)) in
      Location.Address addr, get_page sto <> get_page addr

  let fetch_instr () =
    let opcode = M.read @@ PC.get () in
    let (f, cf, am) = Instruction.Decoding.decode opcode in
    let (arg, page_crossed) = package_arg am in
    let mode_size = addressing_mode_size am in
    PC.set Uint16.(PC.get () + (u16 mode_size)) ;
    let cycles_elapsed = cf page_crossed am in
    cycle_count := !cycle_count + cycles_elapsed ;
    (* Reserved bit always on *) 
    Flag.set Flag.reserved true;
    f arg

  let interrupt () =
    Stack.push_addr (PC.get ()) ;
    Stack.push (R.get `P) ;
    Flag.set Flag.interrupt true;
    PC.set @@ mk_addr ~hi:(M.read (u16 0xFFFA)) ~lo:(M.read (u16 0xFFFB))

  module M = M
end
