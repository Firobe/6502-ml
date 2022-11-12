open Stdint

exception Invalid_instruction of uint16 * uint8

include Cpu0

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

module Utils = Utils

module Make (M : MemoryMap) = struct
  type input = M.input
  type mem = M.t

  module Register = Register
  module Flag = Register.Flag
  module R = Register
  open Utils
  include Make (M)

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

  module Decoding = struct
    module Instruction = Instruction.Make (M)

    (* Return (a, b, c) from the opcode aaabbbcc *)
    let triple (opcode : uint8) =
      let open Uint8 in
      let ib = shift_right_logical opcode 2 in
      let ia = shift_right_logical opcode 5 in
      ( to_int @@ logand ia 7u,
        to_int @@ logand ib 7u,
        to_int @@ logand opcode 3u )

    open Stdlib

    (* Simulate cycles from unofficial opcodes *)
    let unofCycles (a, b, c) pc am =
      if (c = 3 && a >= 6) || a <= 3 then
        match am with
        | Addressing.Immediate -> 2
        | Zero_Page -> 5
        | Zero_Page_X | Absolute -> 6
        | Absolute_X | Absolute_Y -> 7
        | _ -> 8
      else
        let _, cfi =
          Instruction.of_opcode_triple (5, b, if c >= 2 then c - 2 else 0)
        in
        Addressing.cycle_functions.(cfi) pc am

    (* Return instruction operation, cycle function and addressing mode of
     * opcode *)
    let decode (opcode : uint8) =
      let triple = triple opcode in
      let f, cfi = Instruction.of_opcode_triple triple in
      let cf =
        if cfi = -1 then unofCycles triple else Addressing.cycle_functions.(cfi)
      in
      let am = Addressing.of_opcode_triple triple in
      (f, cf, am)

    module Location = Instruction.Location

    (* List of instructions *)
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
      | Addressing.Implicit -> (Location.None, false)
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
  end

  let nmi st =
    Stack.push_addr st (PC.get st.pc);
    Stack.push st (R.get st.reg `P);
    Flag.set st.reg Flag.interrupt true;
    PC.set st.pc
    @@ mk_addr ~lo:(M.read st.mem 0xFFFAU) ~hi:(M.read st.mem 0xFFFBU)

  let next_cycle st =
    (* Check for interrupts *)
    (* NMI *)
    if NMI.check st.nmi then (
      nmi st;
      NMI.clear st.nmi (* IRQ *))
    else if
      (not @@ Flag.get st.reg Flag.interrupt)
      && IRQ_collector.is_pulled st.irq_collector
    then (
      Stack.push_addr st (PC.get st.pc);
      Stack.push st (R.get st.reg `P);
      Flag.set st.reg Flag.interrupt true;
      PC.set st.pc
      @@ mk_addr
           ~lo:(M.read st.mem (u16 0xFFFE))
           ~hi:(M.read st.mem (u16 0xFFFF)));
    (* Continue as normal *)
    let opcode = M.read st.mem @@ PC.get st.pc in
    let f, cf, am = Decoding.decode opcode in
    let arg, page_crossed = Decoding.package_arg st am in
    let mode_size = Addressing.size am in
    PC.set st.pc Uint16.(PC.get st.pc + u16 mode_size);
    let cycles_elapsed = cf page_crossed am in
    st.cycle_count <- st.cycle_count + cycles_elapsed;
    (* Reserved bit always on *)
    Flag.set st.reg Flag.reserved true;
    f st arg

  let nmi st = NMI.pull st.nmi

  let print_state st =
    let pc = PC.get st.pc in
    let opcode = M.read st.mem pc in
    let _, _, am = Decoding.decode opcode in
    let size = Addressing.size am in
    Format.printf "%a  " pp_u16 pc;
    for i = 0 to size - 1 do
      Format.printf "%a " pp_u8 (M.read st.mem Uint16.(pc + u16 i))
    done;
    Format.printf "\t\t A:%a X:%a Y:%a P:%a SP:%a CYC:%3d\n%!" pp_u8
      (R.get st.reg `A) pp_u8 (R.get st.reg `X) pp_u8 (R.get st.reg `Y) pp_u8
      (R.get st.reg `P) pp_u8 (R.get st.reg `S)
      Stdlib.(st.cycle_count * 3 mod 341)
end
