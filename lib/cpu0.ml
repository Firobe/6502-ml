open Stdint

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

module Make (M : MemoryMap) = struct
  module PC = struct
    type t = { mutable value : uint16 }

    let create () = { value = 0x0400U }
    let get t = t.value
    let set t v = t.value <- v

    let init t mem =
      t.value <- Utils.mk_addr ~hi:(M.read mem 0xFFFDU) ~lo:(M.read mem 0xFFFCU)

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

  module Stack = struct
    open Utils

    let total_addr t = mk_addr ~hi:0x01u ~lo:(Register.get t.reg `S)

    let push t v =
      (* Addr = 0x01XX *)
      M.write t.mem (total_addr t) v;
      Register.decr t.reg `S

    let push_addr t v =
      push t (get_hi v);
      push t (get_lo v)

    let pull t =
      Register.incr t.reg `S;
      M.read t.mem (total_addr t)

    let pull_addr t =
      let lo = pull t in
      let hi = pull t in
      mk_addr ~lo ~hi
  end
end
