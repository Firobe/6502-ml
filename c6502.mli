open Stdint
exception Invalid_instruction of uint16 * uint8

module Int_utils : sig
  val u8 : int -> uint8
  val u16 : int -> uint16
  val pp_u8 : Format.formatter -> uint8 -> unit
  val pp_u16 : Format.formatter -> uint16 -> unit
  val mk_addr : hi:uint8 -> lo:uint8 -> uint16
  val get_hi : uint16 -> uint8
  val get_lo : uint16 -> uint8
  val get_bit : uint8 -> int -> bool
end

module type MemoryMap = sig
  val read : uint16 -> uint8
  val write : uint16 -> uint8 -> unit
end

module type CPU = sig
  module M : MemoryMap

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

module MakeCPU : functor (M : MemoryMap) -> CPU