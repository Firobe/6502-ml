open Stdint

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

module Make : functor (M : Mmap) -> Full
