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

module Make : functor (M : Mmap) -> Full
