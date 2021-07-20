(** 6502 chip family simulation

    6502-ml is a library handling the simulation of a 6502 processor with
    arbitrary memory mappings.

    To build a {!module-type:CPU}, one must provide a {!module-type:MemoryMap}
    (stateful module mapping CPU addresses to concrete memory cells and/or
    various peripherals) to the {!module:MakeCPU} functor.

    Afterwards, the obtained {!module-type:CPU} can be run instruction by
    instruction using the {!CPU.fetch_instr} function and inspected with the
    various other functions.

    All of the input and output is done with fixed-size integers from
    {!module:Stdint}. We provide a small helper module {!module:Int_utils} to
    handle these values.

    See the {!example} section for a gentle introduction.
*)

open Stdint

(** {1 Modules} *)

exception Invalid_instruction of uint16 * uint8
(** [Invalid_instruction (address, instruction)] is raised when the program
    counter is pointing to an invalid instruction (which is different from
    unofficial) and {!CPU.fetch_instr} is run. *)

(** Mapping from the CPU memory space to the real world.

    A memory map defines how the CPU interact with the world by translating
    reads and writes within the CPU memory space to concrete read and writes to
    OCaml objects.

    It can be used to map memory arrays to the CPU space or interact with
    various simulated peripherals.

    Addresses can use the full range of an {!type:uint16}: from [0x0000] to
    [0xFFFF]. *)
module type MemoryMap = sig
  val read : uint16 -> uint8
  (** [read a] defines the behavior when trying to read from address [a]. *)

  val write : uint16 -> uint8 -> unit
  (** [write a v] defines the behavior when trying to write value [v] to address
      [a]. *)
end

(** A full CPU as obtained with {!module:MakeCPU}.

    This module contains the mutable state of the CPU. The state can be
    inspected and altered with the various setters and getters of
    {!module:Register} and {!module:PC}.

    The two important functions relevant to simulation are {!fetch_instr} and
    {!interrupt}. *)
module type CPU = sig
  module M : MemoryMap
  (** The memory map of the CPU. You can use {!M.read} and {!M.write} to access
      the CPU memory space. *)

  (** {2 State of the CPU} *)

  val print_state : unit -> unit
    (** Print the content of the registers, PC, the cycle count and the current
        byte pointed by PC. *)

  (** Access and modify the content of the 8-bit registers of the CPU. *)
  module Register : sig
    type t = [`S (** Stack pointer *)
             | `A (** Accumulator *)
             | `X (** X index *)
             | `Y (** Y index *)
             | `P (** Processor status *)
             ]
    (** The different 8-bit registers, represented by polymorphic variants.
        Every register defaults to zero at startup, except the processor status
        which defaults to [0x24]. *)

    val get : t -> uint8
    (** Get the current value of a register. *)

    val set : t -> uint8 -> unit
    (** Change the value of a register. *)
  end

  (** Access and modify the program counter of the CPU.

      The PC defaults to [0x400] at startup and upon reset. *)
  module PC : sig
    val get : unit -> uint16
    (** Get the current address of the PC. *)

    val set : uint16 -> unit
    (** Set the current address of the PC. *)

    val init : unit -> unit
    (** Set the PC by reading a full 16-bit address stored at [0xFFF[C-D]]
        (little-endian). *)
  end

  val enable_decimal : bool ref
  (** Determines if the decimal flag of the processor has any effect in
      instructions [ADC] and [SBC]. Indeed, some machines (such as the NES)
      completely disable decimal mode at the hardware level.

      Default to [true]. *)

  val cycle_count : int ref
  (** How many cycles have elapsed during the last reset. *)

  (** {2 Simulation} *)

  val fetch_instr : unit -> unit
  (** Fetches, decodes and executes the next instruction, modifying the
      current state according to the simulation.

      This is the {e main entry point} of the simulation. *)

  val reset : unit -> unit
  (** Completely restore the default state of the CPU, wiping the CPU memory
      space with zeroes. *)

  val interrupt : unit -> unit
  (** Simulate an harware interrupt of the processor, effectively suspending the
      current context to call the interrupt handler (whose address is stored at
      [0xFFF[A-B]]). *)
end

(** Make a {!module-type:CPU} with a {!module-type:MemoryMap}. *)
module MakeCPU : functor (M : MemoryMap) -> CPU

(** Some helper functions to make life easier with fixed-size integers.

    Mostly aliases for some {!module:Stdint} functions. *)
module Int_utils : sig
  val u8 : int -> uint8
  (** {!type:uint8} from OCaml {!type:int} *)

  val u16 : int -> uint16
  (** {!type:uint16} from OCaml {!type:int} *)

  val pp_u8 : Format.formatter -> uint8 -> unit
  (** Print an {!type:uint8} as 0xYY *)

  val pp_u16 : Format.formatter -> uint16 -> unit
  (** Print an {!type:uint16} as 0xYYYY *)

  val mk_addr : hi:uint8 -> lo:uint8 -> uint16
  (** Make an {!type:uint16} from a [lo] and [hi] byte. *)

  val get_hi : uint16 -> uint8
  (** Get the high byte of an {!type:uint16}. *)

  val get_lo : uint16 -> uint8
  (** Get the low byte of an {!type:uint16}. *)

  val get_bit : uint8 -> int -> bool
  (** [get_bit v n] returns if the [n]th bit of [v] is [1] *)
end

(** {1:example Basic example} *)

(** The code below builds a simple CPU from a {!module-type:MemoryMap} simply
    mapping all the memory space to a big array.

{[
module SimpleCPU = C6502.MakeCPU (struct
    (* Big mutable array, internal to the CPU *)
    let mem = Array.make 0x10000 Uint8.zero

    (* Map addresses to cells of that array *)
    let read a = mem.(Uint16.to_int a)
    let write a v = mem.(Uint16.to_int a) <- v
  end)
   ]}

We can then for example load a ROM into the memory, using {!CPU.M.write}:

{[
  let open Int_utils in (* for u8 and u16 *)
  let rom : Bytes.t = (* read ROM from file... *) in
  (** Load each byte to the CPU, converting to fixed-size integers *)
  Bytes.iteri (fun addr v ->
      SimpleCPU.M.write (u16 addr) (u8 (int_of_char v))
    ) rom
]}

And run the CPU until it encounters a trap (jump to the current address):

{[
  let rec run_until_trap () =
    let old_PC = SimpleCPU.PC.get () in
    SCpu.fetch_instr () ;
    if old_PC <> get_pc () then run_until_trap ()
]}

And then for example inspect the status of the processor:

{[ Format.printf "Processor status: %a\n" pp_u8 (SimpleCPU.Register.get `P) ]}
*)
