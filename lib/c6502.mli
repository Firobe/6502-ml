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
  type t
  (** Type representing a mutable address space *)

  type input
  (** Type representing the information needed to initialize memory *)

  val create : input -> t
  (** Create the initial power-up memory *)

  val read : t -> uint16 -> uint8
  (** [read a] defines the behavior when trying to read from address [a]. *)

  val write : t -> uint16 -> uint8 -> unit
  (** [write a v] defines the behavior when trying to write value [v] to address
      [a]. *)
end

(** Mutable data structure, serving as a collector for device IRQs. Devices are
    meant to use this interface to change their IRQ output, and it is read by the
    CPU.
    Different devices must have different identifiers, and their outputs are
    OR'ed.
*)
module IRQ_collector : sig
  type key = string
  type t

  val is_pulled : t -> bool
  val set_pulled : t -> key -> bool -> unit
  val create : unit -> t
end

module NMI : sig
  type t

  val create : unit -> t
  val pull : t -> unit
  val check : t -> bool
  val clear : t -> unit
end

module type CPU = sig
  type mem

  type input
  (** The memory map of the CPU. You can use {!M.read} and {!M.write} to access
      the CPU memory space. *)

  (** {2 State of the CPU} *)

  (** Access and modify the content of the 8-bit registers of the CPU. *)
  module Register : sig
    type register =
      [ `S  (** Stack pointer *)
      | `A  (** Accumulator *)
      | `X  (** X index *)
      | `Y  (** Y index *)
      | `P  (** Processor status *) ]
    (** The different 8-bit registers, represented by polymorphic variants.
        Every register defaults to zero at startup, except the processor status
        which defaults to [0x24]. *)

    type t
    (** Representation of the state of all registers *)

    val get : t -> register -> uint8
    (** Get the current value of a register. *)

    val set : t -> register -> uint8 -> unit
    (** Change the value of a register. *)
  end

  (** Access and modify the program counter of the CPU.

      The PC defaults to [0x400] at startup and upon reset. *)
  module PC : sig
    type t
    (** Representation of the PC *)

    val get : t -> uint16
    (** Get the current address of the PC. *)

    val set : t -> uint16 -> unit
    (** Set the current address of the PC. *)

    val init : t -> mem -> unit
    (** Set the PC by reading a full 16-bit address stored at [0xFFF[C-D]]
        (little-endian). *)
  end

  type t
  (** Representation of the whole CPU state, including its linked devices. Every
      function modifies this representation in place. *)

  val create : ?collector:IRQ_collector.t -> ?nmi:NMI.t -> input -> t
  (** Return the power-up state of the whole system *)

  val pc : t -> PC.t
  val registers : t -> Register.t
  val memory : t -> mem

  val enable_decimal : t -> bool -> unit
  (** Determines if the decimal flag of the processor has any effect in
      instructions [ADC] and [SBC]. Indeed, some machines (such as the NES)
      completely disable decimal mode at the hardware level.

      Default to [true]. *)

  val cycle_count : t -> int
  (** How many cycles have elapsed during the last reset. *)

  (** {2 Simulation} *)

  val next_instruction : t -> int
  (** Fetches, decodes and executes the whole next instruction, modifying the
      current state according to the simulation. Returns the number of cycles
      elapsed.

      This is the {e main entry point} of the simulation. *)

  val next_cycle : t -> unit
  (** Execute a single cycle of the CPU,
      more granular then [next_instruction] *)

  val reset : t -> unit
  (** Completely restore the default state of the CPU, wiping the CPU memory
      space with zeroes. *)

  val nmi : t -> unit
  (** Simulate a non-maskable interrupt of the processor, effectively suspending the
      current context to call the interrupt handler (whose address is stored at
      [0xFFF[A-B]]). *)

  val print_state : t -> unit
  (** Print the content of the registers, PC, the cycle count and the current
        byte pointed by PC. *)

  val pp : Format.formatter -> t -> unit
  (** Fine-grained pretty-printer *)
end

module Utils = Utils
(** Some helper functions to make life easier with fixed-size integers.

    Mostly aliases for some {!module:Stdint} functions. *)

(** A full CPU as obtained with {!module:MakeCPU}.

    This module contains the mutable state of the CPU. The state can be
    inspected and altered with the various setters and getters of
    {!module:Register} and {!module:PC}.

    Make a {!module-type:CPU} with a {!module-type:MemoryMap}.

    The two important functions relevant to simulation are {!fetch_instr} and
    {!interrupt}. *)
module Make : functor (M : MemoryMap) ->
  CPU with type mem = M.t and type input = M.input

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
