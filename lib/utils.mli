(** Some helper functions to make life easier with fixed-size integers.

    Mostly aliases for some {!module:Stdint} functions. *)

open Stdint

val u8 : int -> uint8
(** {!type:uint8} from OCaml {!type:int} *)

val u16 : int -> uint16
(** {!type:uint16} from OCaml {!type:int} *)

val u8of16 : uint16 -> uint8
(** Cast a 16-bit integer to 8-bit one *)

val u16of8 : uint8 -> uint16
(** Cast a 8-bit integer to 16-bit one *)

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
