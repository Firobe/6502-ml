open Stdint

type t = {
  mutable stack_pointer : uint8;
  mutable acc : uint8;
  mutable irx : uint8;
  mutable iry : uint8;
  mutable processor_status : uint8;
}

type register = [ `S | `A | `X | `Y | `P ]

let create () =
  Uint8.
    {
      stack_pointer = 0xFFu;
      acc = zero;
      irx = zero;
      iry = zero;
      processor_status = 0x24u;
    }

(* Registers *)
open Uint8

let get t = function
  | `S -> t.stack_pointer
  | `A -> t.acc
  | `X -> t.irx
  | `Y -> t.iry
  | `P -> t.processor_status

let set t r v =
  match r with
  | `S -> t.stack_pointer <- v
  | `A -> t.acc <- v
  | `X -> t.irx <- v
  | `Y -> t.iry <- v
  | `P -> t.processor_status <- v

let incr t r = set t r (succ (get t r))
let decr t r = set t r (pred (get t r))

module Flag = struct
  type t = Mask of uint8

  let mkflag n = Mask (shift_left one n)
  let carry = mkflag 0
  let zero = mkflag 1
  let interrupt = mkflag 2 (* DISABLES interrupts when off *)
  let decimal = mkflag 3
  let break = mkflag 4
  let reserved = mkflag 5
  let overflow = mkflag 6
  let negative = mkflag 7
  let mask (Mask m) = m

  let set reg (Mask m) v =
    let p = get reg `P in
    let f = if v then logor p m else logand p (lognot m) in
    set reg `P f

  let get reg (Mask m) =
    if logand m (get reg `P) <> Uint8.zero then true else false

  let geti reg m = if get reg m then one else Uint8.zero
  let update_zero t v = set t zero (v = Uint8.zero)
  let update_neg t v = set t negative (Utils.get_bit v 7)

  let update_nz t v =
    update_zero t v;
    update_neg t v
end
