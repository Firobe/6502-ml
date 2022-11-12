open Stdint

let u8 = Uint8.of_int
let u16 = Uint16.of_int
let u8of16 = Uint8.of_uint16
let u16of8 = Uint16.of_uint8
let pp_u8 fmt u = Format.fprintf fmt "%.2X" (Uint8.to_int u)
let pp_u16 fmt u = Format.fprintf fmt "%.4X" (Uint16.to_int u)

let mk_addr ~hi ~lo =
  let lo = u16of8 lo in
  let hi = u16of8 hi in
  Uint16.(logor (shift_left hi 8) lo)

let get_hi (addr : uint16) = u8of16 Uint16.(shift_right_logical addr 8)
let get_lo (addr : uint16) = u8of16 addr
let get_bit (x : uint8) n = Uint8.(one = logand (shift_right_logical x n) one)
