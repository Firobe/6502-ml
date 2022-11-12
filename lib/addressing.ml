type t =
  | Implicit
  | Accumulator
  | Immediate
  | Zero_Page
  | Zero_Page_X
  | Zero_Page_Y
  | Relative
  | Absolute
  | Absolute_X
  | Absolute_Y
  | Indirect
  | Indexed_Indirect
  | Indirect_Indexed

let size = function
  | Implicit | Accumulator -> 1
  | Immediate | Zero_Page | Zero_Page_X | Zero_Page_Y | Indexed_Indirect
  | Indirect_Indexed | Relative ->
      2
  | Absolute | Absolute_X | Absolute_Y | Indirect -> 3

(* Addressed by b, c, a *)
let layout =
  let impl = Implicit in
  let zpg_ = Zero_Page in
  let abs_ = Absolute in
  let rel_ = Relative in
  let zpgx = Zero_Page_X in
  let zpgy = Zero_Page_Y in
  let absx = Absolute_X in
  let absy = Absolute_Y in
  let imm_ = Immediate in
  let xind = Indexed_Indirect in
  let ind_ = Indirect in
  let indy = Indirect_Indexed in
  let jam_ = Implicit in
  let acc_ = Accumulator in
  [|
    [|
      [|impl; abs_; impl; impl; imm_; imm_; imm_; imm_|];
      [|xind; xind; xind; xind; xind; xind; xind; xind|];
      [|jam_; jam_; jam_; jam_; imm_; imm_; imm_; imm_|];
      [|xind; xind; xind; xind; xind; xind; xind; xind|]
    |];
    [|
      [|zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_|];
      [|zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_|];
      [|zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_|];
      [|zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_; zpg_|]
    |];
    [|
      [|impl; impl; impl; impl; impl; impl; impl; impl|];
      [|imm_; imm_; imm_; imm_; imm_; imm_; imm_; imm_|];
      [|acc_; acc_; acc_; acc_; impl; impl; impl; impl|];
      [|imm_; imm_; imm_; imm_; imm_; imm_; imm_; imm_|]
    |];
    [|
      [|abs_; abs_; abs_; ind_; abs_; abs_; abs_; abs_|];
      [|abs_; abs_; abs_; abs_; abs_; abs_; abs_; abs_|];
      [|abs_; abs_; abs_; abs_; abs_; abs_; abs_; abs_|];
      [|abs_; abs_; abs_; abs_; abs_; abs_; abs_; abs_|]
    |];
    [|
      [|rel_; rel_; rel_; rel_; rel_; rel_; rel_; rel_|];
      [|indy; indy; indy; indy; indy; indy; indy; indy|];
      [|jam_; jam_; jam_; jam_; jam_; jam_; jam_; jam_|];
      [|indy; indy; indy; indy; indy; indy; indy; indy|]
    |];
    [|
      [|zpgx; zpgx; zpgx; zpgx; zpgx; zpgx; zpgx; zpgx|];
      [|zpgx; zpgx; zpgx; zpgx; zpgx; zpgx; zpgx; zpgx|];
      [|zpgx; zpgx; zpgx; zpgx; zpgy; zpgy; zpgx; zpgx|];
      [|zpgx; zpgx; zpgx; zpgx; zpgy; zpgy; zpgx; zpgx|]
    |];
    [|
      [|impl; impl; impl; impl; impl; impl; impl; impl|];
      [|absy; absy; absy; absy; absy; absy; absy; absy|];
      [|impl; impl; impl; impl; impl; impl; impl; impl|];
      [|absy; absy; absy; absy; absy; absy; absy; absy|]
    |];
    [|
      [|absx; absx; absx; absx; absx; absx; absx; absx|];
      [|absx; absx; absx; absx; absx; absx; absx; absx|];
      [|absx; absx; absx; absx; absy; absy; absx; absx|];
      [|absx; absx; absx; absx; absy; absy; absx; absx|]
    |]
  |][@ocamlformat "disable"]

(* Addressing and instruction dispatch *)
let of_opcode_triple (a, b, c) = layout.(b).(c).(a)

(* Precompute number of cycles taken, official *)
let cycle_functions =
  let c0 pc = function
    | Immediate -> 2
    | Zero_Page -> 3
    | Zero_Page_X | Zero_Page_Y | Absolute -> 4
    | Absolute_X | Absolute_Y -> if pc then 5 else 4
    | Indirect_Indexed -> if pc then 6 else 5
    | _ -> 6
  in
  let c1 _ = function
    | Absolute_X | Absolute_Y -> 5
    | Indirect_Indexed -> 6
    | o -> c0 false o
  in
  let c2 _ _ = 2 in
  let c3 _ _ = 3 in
  let c4 _ _ = 4 in
  let c5 _ _ = 6 in
  let c6 _ _ = 7 in
  let c7 _ = function
    | Accumulator -> 2
    | Zero_Page -> 5
    | Zero_Page_X | Absolute -> 6
    | _ -> 7
  in
  let c8 _ = function Absolute -> 3 | _ -> 5 in
  let c9 _ _ = 2 in
  [| c0; c1; c2; c3; c4; c5; c6; c7; c8; c9 |]
