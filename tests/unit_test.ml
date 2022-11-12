open Stdint
open C6502.Utils

module SCpu = C6502.Make (struct
  type t = uint8 array
  type input = string (* rom path *)

  let create path =
    let mem = Array.make 0x10000 Uint8.zero in
    let file = open_in_bin path in
    let store = Bytes.create 0x10000 in
    let read = input file store 0 0x10000 in
    let store = Bytes.sub store 0 read in
    Bytes.iteri (fun i el -> mem.(i) <- u8 (int_of_char el)) store;
    mem

  (* 0x000 to 0xFFFF main memory *)
  let read m a = m.(Uint16.to_int a)
  let write m a v = m.(Uint16.to_int a) <- v
end)

let set_pc cpu = SCpu.PC.set (SCpu.pc cpu)
let get_pc cpu = SCpu.PC.get (SCpu.pc cpu)
let set_reg cpu = SCpu.Register.set (SCpu.registers cpu)
let get_reg cpu = SCpu.Register.get (SCpu.registers cpu)
let read_mem cpu a = (SCpu.memory cpu).(a)

let klaus () =
  let cpu = SCpu.create "test_roms/klaus.bin" in
  let continue = ref true in
  set_pc cpu (u16 0x400);
  set_reg cpu `P (u8 0);
  let start = Sys.time () in
  while !continue do
    let back = get_pc cpu in
    SCpu.next_cycle cpu;
    if back = get_pc cpu then continue := false
  done;
  let time_taken = Sys.time () -. start in
  let instr_per_s = float_of_int (SCpu.cycle_count cpu) /. time_taken in
  let _to_mhz = instr_per_s /. 1000000. in
  Alcotest.(check int) "Trap address" 0x3469 (get_pc cpu |> Uint16.to_int)

let nestest () =
  let file = open_in_bin "test_roms/nestest.log" in
  let cpu = SCpu.create "test_roms/nestest.nes.bin" in
  Printf.printf "Nestest .............. %!";
  let regexp =
    Str.regexp
      "^\\([0-9A-F]+\\).* A:\\([0-9A-F]+\\).* P:\\([0-9A-F]+\\).*CYC: \
       *\\([0-9]+\\)"
  in
  let continue = ref true in
  set_reg cpu `S (u8 0xFD);
  SCpu.enable_decimal cpu false;
  set_reg cpu `P (u8 0x24);
  set_pc cpu (u16 0xC000);
  let last_line = ref "" in
  while !continue do
    if get_pc cpu = u16 0xC66E then continue := false;
    let toParse = input_line file in
    assert (Str.string_match regexp toParse 0);
    let correctPC = int_of_string @@ "0x" ^ Str.matched_group 1 toParse in
    let correctA = int_of_string @@ "0x" ^ Str.matched_group 2 toParse in
    let correctP = int_of_string @@ "0x" ^ Str.matched_group 3 toParse in
    let cycleNb = int_of_string @@ Str.matched_group 4 toParse in
    Alcotest.(check int) "Same cycle" cycleNb (SCpu.cycle_count cpu * 3 mod 341);
    Alcotest.(check int)
      "Same accumulator" correctA
      (Uint8.to_int @@ get_reg cpu `A);
    Alcotest.(check int) "Same PC" correctPC (Uint16.to_int @@ get_pc cpu);
    Alcotest.(check int)
      "Same P register" correctP
      (Uint8.to_int @@ get_reg cpu `P);
    last_line := toParse;
    SCpu.next_cycle cpu
  done;
  Alcotest.(check int) "Final status 1" 0 (Uint8.to_int @@ read_mem cpu 2);
  Alcotest.(check int) "Final status 2" 0 (Uint8.to_int @@ read_mem cpu 3)

let test_rom path ?(expected = "Passed") () =
  let cpu = SCpu.create path in
  let continue = ref true in
  set_reg cpu `S (u8 0xFD);
  SCpu.enable_decimal cpu false;
  set_reg cpu `P (u8 0x24);
  SCpu.PC.init (SCpu.pc cpu) (SCpu.memory cpu);
  while !continue do
    let back = get_pc cpu in
    SCpu.next_cycle cpu;
    if back = get_pc cpu then continue := false
  done;
  let cur_pos = ref 0x6004 in
  while read_mem cpu !cur_pos <> u8 0 do
    incr cur_pos
  done;
  let end_pos = !cur_pos - 2 in
  cur_pos := end_pos;
  while read_mem cpu !cur_pos <> u8 0x0A do
    decr cur_pos
  done;
  let begin_pos = !cur_pos + 1 in
  let outStr =
    String.init
      (end_pos - begin_pos + 1)
      (fun i -> char_of_int @@ Uint8.to_int @@ read_mem cpu (begin_pos + i))
  in
  let outStr =
    if outStr = "Passed" then outStr
    else
      String.init
        (end_pos - 0x6004 + 1)
        (fun i ->
          let m = read_mem cpu (0x6004 + i) in
          char_of_int @@ Uint8.to_int @@ if m = u8 0x0A then u8 0x3B else m)
  in
  Alcotest.(check string) "Status check" expected outStr

let extended_opcodes ?(expected = 0x3469) () =
  let cpu = SCpu.create "test_roms/65C02_extended_opcodes_test.bin" in
  let continue = ref true in
  set_pc cpu (u16 0x400);
  set_reg cpu `P (u8 0x00);
  while !continue do
    let back = get_pc cpu in
    SCpu.next_cycle cpu;
    if back = get_pc cpu then continue := false
  done;
  Alcotest.(check int) "Trap address" expected (get_pc cpu |> Uint16.to_int)

let () =
  let open Alcotest in
  let test_rom ?expected name path =
    test_case name `Quick (test_rom ?expected path)
  in
  run "6502-ml"
    [
      ("basic", [ test_rom "Basics" "test_roms/instr_test/01-basics.nes.bin" ]);
      ( "addressing",
        [
          test_rom "Implied" "test_roms/instr_test/02-implied.nes.bin";
          test_rom "Immediate"
            ~expected:"6B ARR #n;AB ATX #n;;03-immediate;;Failed"
            "test_roms/instr_test/03-immediate.nes.bin";
          test_rom "Zero page" "test_roms/instr_test/04-zero_page.nes.bin";
          test_rom "Zero page XY" "test_roms/instr_test/05-zp_xy.nes.bin";
          test_rom "Absolute" "test_roms/instr_test/06-absolute.nes.bin";
          test_rom ~expected:"9C SYA abs,X;9E SXA abs,Y;;07-abs_xy;;Failed"
            "Absolute XY" "test_roms/instr_test/07-abs_xy.nes.bin";
          test_rom "Indirect X" "test_roms/instr_test/08-ind_x.nes.bin";
          test_rom "Indirect Y" "test_roms/instr_test/09-ind_y.nes.bin";
        ] );
      ( "control flow",
        [
          test_rom "Branches" "test_roms/instr_test/10-branches.nes.bin";
          test_rom "Stack" "test_roms/instr_test/11-stack.nes.bin";
          test_rom "JMP JSR" "test_roms/instr_test/12-jmp_jsr.nes.bin";
          test_rom "RTS" "test_roms/instr_test/13-rts.nes.bin";
          test_rom "RTI" "test_roms/instr_test/14-rti.nes.bin";
          test_rom "BRK" "test_roms/instr_test/15-brk.nes.bin";
        ] );
      (*
    "timing", [
      test_rom "Instructions" "test_roms/instr_timing/1-instr_timing.nes.bin" ;
      test_rom "Branches" "test_roms/instr_timing/2-branch-timing.nes.bin"
    ];
     *)
      ( "end-to-end",
        [
          test_case "Klaus functional tests" `Slow klaus;
          test_case "Nestest comparison" `Slow nestest;
        ] );
      ( "misc",
        [
          test_rom "Special instructions"
            "test_roms/instr_test/16-special.nes.bin";
          test_rom "Absolute X wrap"
            "test_roms/instr_misc/01-abs_x_wrap.nes.bin";
          test_rom "Branch wrap" "test_roms/instr_misc/02-branch_wrap.nes.bin";
        ] );
      ( "unofficial",
        [
          test_case "Extended opcodes" `Quick (extended_opcodes ~expected:0x423);
        ] );
    ]
