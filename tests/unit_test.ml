open Stdint
open C6502.Int_utils

module SCpu = C6502.Make (struct
    type t = uint8 array
    type input = string (* rom path *)

    let create path =
      let mem = Array.make 0x10000 Uint8.zero in
      let file = open_in_bin path in
      let store = Bytes.create 0x10000 in
      let read = input file store 0 0x10000 in
      let store = Bytes.sub store 0 read in
      Bytes.iteri (fun i el -> mem.(i) <- (u8 (int_of_char el))) store;
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

let test1 () =
  let cpu = SCpu.create "test_roms/klaus.bin" in
  let continue = ref true in
  set_pc cpu (u16 0x400) ;
  set_reg cpu `P (u8 0) ;
  let start = Sys.time () in
  while !continue do
    let back = get_pc cpu in
    SCpu.fetch_instr cpu ;
    if back = get_pc cpu then
      continue := false
  done ;
  let time_taken = Sys.time () -. start in
  let instr_per_s = (float_of_int (SCpu.cycle_count cpu)) /. time_taken in
  let to_mhz = instr_per_s /. 1000000. in
  if get_pc cpu = (u16 0x3469) then
    Format.printf "Functional tests ..... OK (freq: %.2f MHz)\n%!" to_mhz
  else Format.printf "Functional tests ..... KO (trap at %a)%!\n"
      Uint16.printer (get_pc cpu)

let test2 () =
  let file = open_in_bin "test_roms/nestest.log" in
  let cpu = SCpu.create "test_roms/nestest.nes.bin" in
  Printf.printf "Nestest .............. %!" ;
  let regexp = Str.regexp "^\\([0-9A-F]+\\).* A:\\([0-9A-F]+\\).* P:\\([0-9A-F]+\\).*CYC: *\\([0-9]+\\)" in
  let continue = ref true in
  let was_error = ref false in
  set_reg cpu `S (u8 0xFD) ;
  SCpu.enable_decimal cpu false;
  set_reg cpu `P (u8 0x24) ;
  set_pc cpu (u16 0xC000) ;
  let last_line = ref "" in
  while !continue do
    if get_pc cpu = (u16 0xC66E) then
      continue := false ;
    let toParse = input_line file in
    assert (Str.string_match regexp toParse 0) ;
    let correctPC = int_of_string @@ ("0x"^Str.matched_group 1 toParse) in
    let correctA = int_of_string @@ ("0x"^Str.matched_group 2 toParse) in
    let correctP = int_of_string @@ ("0x"^Str.matched_group 3 toParse) in
    let cycleNb = int_of_string @@ Str.matched_group 4 toParse in
    if cycleNb <> (SCpu.cycle_count cpu * 3) mod 341 then (
      Printf.printf "KO (cycle difference %d)\n%!"
        ((SCpu.cycle_count cpu * 3) mod 341);
      was_error := true; continue := false
    ) ;
    if (u8 correctA) <> get_reg cpu `A then (
      Printf.printf "KO (ACC difference)\n%!";
      was_error := true; continue := false
    ) ;
    if (u16 correctPC) <> get_pc cpu then (
      Printf.printf "KO (PC difference)\n%!";
      was_error := true; continue := false
    ) ;
    if (u8 correctP) <> get_reg cpu `P then (
      Printf.printf "KO (P status difference)\n%!";
      was_error := true; continue := false
    ) ;
    if not !continue && !was_error then begin
      Printf.printf "Last state:\n%s\nExpected:\n%s\nGot:\n" !last_line toParse;
      SCpu.print_state cpu
    end ;
    last_line := toParse ;
    SCpu.fetch_instr cpu
  done ;
  if not !was_error && read_mem cpu 2 = (u8 0)
     && read_mem cpu 3 = (u8 0) then
    Format.printf "OK\n%!"
  else if not !was_error then
    Format.printf "KO (errors %a %a)\n%!" pp_u8 (read_mem cpu 2)
      pp_u8 (read_mem cpu 3)

let test_rom name path =
  Printf.printf "%s " name ;
  let cpu = SCpu.create path in
  let continue = ref true in
  set_reg cpu `S (u8 0xFD) ;
  SCpu.enable_decimal cpu false ;
  set_reg cpu `P (u8 0x24) ;
  SCpu.PC.init (SCpu.pc cpu) (SCpu.memory cpu);
  while !continue do
    let back = get_pc cpu in
    SCpu.fetch_instr cpu ;
    if back = get_pc cpu then
      continue := false
  done ;
  let cur_pos = ref 0x6004 in
  while read_mem cpu !cur_pos <> (u8 0) do
    incr cur_pos
  done ;
  let end_pos = !cur_pos - 2 in
  cur_pos := end_pos ;
  while read_mem cpu !cur_pos <> (u8 0x0A) do
    decr cur_pos
  done ;
  let begin_pos = !cur_pos + 1 in
  let outStr = String.init (end_pos - begin_pos + 1)
      (fun i -> char_of_int @@ Uint8.to_int
        @@ read_mem cpu (begin_pos + i)) in
  if outStr = "Failed" then
    let errStr = String.init (end_pos - 0x6004 + 1)
        (fun i -> 
           let m = read_mem cpu (0x6004 + i) in
           char_of_int @@ Uint8.to_int @@ (if m = (u8 0x0A) then (u8 0x3B) else m)
        ) in
    Printf.printf "KO (%s)\n%!" errStr
  else Printf.printf "OK\n%!"

let test3 () =
(*
    test_rom "Unit basics .........." "test_roms/instr_test/01-basics.nes.bin" ;
    test_rom "Unit implied ........." "test_roms/instr_test/02-implied.nes.bin" ;
*)
  test_rom "Unit immediate ......." "test_roms/instr_test/03-immediate.nes.bin" ;
  test_rom "Unit zero page ......." "test_roms/instr_test/04-zero_page.nes.bin" ;
  test_rom "Unit zp xy ..........." "test_roms/instr_test/05-zp_xy.nes.bin" ;
  test_rom "Unit absolute ........" "test_roms/instr_test/06-absolute.nes.bin" ;
  test_rom "Unit abs xy .........." "test_roms/instr_test/07-abs_xy.nes.bin" ;
  test_rom "Unit ind x ..........." "test_roms/instr_test/08-ind_x.nes.bin" ;
  test_rom "Unit ind y ..........." "test_roms/instr_test/09-ind_y.nes.bin" ;
  test_rom "Unit branches ........" "test_roms/instr_test/10-branches.nes.bin" ;
  test_rom "Unit stack ..........." "test_roms/instr_test/11-stack.nes.bin" ;
  test_rom "Unit JMP JSR ........." "test_roms/instr_test/12-jmp_jsr.nes.bin" ;
  test_rom "Unit RTS ............." "test_roms/instr_test/13-rts.nes.bin" ;
  test_rom "Unit RTI ............." "test_roms/instr_test/14-rti.nes.bin" ;
  test_rom "Unit BRK ............." "test_roms/instr_test/15-brk.nes.bin" ;
  test_rom "Unit special ........." "test_roms/instr_test/16-special.nes.bin"

let test4 () =
  test_rom "Misc abs x wrap ......" "test_roms/instr_misc/01-abs_x_wrap.nes.bin" ;
  test_rom "Misc branch wrap ....." "test_roms/instr_misc/02-branch_wrap.nes.bin"

let test5 () =
  test_rom "Timing instr ........." "test_roms/instr_timing/1-instr_timing.nes.bin" ;
  test_rom "Timing branches ......" "test_roms/instr_timing/2-branch-timing.nes.bin"

let test6 () =
  let cpu = SCpu.create "test_roms/65C02_extended_opcodes_test.bin" in
  let continue = ref true in
  set_pc cpu (u16 0x400) ;
  set_reg cpu `P (u8 0x00) ;
  while !continue do
    let back = get_pc cpu in
    SCpu.fetch_instr cpu ;
    if back = get_pc cpu then
      continue := false
  done ;
  if get_pc cpu = (u16 0x3469) then
    Printf.printf "Extended ops .......... OK\n%!"
  else Format.printf "Extended ops ......... KO (trap at %a)%!\n"
      pp_u16 (get_pc cpu)

let tests =
  test1 () ;
  test2 () ;
  test3 () ;
  test4 () ;
  test6 () 
(*   test5 () *)
