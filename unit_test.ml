let dump_memory ?path:(path="memdump") () =
    let file = open_out_bin (path ^ ".bin") in
    let store = Bytes.create 0x10000 in
    for i = 0 to (Array.length Cpu.memory) - 1 do
        Bytes.set store i @@ char_of_int Cpu.memory.(i)
    done ;
    output file store 0 (Bytes.length store) ;
    close_out file

let load_rom path =
    let file = open_in_bin path in
    let store = Bytes.create 0x10000 in
    let read = input file store 0 0x10000 in
    let store = Bytes.sub store 0 read in
    Bytes.iteri (fun i el -> Array.set Cpu.memory (i) (int_of_char el)) store

let test1 () =
    Cpu.reset () ;
    load_rom "test_roms/klaus.bin" ;
    let continue = ref true in
    Cpu.program_counter := 0x400 ;
    Cpu.processor_status := 0x00 ;
    while !continue do
        let back = !Cpu.program_counter in
        Cpu.fetch_instr () ;
        if back = !Cpu.program_counter then
            continue := false
    done ;
    if !Cpu.program_counter = 0x3469 then
        Printf.printf "Functional tests ..... OK\n%!"
    else Printf.printf "Functional tests ..... KO (trap at %.4X)%!\n" !Cpu.program_counter

let test2 () =
    let file = open_in_bin "test_roms/nestest.log" in
    Cpu.reset () ;
    load_rom "test_roms/nestest.nes.bin" ;
    Printf.printf "Nestest .............. " ;
    let regexp = Str.regexp "^\\([0-9A-F]+\\).* P:\\([0-9A-F]+\\).*CYC: *\\([0-9]+\\)" in
    let continue = ref true in
    let was_error = ref false in
    Cpu.stack_pointer := 0xFD ;
    Cpu.enable_decimal := false ;
    Cpu.processor_status := 0x24 ;
    Cpu.program_counter := 0xC000 ;
    while !continue do
        if !Cpu.program_counter = 0xC66E then
            continue := false ;
        let toParse = input_line file in
        assert (Str.string_match regexp toParse 0) ;
        let correctPC = int_of_string @@ ("0x"^Str.matched_group 1 toParse) in
        let correctP = int_of_string @@ ("0x"^Str.matched_group 2 toParse) in
        let cycleNb = int_of_string @@ Str.matched_group 3 toParse in
        if cycleNb != (!Cpu.cycle_count * 3) mod 341 then (
            Printf.printf "KO (cycle difference)\n%!";
            was_error := true; continue := false
        ) ;
        if correctPC != !Cpu.program_counter then (
            Printf.printf "KO (PC difference)\n%!";
            was_error := true; continue := false
        ) ;
        if correctP != !Cpu.processor_status then (
            Printf.printf "KO (P status difference)\n%!";
            was_error := true; continue := false
        ) ;
        Cpu.fetch_instr ()
    done ;
    if not !was_error && Cpu.memory.(2) = 0 && Cpu.memory.(3) = 0 then
        Printf.printf "OK\n%!"
    else if not !was_error then
        Printf.printf "KO (errors %.2X %.2X)\n%!" Cpu.memory.(2) Cpu.memory.(3)

let test_rom name path =
    Cpu.reset ();
    Printf.printf "%s " name ;
    load_rom path ;
    let continue = ref true in
    Cpu.stack_pointer := 0xFD ;
    Cpu.enable_decimal := false ;
    Cpu.processor_status := 0x24 ;
    Cpu.init_pc () ;
    while !continue do
        let back = !Cpu.program_counter in
        Cpu.fetch_instr () ;
        if back = !Cpu.program_counter then
            continue := false
    done ;
    let cur_pos = ref 0x6004 in
    while Cpu.memory.(!cur_pos) != 0 do
        incr cur_pos
    done ;
    let end_pos = !cur_pos - 2 in
    cur_pos := end_pos ;
    while Cpu.memory.(!cur_pos) != 0x0A do
        decr cur_pos
    done ;
    let begin_pos = !cur_pos + 1 in
    let outStr = String.init (end_pos - begin_pos + 1)
        (fun i -> char_of_int @@ Cpu.memory.(begin_pos + i)) in
    if outStr = "Failed" then
        let errStr = String.init (end_pos - 0x6004 + 1)
            (fun i -> 
                let m = Cpu.memory.(0x6004 + i) in
                char_of_int @@ (if m = 0x0A then 0x3B else m)
            ) in
        Printf.printf "KO (%s)\n%!" errStr
    else Printf.printf "OK\n%!"

let test3 () =
    test_rom "Unit basics .........." "test_roms/instr_test/01-basics.nes.bin" ;
    test_rom "Unit implied ........." "test_roms/instr_test/02-implied.nes.bin" ;
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

let tests =
  test3 () ;
  test4 () ;
  test2 () ;
  test1 () ;
(*   test5 () *)
