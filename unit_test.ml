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
        Printf.printf "Self-test ............ OK (%d cycles)\n" !Cpu.cycle_count
    else Printf.printf "Self-test ............ KO (trap at %.4X)\n" !Cpu.program_counter

let test2 () =
    let file = open_in_bin "test_roms/nestest.log" in
    Cpu.reset () ;
    load_rom "test_roms/nestest.bin" ;
    let regexp = Str.regexp "^\\([0-9A-F]+\\).*CYC: *\\([0-9]+\\)" in
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
        let cycleNb = int_of_string @@ Str.matched_group 2 toParse in
        if cycleNb != (!Cpu.cycle_count * 3) mod 341 then (
            Printf.printf "Advanced self-test ... KO (cycle difference)\n";
            was_error := true; continue := false
        ) ;
        if correctPC != !Cpu.program_counter then (
            Printf.printf "Advanced self-test ... KO (PC difference)\n";
            was_error := true; continue := false
        ) ;
        Cpu.fetch_instr ()
    done ;
    if not !was_error && Cpu.memory.(2) = 0 && Cpu.memory.(3) = 0 then
        Printf.printf "Advanced self-test ... OK (%d cycles)\n" !Cpu.cycle_count
    else if not !was_error then
        Printf.printf "Advanced self-test ... KO (errors %.2X %.2X)\n" Cpu.memory.(2) Cpu.memory.(3)

let tests =
  test1 () ;
  test2 ()
