let load_rom path =
    let file = open_in_bin path in
    let store = Bytes.create 0x10000 in
    let read = input file store 0 0x10000 in
    let store = Bytes.sub store 0 read in
    Bytes.iteri (fun i el -> Array.set Cpu.memory (i) (int_of_char el)) store

let test1 () =
    Cpu.reset () ;
    let count = ref 0 in
    load_rom "test_roms/klaus.bin" ;
    let continue = ref true in
    Cpu.program_counter := 0x400 ;
    Cpu.processor_status := 0x00 ;
    while !continue do
        incr count ;
        let back = !Cpu.program_counter in
        Cpu.fetch_instr () ;
        if back = !Cpu.program_counter then
            continue := false
    done ;
    if !Cpu.program_counter = 0x3469 then
        Printf.printf "Self-test ............ OK (%d cycles)\n" !count
    else Printf.printf "Self-test ............ KO (trap at %.4X)\n" !Cpu.program_counter

let test2 () =
    Cpu.reset () ;
    let count = ref 0 in
    load_rom "test_roms/nestest.bin" ;
    let continue = ref true in
    Cpu.stack_pointer := 0xFD ;
    Cpu.enable_decimal := false ;
    Cpu.processor_status := 0x24 ;
    Cpu.program_counter := 0xC000 ;
    while !continue do
        incr count ;
        Cpu.print_state () ;
        Cpu.fetch_instr () ;
        if !Cpu.program_counter = 0xC66E then
            continue := false
    done ;
    if Cpu.memory.(2) = 0 && Cpu.memory.(3) = 0 then
        Printf.printf "Advanced self-test ... OK (%d cycles)\n" !count
    else
        Printf.printf "Advanced self-test ... KO (errors %.2X %.2X)\n" Cpu.memory.(2) Cpu.memory.(3)

let tests =
  test1 () ;
  test2 ()
