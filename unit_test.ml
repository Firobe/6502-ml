let load_rom path =
    let file = open_in_bin path in
    let store = Bytes.create 0x10000 in
    let read = input file store 0 0x10000 in
    let store = Bytes.sub store 0 read in
    Bytes.iteri (fun i el -> Array.set Cpu.memory (i) (int_of_char el)) store

let main =
    let count = ref 0 in
    load_rom "test.bin" ;
    let continue = ref true in
    Cpu.program_counter := 0x400 ;
    Cpu.processor_status := 0x00 ;
    while !continue do
        count := !count + 1 ;
        let back = !Cpu.program_counter in
        Cpu.fetch_instr () ;
        if back = !Cpu.program_counter then
            continue := false
    done ;
    if !Cpu.program_counter = 0x3469 then
        Printf.printf "Self-test OK (%d cycles)\n" !count
    else Printf.printf "Self-test KO : %.4X\n" !Cpu.program_counter
