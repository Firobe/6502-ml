let load_rom path =
    let file = open_in_bin path in
    let store = Bytes.create 0x10000 in
    let read = input file store 0 0x10000 in
    let store = Bytes.sub store 0 read in
    Bytes.iteri (fun i el -> Array.set Cpu.memory (i) (int_of_char el)) store ;
    Printf.printf "Loaded %d bytes into the memory\n%!" read

let main =
    let count = ref 0 in
    if Array.length Sys.argv > 1 then
        load_rom Sys.argv.(1)
    ;
    let continue = ref true in
    Cpu.program_counter := 0x400 ;
    while !continue do
        count := !count + 1 ;
        let back = !Cpu.program_counter in
        Cpu.next_instr () ;
        if back = !Cpu.program_counter then
            continue := false
    done ;
    Printf.printf "END : trap encountered at %.4X\n%!" !Cpu.program_counter ;
    Printf.printf "Lasted %d cycles...\n" !count
