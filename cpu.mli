val memory : int array
val program_counter : int ref

val next_instr : ?debug:bool -> unit -> unit
val print_registers : unit -> unit
