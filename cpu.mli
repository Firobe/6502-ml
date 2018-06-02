val memory : int array
val program_counter : int ref

val next_instr : ?debug:bool -> unit -> unit
val dump_registers : unit -> unit
