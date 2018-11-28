val memory : int array
val program_counter : int ref
val stack_pointer : int ref
val processor_status : int ref
val acc : int ref
val irx : int ref
val iry : int ref
val enable_decimal : bool ref
val cycle_count : int ref

val fetch_instr : unit -> unit
val print_state : unit -> unit
val reset : unit -> unit
val init_pc : unit -> unit
