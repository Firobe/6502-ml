val memory : int array
val program_counter : int ref
val stack_pointer : int ref
val processor_status : int ref
val accumulator : int ref
val index_register_x : int ref
val index_register_y : int ref
val enable_decimal : bool ref

val fetch_instr : unit -> unit
val print_state : unit -> unit
val reset : unit -> unit
