include module type of Distributed_intf

val disabled : (module S)

val irmin : unit -> (module S)
