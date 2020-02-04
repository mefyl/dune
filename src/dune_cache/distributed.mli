include module type of Distributed_intf

val disabled : (module S)

val irmin : string -> (module S)
