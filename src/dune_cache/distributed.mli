include module type of Distributed_intf

module WeakKey : WeakKey

val disabled : (module S)

val irmin : unit -> (module S)

val irmin_git : string -> (module S)
