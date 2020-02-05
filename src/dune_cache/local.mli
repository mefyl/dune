open Stdune

include module type of Local_intf

val default_root : unit -> Path.t

include S
