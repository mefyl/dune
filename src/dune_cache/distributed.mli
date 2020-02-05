open Stdune
open Local_intf

include module type of Distributed_intf

module WeakKey : WeakKey

module type S = sig
  type t

  val v : t

  val distribute : distribution -> (unit, string) Result.t

  val prefetch :
    WeakKey.t -> Path.t -> (module FSScheme) -> (int, string) Result.t
end

val disabled : (module S)

val irmin : unit -> (module S)

val irmin_git : string -> (module S)
