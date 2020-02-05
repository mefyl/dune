open Stdune

include module type of Dune_cache_intf

module Key : sig
  type t = Digest.t

  val of_string : string -> (t, string) Result.t

  val to_string : t -> string
end

val promotion_to_string : promotion -> string

val command_to_dyn : command -> Dyn.t

module Local = Local

val make_caching : (module Cache with type t = 'a) -> 'a -> (module Caching)
