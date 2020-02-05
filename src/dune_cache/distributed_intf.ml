open Stdune
open Dune_cache_intf

type distribution =
  { key : Digest.t
  ; metadata : string
  ; files : File.t list
  }

module type WeakKey = sig
  type t

  val of_paths : Path.Build.t list -> t

  val to_string : t -> string
end

module type S = sig
  type t

  val v : t

  val distribute : distribution -> (unit, string) Result.t
end
