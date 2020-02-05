open Stdune
include Dune_cache_intf
module Key = Key
module Local = Local

let promotion_to_string = function
  | Already_promoted { in_the_cache; in_the_build_directory; _ } ->
    Printf.sprintf "%s already promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_cache)
  | Promoted { in_the_cache; in_the_build_directory; _ } ->
    Printf.sprintf "%s promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_cache)

let command_to_dyn = function
  | Dedup { in_the_build_directory; in_the_cache; digest } ->
    let open Dyn.Encoder in
    record
      [ ("in_the_build_directory", Path.Build.to_dyn in_the_build_directory)
      ; ("in_the_cache", Path.to_dyn in_the_cache)
      ; ("digest", Digest.to_dyn digest)
      ]

let make_caching (type t) (module Caching : Cache with type t = t) (cache : t) :
    (module Caching) =
  ( module struct
    module Cache = Caching

    let cache = cache
  end )
