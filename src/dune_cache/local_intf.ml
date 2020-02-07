open Stdune
open Dune_cache_intf

module type FSScheme = sig
  val path : Path.t -> Digest.t -> Path.t

  val digest : Path.t -> Digest.t

  val list : Path.t -> Path.t list

  val path_files : Path.t -> Path.t

  val path_meta : Path.t -> Path.t
end

module type S = sig
  include Cache

  val promote_sync :
       t
    -> (Path.Build.t * Digest.t) list
    -> Key.t
    -> metadata
    -> int option
    -> Duplication_mode.t option
    -> (promotion list, string) Result.t

  val make :
       ?root:Path.t
    -> ?duplication_mode:Duplication_mode.t
    -> handler
    -> (t, string) Result.t

  val duplication_mode : t -> Duplication_mode.t

  (** The size overhead of cached files. That is, the total size of cached files
      that are not linked in a build directory. *)
  val size : t -> int

  module Trimming_result : sig
    type t =
      { trimmed_files_size : int
      ; trimmed_files : Path.t list
      ; trimmed_metafiles : Path.t list
      }
  end

  (** [trim cache size] removes files from [cache], starting with the least
      recently used one, until [size] bytes have been freed. *)
  val trim : t -> int -> Trimming_result.t

  (** Purge invalid or incomplete cached rules. *)
  val garbage_collect : t -> Trimming_result.t
end
