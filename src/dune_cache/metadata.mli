open Stdune
open Dune_cache_intf
open Local_intf

module FirstTwoCharsSubdir : FSScheme

type t =
  { metadata : Sexp.t list
  ; files : File.t list
  }

val to_sexp : t -> Sexp.t

val of_sexp : Sexp.t -> (t, string) Result.t

val of_string : string -> (t, string) Result.t

val parse : Path.t -> (t, string) Result.t
