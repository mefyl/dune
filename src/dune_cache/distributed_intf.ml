open Stdune

type distribution =
  { key : Digest.t
  ; metadata : string
  ; metadata_path : Path.t
  ; files : (Digest.t * string) list
  }

module type S = sig
  type t

  val v : t

  val distribute : distribution -> (unit, string) Result.t
end
