open Stdune
open Dune_cache_intf
open Local_intf
open Result.O

(* Where to store file with a given hash. In this case ab/abcdef. *)
module FirstTwoCharsSubdir : FSScheme = struct
  let path root hash =
    let hash = Digest.to_string hash in
    let short_hash = String.sub hash ~pos:0 ~len:2 in
    Path.L.relative root [ short_hash; hash ]

  let digest path =
    match Digest.from_hex (Path.basename (fst (Path.split_extension path))) with
    | Some digest -> digest
    | None ->
      Code_error.raise "strange cached file path (not a valid hash)"
        [ (Path.to_string path, Path.to_dyn path) ]

  let list root =
    let f dir =
      let is_hex_char c =
        let char_in s e = Char.compare c s >= 0 && Char.compare c e <= 0 in
        char_in 'a' 'f' || char_in '0' '9'
      and root = Path.L.relative root [ dir ] in
      if String.for_all ~f:is_hex_char dir then
        Array.map ~f:(Path.relative root) (Sys.readdir (Path.to_string root))
      else
        Array.of_list []
    in
    Array.to_list
      (Array.concat
         (Array.to_list (Array.map ~f (Sys.readdir (Path.to_string root)))))

  let path_files root = Path.relative root "files"

  let path_meta root = Path.relative root "meta"
end

type t =
  { metadata : Sexp.t list
  ; files : File.t list
  }

let to_sexp { metadata; files } =
  let open Sexp in
  let f ({ in_the_build_directory; in_the_cache; _ } : File.t) =
    Sexp.List
      [ Sexp.Atom
          (Path.Local.to_string (Path.Build.local in_the_build_directory))
      ; Sexp.Atom (Path.to_string in_the_cache)
      ]
  in
  List
    [ List (Atom "metadata" :: metadata)
    ; List (Atom "files" :: List.map ~f files)
    ]

let of_sexp = function
  | Sexp.List
      [ List (Atom "metadata" :: metadata); List (Atom "files" :: produced) ] ->
    let+ files =
      Result.List.map produced ~f:(function
        | List [ Atom in_the_build_directory; Atom in_the_cache ] ->
          let in_the_build_directory =
            Path.Build.of_string in_the_build_directory
          and in_the_cache = Path.of_string in_the_cache in
          Ok
            { File.in_the_cache
            ; in_the_build_directory
            ; digest = FirstTwoCharsSubdir.digest in_the_cache
            }
        | _ -> Error "invalid metadata scheme in produced files list")
    in
    { metadata; files }
  | _ -> Error "invalid metadata"

let of_string repr = Csexp.parse_string repr >>= of_sexp

let parse path =
  Io.with_file_in path ~f:(fun input -> Csexp.parse (Stream.of_channel input))
  >>= of_sexp
