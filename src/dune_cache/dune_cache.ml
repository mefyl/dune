open Stdune
module Key = Key
include Dune_cache_intf
open Result.O

module Trimming_result = struct
  type t =
    { trimmed_files_size : int
    ; trimmed_files : Path.t list
    ; trimmed_metafiles : Path.t list
    }

  let empty =
    { trimmed_files_size = 0; trimmed_files = []; trimmed_metafiles = [] }

  let add t ~size ~file =
    { t with
      trimmed_files = file :: t.trimmed_files
    ; trimmed_files_size = size + t.trimmed_files_size
    }
end

type 'a result = ('a, string) Result.t

let default_root () =
  Path.L.relative (Path.of_string Xdg.cache_dir) [ "dune"; "db"; "v2" ]

let promotion_to_string = function
  | Already_promoted { in_the_cache; in_the_build_directory; _ } ->
    Printf.sprintf "%s already promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_cache)
  | Promoted { in_the_cache; in_the_build_directory; _ } ->
    Printf.sprintf "%s promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_cache)

let file_of_promotion = function
  | Already_promoted f
  | Promoted f ->
    f

let command_to_dyn = function
  | Dedup { in_the_build_directory; in_the_cache; digest } ->
    let open Dyn.Encoder in
    record
      [ ("in_the_build_directory", Path.Build.to_dyn in_the_build_directory)
      ; ("in_the_cache", Path.to_dyn in_the_cache)
      ; ("digest", Digest.to_dyn digest)
      ]

(* How to handle collisions. E.g. another version could assume collisions are
   not possible *)
module Collision = struct
  type res =
    | Found of Path.t
    | Not_found of Path.t

  (* We need to ensure we do not create holes in the suffix numbering for this
     to work *)
  let search path file =
    let rec loop n =
      let path = Path.extend_basename path ~suffix:("." ^ string_of_int n) in
      if Path.exists path then
        if Io.compare_files path file = Ordering.Eq then
          Found path
        else
          loop (n + 1)
      else
        Not_found path
    in
    loop 1
end

module type FSScheme = sig
  val path : Path.t -> Digest.t -> Path.t

  val digest : Path.t -> Digest.t

  val list : Path.t -> Path.t list
end

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
end

module FSSchemeImpl = FirstTwoCharsSubdir

let apply ~f o v =
  match o with
  | Some o -> f v o
  | None -> v

module Metadata_file = struct
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
        [ List (Atom "metadata" :: metadata); List (Atom "files" :: produced) ]
      ->
      let+ files =
        Result.List.map produced ~f:(function
          | List [ Atom in_the_build_directory; Atom in_the_cache ] ->
            let in_the_build_directory =
              Path.Build.of_string in_the_build_directory
            and in_the_cache = Path.of_string in_the_cache in
            Ok
              { File.in_the_cache
              ; in_the_build_directory
              ; digest = FSSchemeImpl.digest in_the_cache
              }
          | _ -> Error "invalid metadata scheme in produced files list")
      in
      { metadata; files }
    | _ -> Error "invalid metadata"

  let parse path =
    Io.with_file_in path ~f:(fun input -> Csexp.parse (Stream.of_channel input))
    >>= of_sexp
end

module Cache = struct
  type t =
    { build_root : Path.t option
    ; distributed : (module Distributed.S)
    ; duplication_mode : Duplication_mode.t
    ; handler : handler
    ; repositories : repository list
    ; root : Path.t
    ; temp_dir : Path.t
    }

  let path_files cache = Path.relative cache.root "files"

  let path_meta cache = Path.relative cache.root "meta"

  let make_path cache path =
    match cache.build_root with
    | Some p -> Result.ok (Path.append_local p path)
    | None ->
      Result.Error
        (Format.asprintf "relative path \"%a\" while no build root was set"
           Path.Local.pp path)

  let search cache hash file =
    Collision.search (FSSchemeImpl.path (path_files cache) hash) file

  let with_repositories cache repositories = { cache with repositories }

  let duplicate ?(duplication = None) cache =
    match Option.value ~default:cache.duplication_mode duplication with
    | Copy -> fun src dst -> Io.copy_file ~src ~dst ()
    | Hardlink -> Path.link

  let retrieve cache (file : File.t) =
    let path = Path.build file.in_the_build_directory in
    Log.infof "retrieve %s from cache" (Path.to_string_maybe_quoted path);
    duplicate cache file.in_the_cache path;
    path

  let deduplicate cache (file : File.t) =
    match cache.duplication_mode with
    | Copy -> ()
    | Hardlink -> (
      let target = Path.Build.to_string file.in_the_build_directory in
      let tmpname = Path.Build.to_string (Path.Build.of_string ".dedup") in
      Log.infof "deduplicate %s from %s" target
        (Path.to_string file.in_the_cache);
      let rm p = try Unix.unlink p with _ -> () in
      try
        rm tmpname;
        Unix.link (Path.to_string file.in_the_cache) tmpname;
        Unix.rename tmpname target
      with Unix.Unix_error (e, syscall, _) ->
        rm tmpname;
        Log.infof "error handling dune-cache command: %s: %s" syscall
          (Unix.error_message e) )

  let promote_sync cache paths key metadata repo duplication =
    let open Result.O in
    let* repo =
      match repo with
      | Some idx -> (
        match List.nth cache.repositories idx with
        | None ->
          Result.Error (Printf.sprintf "repository out of range: %i" idx)
        | repo -> Result.Ok repo )
      | None -> Result.Ok None
    in
    let metadata =
      apply
        ~f:(fun metadata repository ->
          metadata
          @ [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom repository.remote ]
            ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom repository.commit ]
            ])
        repo metadata
    in
    let promote (path, expected_hash) =
      let* abs_path = make_path cache (Path.Build.local path) in
      Log.infof "promote %s" (Path.to_string abs_path);
      let stat = Unix.lstat (Path.to_string abs_path) in
      let* stat =
        if stat.st_kind = S_REG then
          Result.Ok stat
        else
          Result.Error
            (Format.sprintf "invalid file type: %s"
               (Path.string_of_file_kind stat.st_kind))
      in
      let prepare path =
        let dest = Path.relative cache.temp_dir "data" in
        if Path.exists dest then Path.unlink dest;
        duplicate ~duplication cache path dest;
        dest
      in
      let tmp = prepare abs_path in
      let effective_hash = Digest.file_with_stats tmp (Path.stat tmp) in
      if Digest.compare effective_hash expected_hash != Ordering.Eq then (
        let message =
          Printf.sprintf "hash mismatch: %s != %s"
            (Digest.to_string effective_hash)
            (Digest.to_string expected_hash)
        in
        Log.infof "%s" message;
        Result.Error message
      ) else
        match search cache effective_hash tmp with
        | Collision.Found in_the_cache ->
          Path.unlink tmp;
          Path.touch in_the_cache;
          Result.Ok
            (Already_promoted
               { in_the_build_directory = path
               ; in_the_cache
               ; digest = effective_hash
               })
        | Collision.Not_found in_the_cache ->
          Path.mkdir_p (Path.parent_exn in_the_cache);
          let dest = Path.to_string in_the_cache in
          Unix.rename (Path.to_string tmp) dest;
          (* Remove write permissions *)
          Unix.chmod dest (stat.st_perm land 0o555);
          Result.Ok
            (Promoted
               { in_the_build_directory = path
               ; in_the_cache
               ; digest = effective_hash
               })
    in
    let* promoted = Result.List.map ~f:promote paths in
    let metadata_path = FSSchemeImpl.path (path_meta cache) key
    and metadata_tmp_path = Path.relative cache.temp_dir "metadata"
    and files = List.map ~f:file_of_promotion promoted in
    let metadata_file : Metadata_file.t = { metadata; files } in
    let metadata = Csexp.to_string (Metadata_file.to_sexp metadata_file) in
    Io.write_file metadata_tmp_path metadata;
    let () =
      match Io.read_file metadata_path with
      | contents ->
        if contents <> metadata then
          User_warning.emit
            [ Pp.textf "non reproductible collision on rule %s"
                (Digest.to_string key)
            ]
      | exception Sys_error _ -> Path.mkdir_p (Path.parent_exn metadata_path)
    in
    Path.rename metadata_tmp_path metadata_path;
    let module D = (val cache.distributed) in
    let* files =
      let f (path, digest) =
        let+ path = make_path cache (Path.Build.local path) in
        (digest, Io.read_file ~binary:true path)
      in
      Result.List.map ~f paths
    in
    let+ () = D.distribute { key; metadata; metadata_path; files } in
    let f = function
      | Already_promoted file when cache.duplication_mode <> Copy ->
        cache.handler (Dedup file)
      | _ -> ()
    in
    List.iter ~f promoted;
    promoted

  let promote cache paths key metadata ~repository ~duplication =
    Result.map ~f:ignore
      (promote_sync cache paths key metadata repository duplication)

  let search cache key =
    let path = FSSchemeImpl.path (path_meta cache) key in
    let* sexp =
      try
        Io.with_file_in path ~f:(fun input ->
            Csexp.parse (Stream.of_channel input))
      with Sys_error _ -> Error "no cached file"
    in
    let+ metadata = Metadata_file.of_sexp sexp in
    (* Touch cache files so they are removed last by LRU trimming *)
    let () =
      let f (file : File.t) =
        (* There is no point in trying to trim out files that are missing : dune
           will have to check when hardlinking anyway since they could disappear
           inbetween. *)
        try Path.touch file.in_the_cache
        with Unix.(Unix_error (ENOENT, _, _)) -> ()
      in
      List.iter ~f metadata.files
    in
    (metadata.metadata, metadata.files)

  let set_build_dir cache p = { cache with build_root = Some p }

  let teardown cache = Path.rm_rf ~allow_external:true cache.temp_dir

  let detect_duplication_mode root =
    let () = Path.mkdir_p root in
    let beacon = Path.relative root "beacon"
    and target = Path.relative Path.build_dir ".cache-beacon" in
    let () = Path.touch beacon in
    let rec test () =
      match Path.link beacon target with
      | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
        Path.unlink_no_err target;
        test ()
      | exception Unix.Unix_error _ -> Duplication_mode.Copy
      | () -> Duplication_mode.Hardlink
    in
    test ()

  let make ?(root = default_root ())
      ?(duplication_mode = detect_duplication_mode root) handler =
    if Path.basename root <> "v2" then
      Result.Error "unable to read dune-cache"
    else
      Result.ok
        { build_root = None
        ; distributed = Distributed.irmin ()
        ; duplication_mode
        ; handler
        ; repositories = []
        ; root
        ; temp_dir =
            Path.temp_dir ~temp_dir:root "promoting"
              (string_of_int (Unix.getpid ()))
        }

  let duplication_mode cache = cache.duplication_mode
end

let trimmable stats = stats.Unix.st_nlink = 1

let _garbage_collect default_trim cache =
  let path = Cache.path_meta cache in
  let metas =
    List.map ~f:(fun p -> (p, Metadata_file.parse p)) (FSSchemeImpl.list path)
  in
  let f default_trim = function
    | p, Result.Error msg ->
      Log.infof "remove invalid metadata file %a: %s" Path.pp p msg;
      Path.unlink_no_err p;
      { default_trim with Trimming_result.trimmed_metafiles = [ p ] }
    | p, Result.Ok { Metadata_file.files; _ } ->
      if
        List.for_all
          ~f:(fun { File.in_the_cache; _ } -> Path.exists in_the_cache)
          files
      then
        default_trim
      else (
        Log.infof "remove metadata file %a as some produced files are missing"
          Path.pp p;
        let res =
          List.fold_left ~init:default_trim
            ~f:(fun trim f ->
              let p = f.File.in_the_cache in
              try
                let stats = Path.stat p in
                if trimmable stats then (
                  Path.unlink_no_err p;
                  Trimming_result.add trim ~file:p ~size:stats.st_size
                ) else
                  trim
              with Unix.Unix_error (Unix.ENOENT, _, _) -> trim)
            files
        in
        Path.unlink_no_err p;
        res
      )
  in
  List.fold_left ~init:default_trim ~f metas

let garbage_collect = _garbage_collect Trimming_result.empty

let trim cache free =
  let path = Cache.path_files cache in
  let files = FSSchemeImpl.list path in
  let f path =
    let stats = Path.stat path in
    if trimmable stats then
      Some (path, stats.st_size, stats.st_mtime)
    else
      None
  and compare (_, _, t1) (_, _, t2) = Ordering.of_int (Stdlib.compare t1 t2) in
  let files = List.sort ~compare (List.filter_map ~f files)
  and delete (trim : Trimming_result.t) (path, size, _) =
    if trim.trimmed_files_size >= free then
      trim
    else (
      Path.unlink path;
      Trimming_result.add trim ~size ~file:path
    )
  in
  let trim = List.fold_left ~init:Trimming_result.empty ~f:delete files in
  _garbage_collect trim cache

let size cache =
  let root = Cache.path_files cache in
  let files = FSSchemeImpl.list root in
  let stats =
    let f p =
      try
        let stats = Path.stat p in
        if trimmable stats then
          stats.st_size
        else
          0
      with Unix.Unix_error (Unix.ENOENT, _, _) -> 0
    in
    List.map ~f files
  in
  List.fold_left ~f:(fun acc size -> acc + size) ~init:0 stats

let make_caching (type t) (module Caching : Cache with type t = t) (cache : t) :
    (module Caching) =
  ( module struct
    module Cache = Caching

    let cache = cache
  end )
