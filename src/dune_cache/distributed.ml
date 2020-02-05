open Stdune
open Dune_cache_intf
include Distributed_intf

module WeakKey = struct
  type t = Digest.t

  let of_paths paths =
    List.map ~f:(fun p -> Path.Local.to_string (Path.Build.local p)) paths
    |> Digest.generic

  let to_string = Digest.to_string
end

let disabled =
  ( module struct
    type t = unit

    let v = ()

    let distribute _ = Result.Ok ()
  end : S )

let _irmin (type t)
    (module Store : Irmin.S
      with type t = t
       and type key = string list
       and type contents = string) (store : t Lwt.t) =
  ( module struct
    include Store

    let v = Lwt_main.run store

    let ( let* ) = Lwt.bind

    let find_or_create_tree tree path =
      Store.Tree.find_tree tree path
      |> Lwt.map (Option.value ~default:Store.Tree.empty)

    let distribute { key; metadata; files } =
      let weak_key =
        List.map ~f:(fun (f : File.t) -> f.in_the_build_directory) files
        |> WeakKey.of_paths |> WeakKey.to_string
      in
      let insert (root : Store.tree option) =
        let root =
          match root with
          | None -> Store.Tree.empty
          | Some tree -> tree
        and insert_file tree { File.digest; in_the_cache; _ } =
          let contents = Io.read_file in_the_cache in
          Store.Tree.add tree [ Digest.to_string digest ] contents
        in
        let* tree_files =
          let* tree_files = find_or_create_tree root [ "files" ] in
          Lwt_list.fold_left_s insert_file tree_files files
        in
        let* tree_metadata =
          let* tree_metadata = find_or_create_tree root [ "meta" ] in
          let* weak = find_or_create_tree tree_metadata [ weak_key ] in
          let* weak = Store.Tree.add weak [ Digest.to_string key ] metadata in
          Store.Tree.add_tree tree_metadata [ weak_key ] weak
        in
        let* root = Store.Tree.add_tree root [ "files" ] tree_files in
        let* root = Store.Tree.add_tree root [ "meta" ] tree_metadata in
        Lwt.return (Some root)
      and info () =
        let author = "dune-cache <https://github.com/ocaml/dune>"
        and date = Int64.of_float (Unix.gettimeofday ())
        and message =
          Format.asprintf "Promotion of rule %s" (Digest.to_string key)
        in
        Irmin.Info.v ~author ~date message
      in
      let store =
        Lwt.map
          (Result.map_error ~f:(fun _ -> "FIXME irmin write error"))
          (Store.with_tree ~info v [] insert)
      in
      Lwt_main.run store
  end : S )

let irmin () =
  let module Store = Irmin_mem.KV (Irmin.Contents.String) in
  let store =
    let open Lwt.Infix in
    Store.Repo.v (Irmin_mem.config ()) >>= fun repo -> Store.master repo
  in
  _irmin (module Store) store

module GitFS = struct
  include Git_unix.Store

  let v ?dotgit ?compression ?buffers root =
    let buffer =
      match buffers with
      | None -> None
      | Some p -> Some (Lwt_pool.use p)
    in
    v ?dotgit ?compression ?buffer root
end

let irmin_git path =
  let module Store =
    Irmin_git.KV (GitFS) (Git_unix.Sync (GitFS)) (Irmin.Contents.String)
  in
  let store =
    let open Lwt.Infix in
    Store.Repo.v (Irmin_git.config ~bare:false path) >>= fun repo ->
    Store.master repo
  in
  _irmin (module Store) store
