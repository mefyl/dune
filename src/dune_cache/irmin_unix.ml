(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_unix.endpoint

  val remote : string -> Irmin.remote
end

module KV (G : Irmin_git.G) (C : Irmin.Contents.S) = struct
  include Irmin_git.KV (G) (Git_unix.Sync (G)) (C)

  let remote uri =
    let e = Git_unix.endpoint (Uri.of_string uri) in
    E e
end

module FS = struct
  module G = struct
    include Git_unix.Store

    let v ?dotgit ?compression ?buffers root =
      let buffer =
        match buffers with
        | None -> None
        | Some p -> Some (Lwt_pool.use p)
      in
      v ?dotgit ?compression ?buffer root
  end

  module KV = KV (G)
end
