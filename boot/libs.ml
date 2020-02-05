let executables = [ "main" ]

let external_libraries =
  [ "unix"
  ; "threads.posix"
  ; "bytes"
  ; "astring"
  ; "fpath"
  ; "bigarray-compat"
  ; "cstruct"
  ; "seq"
  ; "stdlib-shims"
  ; "fmt"
  ; "result"
  ; "lwt"
  ; "re"
  ; "re.posix"
  ; "stringext"
  ; "uri"
  ; "logs"
  ; "hex"
  ; "psq"
  ; "lru"
  ; "rresult"
  ; "ke"
  ; "bigstringaf"
  ; "angstrom"
  ; "encore.butils"
  ; "encore"
  ; "duff"
  ; "ocamlgraph"
  ; "ocplib-endian"
  ; "bigarray"
  ; "ocplib-endian.bigstring"
  ; "optint"
  ; "checkseum.laolao"
  ; "checkseum.c"
  ; "decompress.impl"
  ; "decompress"
  ; "eqaf"
  ; "digestif.rakia"
  ; "digestif.c"
  ; "git"
  ; "mmap"
  ; "logs.fmt"
  ; "threads"
  ; "lwt.unix"
  ; "sexplib0"
  ; "ppx_sexp_conv.runtime-lib"
  ; "base.base_internalhash_types"
  ; "base.caml"
  ; "base.shadow_stdlib"
  ; "base"
  ; "fieldslib"
  ; "uri-sexp"
  ; "base64"
  ; "cohttp"
  ; "logs.lwt"
  ; "cohttp-lwt"
  ; "git-http"
  ; "parsexp"
  ; "sexplib"
  ; "macaddr"
  ; "domain-name"
  ; "ipaddr"
  ; "ipaddr-sexp"
  ; "conduit"
  ; "conduit-lwt"
  ; "uri.services"
  ; "ipaddr.unix"
  ; "conduit-lwt-unix"
  ; "magic-mime"
  ; "cohttp-lwt-unix"
  ; "git-unix"
  ; "uchar"
  ; "uutf"
  ; "jsonm"
  ; "irmin"
  ; "irmin-git"
  ; "irmin-mem"
  ]

let local_libraries =
  [ ("src/stdune", Some "Stdune", false, None)
  ; ("src/dune_lang", Some "Dune_lang", false, None)
  ; ("vendor/incremental-cycles/src", Some "Incremental_cycles", false, None)
  ; ("src/dag", Some "Dag", false, None)
  ; ("src/fiber", Some "Fiber", false, None)
  ; ("src/memo", Some "Memo", false, None)
  ; ("src/xdg", Some "Xdg", false, None)
  ; ("src/dune_cache", Some "Dune_cache", false, None)
  ; ("src/dune_cache_daemon", Some "Dune_cache_daemon", false, None)
  ; ("vendor/re/src", Some "Dune_re", false, None)
  ; ("vendor/opam-file-format/src", None, false, None)
  ; ("otherlibs/dune-glob", Some "Dune_glob", false, None)
  ; ("src/ocaml-config", Some "Ocaml_config", false, None)
  ; ("src/catapult", Some "Catapult", false, None)
  ; ("src/jbuild_support", Some "Jbuild_support", false, None)
  ; ("otherlibs/action-plugin/src", Some "Dune_action_plugin", false, None)
  ; ("src/dune", Some "Dune", true, None)
  ; ("vendor/cmdliner/src", None, false, None)
  ;
  ("otherlibs/build-info/src", Some "Build_info", false,
  Some "build_info_data")
  ]
