(lang dune 3.0)
(name muhfl)
(generate_opam_files)
(using menhir 2.0)
(source (github hopv/muhfl))
(maintainers "Ken Sakayori <sakayori@kb.is.s.u-tokyo.ac.jp")

(package
  (name muhfl)
  (synopsis "A HFLZ solver")
  ; (description "A longer description")
  (depends
          ocaml
          async
          cmdliner
          core 
          core_kernel
          (core_unix (= v0.14.0))
          fmt
          logs
          lwt
          menhir
          menhirLib
          ppx_compare
          ppx_deriving
          ppx_deriving_cmdliner
          ppx_let
          ppx_sexp_conv
          yojson
          z3
 ))
