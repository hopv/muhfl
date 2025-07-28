(lang dune 3.19)
(name muhfl)
(generate_opam_files)
(using menhir 3.0)
(source (github hopv/muhfl))
(maintainers "Ken Sakayori <sakayori@kb.is.s.u-tokyo.ac.jp")

(package
  (name muhfl)
  (synopsis "A HFLZ solver")
  ; (description "A longer description")
  (depends
          (ocaml (and (>= 4.14) (< 5.0)))
          async
          cmdliner
          (core (and (>= v0.16) (< v0.17))) ; core v.0.17 is supposed to be for Ocaml 5.0
          (core_unix (and (>= v0.16) (< v0.17))) ; core v.0.17 is supposed to be for Ocaml 5.0
          fmt
          hfl
          logs
          lwt
          menhir
          menhirLib
          ppx_compare
          ppx_deriving
          (ppx_deriving_cmdliner (= "dev"))
          ppx_let
          ppx_sexp_conv
          yojson
          z3
 ))
