(lang dune 3.11)

(name mop)

(generate_opam_files true)

(source
 (github "9glenda/mop"))

(authors "9glenda")

(maintainers "9glenda")

(license LICENSE)

(documentation https://github.com/9glenda/mop)

(package
 (name mop)
 (synopsis "Simple [M]onadic [O]caml [P]arser Combinator")
 (description "")
 (depends 
   dune
   (ocaml (>= 5.1.0))
   ; jane street
   (core (>= 0.16.0))
   ; (core_extended (>= 0.16.0))
   ; ppx
   ppx_deriving
  )
 (tags
  (topics "ocaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
