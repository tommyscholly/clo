(lang dune 3.12)

(name kali)

(generate_opam_files true)

(using menhir 2.1)

(package
 (name kali)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
