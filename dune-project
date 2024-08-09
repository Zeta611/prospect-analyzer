(lang dune 3.2)
(using menhir 2.0)

(name prospect_analyzer)

(generate_opam_files true)

(source
 (github Zeta611/prospect-analyzer))

(authors "Jay Lee")

(maintainers "Jay Lee")

(license MIT)

(package
 (name prospect_analyzer)
 (synopsis "prospect analyzer")
 (description "prospect analyzer")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
