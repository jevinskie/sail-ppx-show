(lang dune 3.12)

(name sail_ppx_show)

(generate_opam_files true)

(source
 (github jevinskie/sail_ppx_show))

(authors "Jevin Sweval")

(maintainers "Jevin Sweval")

(license BSD-2-Clause)

(documentation https://github.com/jevinskie/sail_ppx_show)

(package
 (name sail_ppx_show)
 (synopsis
  "A ppxlib preprocessor to add [@@deriving show] to various parts of Sail")
 (description
  "A ppxlib preprocessor to add [@@deriving show] to various parts of Sail")
 (depends ocaml dune)
 (tags
  (sail ppx ppxlib deriving_show)))
