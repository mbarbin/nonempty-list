(lang dune 3.16)

(name nonempty-list)

(generate_opam_files)

(license MIT)

(authors "Jane Street Group, LLC")

(maintainers "Mathieu Barbin")

(source
 (github mbarbin/nonempty-list))

(documentation "https://mbarbin.github.io/nonempty-list/")

(package
 (name nonempty-list)
 (synopsis
  "Repackaging core_kernel.nonempty_list as a standalone package for using with Base")
 (depends
  (ocaml
   (>= 5.2))
  (base
   (and
    (>= v0.17)
    (< v0.18)))
  (base_quickcheck
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_compare
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_hash
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_value
   (and
    (>= v0.17)
    (< v0.18)))
  (reversed-list
   (and
    (>= v0.17)
    (< v0.18)))))
