(lang dune 3.17)

(name nonempty-list)

(generate_opam_files)

(license MIT)

(authors "Jane Street Group, LLC")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/nonempty-list))

(documentation "https://mbarbin.github.io/nonempty-list/")

(implicit_transitive_deps false)

(package
 (name nonempty-list)
 (synopsis
  "Repackaging core_kernel.nonempty_list as a standalone package for using with Base")
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (base
   (>= v0.17))
  (base_quickcheck
   (>= v0.17))
  (ppx_compare
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (reversed-list
   (and
    (>= v0.17)))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))
