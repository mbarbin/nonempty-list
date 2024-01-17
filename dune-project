(lang dune 3.12)

(name nonempty-list)

(generate_opam_files)

(license MIT)

(authors "Mathieu Barbin")

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
   (>= 5.1))
  (base
   (and
    (>= v0.16)
    (< v0.17)))
  (ppx_jane
   (and
    (>= v0.16)
    (< v0.17)))
  (reversed-list
   (and
    (>= v0.16)
    (< v0.17)))))
