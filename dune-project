(lang dune 3.7)
(using ctypes 0.3)
(name ocamlfilt)
(generate_opam_files true)

(license BSD-3-Clause)
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(authors "Tim McGilchrist <timmcgil@gmail.com>")
(source (github  tmcgilchrist/ocamlfilt))

(package
 (name ocamlfilt)
 (synopsis "Demangle OCaml symbol names")
 (description "OCaml implementation of demangling OCaml symbols. ocamlfilt works similarly to c++filt, in that it accepts mangled symbol names as command line arguments, and if none are provided it accepts mangled symbols from stdin. Demangled symbols are written to stdout.")
 (depends
  (ocaml (>= 4.14))
  (cmdliner (>= 1.3))
  (alcotest (and :with-test (>= 1.9.0)))
  (qcheck-alcotest (and :with-test (>= 0.25)))))