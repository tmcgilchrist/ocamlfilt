# ocamlfilt

[![GitHub CI][github-shield]][github-ci]

Demangle OCaml symbol names. ocamlfilt works similarly to c++filt, in that it accepts mangled symbol names as command line arguments, and if none are provided it accepts mangled symbols from stdin. Demangled symbols are written to stdout.

OCaml compiler has had various mangling schemes. This tool recognises the follow formats:
 * Classic: Used in OCaml 4.14 and OCaml up to 5.4, uses scheme of `camlModule__name_NNN` to represent symbols in modules. 5.1.0 replaced `__` with `.` as the separator. see #8998 etc below.
 * Classic 5.4: Replaced '.' and '__' separators with '$' see #13050 below.

[#8998](https://github.com/ocaml/ocaml/pull/8998), [#11321](https://github.com/ocaml/ocaml/pull/11321), [#11430](https://github.com/ocaml/ocaml/pull/11430): change mangling of OCaml long identifiers
  from `camlModule__name_NNN` to `camlModule.name_NNN`.  The previous mangling schema, using `__`, was ambiguous.

[#13050](https://github.com/ocaml/ocaml/pull/13050): Use '$' instead of '.' to separate module names in symbol names. This changes mangling of OCaml identifiers from `camlModule.name_NNN` to `camlModule$name_NNN`. Additionally it changes the encoding of special characters from $xx (two hex digits) to $$xx (two dollar signs followed by two hex digits). Mangled names are now consistent across all platforms.

## Installation

``` shell
opam install ocamlfilt
```

## Usage

To demangle a file run:

``` shell
ocamlfilt -i mangled.txt -o demangled.txt
```

Alternatively ocamlfilt can accept data from stdin and pipe to stdout:

``` shell
curl http://example.com/mangled-symbols.txt | ocamlfilt | less
```


 [github-shield]: https://github.com/tmcgilchrist/ocamlfilt/actions/workflows/build.yaml/badge.svg
 [github-ci]: https://github.com/tmcgilchrist/ocamlfilt/actions/workflows/build.yaml