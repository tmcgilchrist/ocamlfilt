(** Various OCaml de-mangling implementations and utility functions. *)

val is_ocaml_classic : string -> bool
(** Attempt to identify symbol as OCaml classic name mangling. *)

val demangle_classic_sym : string -> string option
(** Demangle a symbol using OCaml classic mangling. *)

val is_ocaml_classic_5_4 : string -> bool
(** Attempt to identify symbol as OCaml classic 5.4 name mangling. *)

val demangle_classic_5_4_sym : string -> string option
(** Demangle a symbol using OCaml 5.4 mangling. *)

val demangle_run_length : string -> string option
(** Demangle a symbol using Run Length encoding. *)

val is_ocaml_runlength : string -> bool
(** Attempt to identify symbol as OCaml run-length name mangling. *)

val unescape : string -> string
(** Percent unescape string. *)
