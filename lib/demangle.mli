(** Various OCaml de-mangling implementations and utility functions. *)

(** Attempt to identify symbol as OCaml classic name mangling. *)
val is_ocaml_classic : string -> bool

(** Demangle a symbol using OCaml classic mangling. *)
val demangle_classic_sym : string -> string option

(** Attempt to identify symbol as OCaml classic 5.4 name mangling. *)
val is_ocaml_classic_5_4 : string -> bool

(** Demangle a symbol using OCaml 5.4 mangling. *)
val demangle_classic_5_4_sym : string -> string option
