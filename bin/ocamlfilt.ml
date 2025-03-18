(** Format of symbols to demangle. *)
type demangle_format =
  | Classic (** Classic OCaml 4.14 onwards mangling *)
  | Classic_5_4 (** Classic OCaml 5.4 mangling *)
  | CPlusPlus (** Classic OCaml plus C++ GNU/Itanium style for anonymous functions *)
  | RunLength (** New style run length encoded OCaml identifiers *)

let main style symbols =
  let style = Option.value ~default:Classic style in
  match symbols with
  | [] ->
     print_endline "Try reading from stdin"; exit 2
  | symbols when style == Classic ->
     List.iter (fun s ->
              Ocamlfilt.Demangle.demangle_classic_sym s
              |> function | None -> () | Some s -> print_endline s) symbols
  | symbols when style == Classic_5_4 ->
     List.iter (fun s ->
              Ocamlfilt.Demangle.demangle_classic_5_4_sym s
              |> function | None -> () | Some s -> print_endline s) symbols
  | _otherwise -> print_endline "Unimplemented"; exit 2

open Cmdliner

let demangle_format =
  let doc = "Mangling scheme to use. $(docv) specifies what mangling format to use.
Valid values are:
$(b,classic) for classic OCaml,
$(b,classic_5_4) for OCaml 5.4 style,
$(b,c++) for C++ GNU/Itanium style, and
$(b,run-length) for new style run length encoded." in
  let demangle_format = Arg.enum
    [ "classic", Classic;
      "classic_5_4", Classic_5_4;
      "c++", CPlusPlus;
      "run-length", RunLength ] in
  Arg.(value & opt (some demangle_format) ~vopt:(Some Classic) None & info ["format"] ~docv:"FORMAT" ~doc)


let symbols_term =
  let info = Arg.info []
               ~docv:"NAME"
               ~doc:"The list of names to demangle."
  in
  Arg.value (Arg.pos_all Arg.string [] info)

let cmd_t = Term.(const main $ demangle_format $ symbols_term )

let cmd =
  let doc = "Demangles names generated by the OCaml compiler" in
  let info = Cmd.info "ocamlfilt" ~doc in
  Cmd.v info cmd_t

let () =
  exit (Cmd.eval cmd)
