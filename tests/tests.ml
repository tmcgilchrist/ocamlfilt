let demangle_classic () =
  let examples = [
    (* OCaml 4.14 identifiers *)
    "camlDune__exe__Ocamlfilt__main_272", "Dune.exe.Ocamlfilt.main_272";
    "camlStdlib__Bytes__$2b$2b_313", "Stdlib.Bytes.++_313";

    (* Regular function names *)
    "camlStdlib.float_of_string_opt_193", "Stdlib.float_of_string_opt_193";
    "camlStdlib__Sys.set_signal_323", "Stdlib.Sys.set_signal_323";
    "camlCamlinternalLazy.force_gen_lazy_block_370", "CamlinternalLazy.force_gen_lazy_block_370";
    "camlPtrace.ptrace_request_op_value_468", "Ptrace.ptrace_request_op_value_468";

    (* Function names requiring escaping *)
    "camlStdlib.$40_dps_863", "Stdlib.@_dps_863";
    "camlStdlib.$40_196", "Stdlib.@_196";
    "camlCmdliner_base.some$27_988","Cmdliner_base.some'_988";

    (* Extended indexing operators *)
    "camlOcamlfilt__Demangle..$25$5b$3b$2e$2e$5d_289", "Ocamlfilt.Demangle..%[;..]_289";
    "camlOcamlfilt__Demangle..$25$7b$3b$2e$2e$7d_412", "Ocamlfilt.Demangle..%{;..}_412";
    "camlOcamlfilt__Demangle..$25$28$3b$2e$2e$29_413", "Ocamlfilt.Demangle..%(;..)_413";
    "camlOcamlfilt__Demangle..$25$7b$7d$3c$2d_388", "Ocamlfilt.Demangle..%{}<-_388";
    "camlOcamlfilt__Demangle..$25$5b$5d$3c$2d_289", "Ocamlfilt.Demangle..%[]<-_289";
    "camlOcamlfilt__Demangle..$25$5b$5d$3c$2d_288", "Ocamlfilt.Demangle..%[]<-_288";
    "camlOcamlfilt__Demangle..$25$5b$5d_386", "Ocamlfilt.Demangle..%[]_386" ] in
  List.iter (fun (input, expected) ->
    Alcotest.(check string) "demangle classic" expected
      (Ocamlfilt.Demangle.demangle_classic_sym input |> Option.get)) examples

let identify_classic_encodings () =
  let examples = [
    (* TODO OCaml 4.14 identifiers *)
    (* "camlDune__exe__Ocamlfilt__main_272", true; *)
    (* "camlStdlib__Bytes__$2b$2b_313", true; *)

    (* Regular function names *)
    "camlStdlib.float_of_string_opt_193", true;
    "camlStdlib__Sys.set_signal_323", true;

    (* Function names requiring escaping *)
    "camlStdlib.$40_dps_863", true;
    "camlStdlib.$40_196", true;
    "camlCmdliner_base.some$27_988", true;

    (* Extended indexing operators *)
    "camlDemangle..$25$7b$3b..$7d$3c$2d_394", true;
    "camlOcamlfilt__Demangle..$25$5b$5d$3c$2d_288", true;
    "camlOcamlfilt__Demangle..$25$5b$5d_386", true;

    (* OCaml 5.4 identifiers *)
    "camlStdlib$$$40_dps_865", false;
    "camlStdlib$float_of_string_opt_193", false;
    "camlStdlib__Bytes$$$2b$$2b_317", false; ] in
  List.iter (fun (input, expected) ->
    Alcotest.(check bool) "identify run length encodings" expected
      (Ocamlfilt.Demangle.is_ocaml_classic input)) examples

let demangle_classic_5_4 () =
  let examples = [
    (* Regular function names *)
    "camlStdlib$float_of_string_opt_193", "Stdlib.float_of_string_opt_193";
    "camlStdlib__Sys$set_signal_323", "Stdlib.Sys.set_signal_323";
    "camlCamlinternalLazy$force_gen_lazy_block_370", "CamlinternalLazy.force_gen_lazy_block_370";

    (* Function names requiring escaping *)
    "camlCmdliner_base$some$27_988", "Cmdliner_base.some'_988";
    "camlStdlib$$$40_dps_865", "Stdlib.@_dps_865";
    "camlStdlib__Bytes$$$2b$$2b_317", "Stdlib.Bytes.++_317";

    (* Extended indexing operators *)
    "camlOcamlfilt__Demangle$$$2e$$25$$5b$$3b$$2e$$2e$$5d_289", "Ocamlfilt.Demangle..%[;..]_289";
    "camlOcamlfilt__Demangle$$$2e$$25$$7b$$3b$$2e$$2e$$7d_412", "Ocamlfilt.Demangle..%{;..}_412";
    "camlOcamlfilt__Demangle$$$2e$$25$$28$$3b$$2e$$2e$$29_413", "Ocamlfilt.Demangle..%(;..)_413";
    "camlOcamlfilt__Demangle$$$2e$$25$$7b$$7d$$3c$$2d_388", "Ocamlfilt.Demangle..%{}<-_388";
    "camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d$$3c$$2d_289", "Ocamlfilt.Demangle..%[]<-_289";
    "camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d_387", "Ocamlfilt.Demangle..%[]_387";
    "camlStdlib$$$40_196", "Stdlib.@_196" ] in
  List.iter (fun (input, expected) ->
    Alcotest.(check string) "demangle classic" expected
      (Ocamlfilt.Demangle.demangle_classic_5_4_sym input |> Option.get)) examples

let identify_classic_5_4_encodings () =
  let examples = [
    (* Regular function names *)
    "camlStdlib$float_of_string_opt_193", true;
    "camlStdlib__Sys$set_signal_323", true;
    "camlCamlinternalLazy$force_gen_lazy_block_370", true;

    (* Function names requiring escaping *)
    "camlStdlib$$$40_dps_865", true;
    "camlCmdliner_base$some$27_988", true;
    "camlStdlib$$$40_196", true;

    (* Extended indexing operators *)
    "camlStdlib__Bytes$$$2b$$2b_317", true;
    "camlOcamlfilt__Demangle$$$2e$$25$$5b$$3b$$2e$$2e$$5d_289", true;
    "camlOcamlfilt__Demangle$$$2e$$25$$7b$$3b$$2e$$2e$$7d_412", true;
    "camlOcamlfilt__Demangle$$$2e$$25$$28$$3b$$2e$$2e$$29_413", true;
    "camlOcamlfilt__Demangle$$$2e$$25$$7b$$7d$$3c$$2d_388", true;
    "camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d$$3c$$2d_289", true;
    "camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d_387", true;

    (* OCaml 5.1 identifiers *)
    "camlStdlib.float_of_string_opt_193", false;
    "camlStdlib.$40_dps_865", false;
    "camlStdlib__Bytes.$2b$2b_317", false;

    (* TODO OCaml 4.14 identifiers *)
    "camlDune__exe__Ocamlfilt__main_272", false;
    "camlStdlib__Bytes__$2b$2b_313", false; ] in
  List.iter (fun (input, expected) ->
    Alcotest.(check bool) "identify run length encodings" expected
      (Ocamlfilt.Demangle.is_ocaml_classic_5_4 input)) examples

let () =
  let open Alcotest in
  run "OCaml" [
    "identify",
      [ test_case "identify::classic" `Quick identify_classic_encodings;
        test_case "identify::classic 5.4" `Quick identify_classic_5_4_encodings ];
    "demangling",
      [ test_case "demangling::classic" `Quick demangle_classic;
        test_case "demangling::classic 5.4" `Quick demangle_classic_5_4;
      ]
    ]

