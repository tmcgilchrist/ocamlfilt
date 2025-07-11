(* QuickCheck generators *)

open Ocamlfilt

let demangle_classic () =
  let examples =
    [
      (* OCaml 4.14 identifiers *)
      ("camlDune__exe__Ocamlfilt__main_272", "Dune.exe.Ocamlfilt.main_272");
      ("camlStdlib__Bytes__$2b$2b_313", "Stdlib.Bytes.++_313");
      (* Regular function names *)
      ("camlStdlib.float_of_string_opt_193", "Stdlib.float_of_string_opt_193");
      ("camlStdlib__Sys.set_signal_323", "Stdlib.Sys.set_signal_323");
      ( "camlCamlinternalLazy.force_gen_lazy_block_370",
        "CamlinternalLazy.force_gen_lazy_block_370" );
      ( "camlPtrace.ptrace_request_op_value_468",
        "Ptrace.ptrace_request_op_value_468" );
      (* Function names requiring escaping *)
      ("camlStdlib.$40_dps_863", "Stdlib.@_dps_863");
      ("camlStdlib.$40_196", "Stdlib.@_196");
      ("camlCmdliner_base.some$27_988", "Cmdliner_base.some'_988");
      (* Extended indexing operators *)
      ( "camlOcamlfilt__Demangle..$25$5b$3b$2e$2e$5d_289",
        "Ocamlfilt.Demangle..%[;..]_289" );
      ( "camlOcamlfilt__Demangle..$25$7b$3b$2e$2e$7d_412",
        "Ocamlfilt.Demangle..%{;..}_412" );
      ( "camlOcamlfilt__Demangle..$25$28$3b$2e$2e$29_413",
        "Ocamlfilt.Demangle..%(;..)_413" );
      ("camlOcamlfilt__Demangle..$25$7b$7d_388", "Ocamlfilt.Demangle..%{}_388");
      ( "camlOcamlfilt__Demangle..$25$7b$7d$3c$2d_388",
        "Ocamlfilt.Demangle..%{}<-_388" );
      ( "camlOcamlfilt__Demangle..$25$5b$5d$3c$2d_289",
        "Ocamlfilt.Demangle..%[]<-_289" );
      ( "camlOcamlfilt__Demangle..$25$5b$5d$3c$2d_288",
        "Ocamlfilt.Demangle..%[]<-_288" );
      ("camlOcamlfilt__Demangle..$25$5b$5d_386", "Ocamlfilt.Demangle..%[]_386");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        "demangle classic" expected
        (Demangle.demangle_classic_sym input |> Option.get))
    examples

let identify_classic_encodings () =
  let examples =
    [
      (* TODO OCaml 4.14 identifiers *)
      (* "camlDune__exe__Ocamlfilt__main_272", true; *)
      (* "camlStdlib__Bytes__$2b$2b_313", true; *)

      (* Regular function names *)
      ("camlStdlib.float_of_string_opt_193", true);
      ("camlStdlib__Sys.set_signal_323", true);
      (* Function names requiring escaping *)
      ("camlStdlib.$40_dps_863", true);
      ("camlStdlib.$40_196", true);
      ("camlCmdliner_base.some$27_988", true);
      (* Extended indexing operators *)
      ("camlDemangle..$25$7b$3b..$7d$3c$2d_394", true);
      ("camlOcamlfilt__Demangle..$25$5b$5d$3c$2d_288", true);
      ("camlOcamlfilt__Demangle..$25$5b$5d_386", true);
      (* OCaml 5.4 identifiers *)
      ("camlStdlib$$$40_dps_865", false);
      ("camlStdlib$float_of_string_opt_193", false);
      ("camlStdlib__Bytes$$$2b$$2b_317", false);
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check bool)
        "identify run length encodings" expected
        (Demangle.is_ocaml_classic input))
    examples

let demangle_classic_5_4 () =
  let examples =
    [
      (* Regular function names *)
      ("camlStdlib$float_of_string_opt_193", "Stdlib.float_of_string_opt_193");
      ("camlStdlib__Sys$set_signal_323", "Stdlib.Sys.set_signal_323");
      ( "camlCamlinternalLazy$force_gen_lazy_block_370",
        "CamlinternalLazy.force_gen_lazy_block_370" );
      (* Function names requiring escaping *)
      ("camlCmdliner_base$some$27_988", "Cmdliner_base.some'_988");
      ("camlStdlib$$$40_dps_865", "Stdlib.@_dps_865");
      ("camlStdlib__Bytes$$$2b$$2b_317", "Stdlib.Bytes.++_317");
      (* Extended indexing operators *)
      ( "camlOcamlfilt__Demangle$$$2e$$25$$5b$$3b$$2e$$2e$$5d_289",
        "Ocamlfilt.Demangle..%[;..]_289" );
      ( "camlOcamlfilt__Demangle$$$2e$$25$$7b$$3b$$2e$$2e$$7d_412",
        "Ocamlfilt.Demangle..%{;..}_412" );
      ( "camlOcamlfilt__Demangle$$$2e$$25$$28$$3b$$2e$$2e$$29_413",
        "Ocamlfilt.Demangle..%(;..)_413" );
      ( "camlOcamlfilt__Demangle$$$2e$$25$$7b$$7d$$3c$$2d_388",
        "Ocamlfilt.Demangle..%{}<-_388" );
      ( "camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d$$3c$$2d_289",
        "Ocamlfilt.Demangle..%[]<-_289" );
      ( "camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d_387",
        "Ocamlfilt.Demangle..%[]_387" );
      ("camlStdlib$$$40_196", "Stdlib.@_196");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        "demangle classic" expected
        (Demangle.demangle_classic_5_4_sym input |> Option.get))
    examples

let identify_classic_5_4_encodings () =
  let examples =
    [
      (* Regular function names *)
      ("camlStdlib$float_of_string_opt_193", true);
      ("camlStdlib__Sys$set_signal_323", true);
      ("camlCamlinternalLazy$force_gen_lazy_block_370", true);
      (* Function names requiring escaping *)
      ("camlStdlib$$$40_dps_865", true);
      ("camlCmdliner_base$some$27_988", true);
      ("camlStdlib$$$40_196", true);
      (* Extended indexing operators *)
      ("camlStdlib__Bytes$$$2b$$2b_317", true);
      ("camlOcamlfilt__Demangle$$$2e$$25$$5b$$3b$$2e$$2e$$5d_289", true);
      ("camlOcamlfilt__Demangle$$$2e$$25$$7b$$3b$$2e$$2e$$7d_412", true);
      ("camlOcamlfilt__Demangle$$$2e$$25$$28$$3b$$2e$$2e$$29_413", true);
      ("camlOcamlfilt__Demangle$$$2e$$25$$7b$$7d$$3c$$2d_388", true);
      ("camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d$$3c$$2d_289", true);
      ("camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d_387", true);
      (* OCaml 5.1 identifiers *)
      ("camlStdlib.float_of_string_opt_193", false);
      ("camlStdlib.$40_dps_865", false);
      ("camlStdlib__Bytes.$2b$2b_317", false);
      (* TODO OCaml 4.14 identifiers *)
      ("camlDune__exe__Ocamlfilt__main_272", false);
      ("camlStdlib__Bytes__$2b$2b_313", false);
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check bool)
        "identify run length encodings" expected
        (Demangle.is_ocaml_classic_5_4 input))
    examples

(** Replace `-` with `_` *)
let replace from to_ str =
  let buf = Bytes.of_string str in
  for i = 0 to Bytes.length buf - 1 do
    let a = Bytes.get buf i in
    if Char.equal from a then Bytes.set buf i to_
  done;
  Bytes.to_string buf

let test_encode_two _ =
  let examples =
    [
      ("Gödel", "Gdel_5qa");
      ("føø", "f_5gaa");
      ("ρυστ", "2xaedc");
      ("académie_française", "acadmie_franaise_npb1a");
      ("ÆÇlass", "lass_9jag");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        "punycode encoding" expected
        ( Punycode.encode input |> Result.get_ok |> fun s ->
          replace '-' '_' s ))
    examples

let string =
  let pp_string ppf x = Fmt.pf ppf "%S" x in
  Alcotest.testable pp_string String.equal

let test_decode_two _ =
  let examples =
    [
      ("Gdel-5qa", "Gödel");
      ("f_5gaa", "føø");
      ("acadmie_franaise_npb1a", "académie_française");
      ("lass-9jag", "ÆÇlass");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        "punycode decoding" expected
        ( replace '_' '-' input |> Punycode.decode |> Result.get_ok
          |> Punycode.ImmutArray.to_utf8
        |> fun s -> replace '-' '_' s ))
    examples

(* Escaped string for "Gödel" *)
let str = "\u{0047}\u{00F6}\u{0064}\u{0065}\u{006C}"

let demangle_run_length () =
  let examples =
    [
      (* Regular function names *)
      ("_OM6Stdlib23float_of_string_opt_193", "Stdlib.float_of_string_opt_193");
      ("_OM6Stdlib3Sys14set_signal_323", "Stdlib.Sys.set_signal_323");
      ( "_OM16CamlinternalLazy24force_gen_lazy_block_370",
        "CamlinternalLazy.force_gen_lazy_block_370" );

      (* Function names requiring escaping *)
      "_OM13Cmdliner_base11some$27_988", "Cmdliner_base.some'_988";
      "_OM6Stdlib11$40_dps_865", "Stdlib.@_dps_865";
      "_OM6Stdlib5Bytes10$2b$2b_317", "Stdlib.Bytes.++_317";

      (* Extended indexing operators *)
      "_OM9Ocamlfilt8Demangle25$2e$25$5b$3b$2e$2e$5d_289", "Ocamlfilt.Demangle..%[;..]_289";
      "_OM9Ocamlfilt8Demangle25$2e$25$7b$3b$2e$2e$7d_412", "Ocamlfilt.Demangle..%{;..}_412";
      "_OM9Ocamlfilt8Demangle25$2e$25$28$3b$2e$2e$29_413", "Ocamlfilt.Demangle..%(;..)_413";
      "_OM9Ocamlfilt8Demangle22$2e$25$7b$7d$3c$2d_388", "Ocamlfilt.Demangle..%{}<-_388";
      "_OM9Ocamlfilt8Demangle22$2e$25$5b$5d$3c$2d_289", "Ocamlfilt.Demangle..%[]<-_289";
      "_OM9Ocamlfilt8Demangle16$2e$25$5b$5d_387", "Ocamlfilt.Demangle..%[]_387";
      "_OM6Stdlib7$40_196", "Stdlib.@_196";

      (* Closure function names *)
      (* Rust
         _RNCNvCsgStHSCytQ6I_7mycrate4main0B3_
         => mycrate::main::{closure#0} *)
      (* C++ clang on macOS
         __ZZ7abssortPfjENK3$_0clEff =>
         abssort(float*, unsigned int)::$_0::operator()(float, float) const
       *)
      (* OCaml
          _OC1M1x8_4_10_456 =>
          "prefix" + M + named function x + closure at line 4 column 10
       *)
      "_OC3Foo8_2_4_456", "Foo.<fun_2_4_456>";
      "_OC1M1x9_4_10_456", "M.x.<fun_4_10_456>";
      "_OC3Foo9_13_4_346", "Foo.<fun_13_4_346>"
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        "demangle run-length" expected
        (Demangle.demangle_run_length input |> Option.get))
    examples

let identify_run_length () =
  let examples =
    [
      (* Regular function names *)
      ("_OM6Stdlib23float_of_string_opt_193", true);
      ("_OM6Stdlib3Sys14_set_signal_323", true);
      ("_OM16CamlinternalLazy24force_gen_lazy_block_370", true);

      (* Closure function names *)
      ("_OC3Foo7_2_4_456", true);
      ("_OC1M1x7_4_10_456", true);

      (* Function names requiring escaping *)
      (* "camlCmdliner_base$some$27_988", "Cmdliner_base.some'_988"; *)
      (* "camlStdlib$$$40_dps_865", "Stdlib.@_dps_865"; *)
      (* "camlStdlib__Bytes$$$2b$$2b_317", "Stdlib.Bytes.++_317"; *)

      (* Extended indexing operators *)
      (* "camlOcamlfilt__Demangle$$$2e$$25$$5b$$3b$$2e$$2e$$5d_289", "Ocamlfilt.Demangle..%[;..]_289"; *)
      (* "camlOcamlfilt__Demangle$$$2e$$25$$7b$$3b$$2e$$2e$$7d_412", "Ocamlfilt.Demangle..%{;..}_412"; *)
      (* "camlOcamlfilt__Demangle$$$2e$$25$$28$$3b$$2e$$2e$$29_413", "Ocamlfilt.Demangle..%(;..)_413"; *)
      (* "camlOcamlfilt__Demangle$$$2e$$25$$7b$$7d$$3c$$2d_388", "Ocamlfilt.Demangle..%{}<-_388"; *)
      (* "camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d$$3c$$2d_289", "Ocamlfilt.Demangle..%[]<-_289"; *)
      (* "camlOcamlfilt__Demangle$$$2e$$25$$5b$$5d_387", "Ocamlfilt.Demangle..%[]_387"; *)
      (* "camlStdlib$$$40_196", "Stdlib.@_196"  *)

      (* Regular function names *)
      ("camlStdlib.float_of_string_opt_193", false);
      ("camlStdlib__Sys.set_signal_323", false);
      (* Function names requiring escaping *)
      ("camlStdlib.$40_dps_863", false);
      ("camlStdlib.$40_196", false);
      ("camlCmdliner_base.some$27_988", false);
      (* Extended indexing operators *)
      ("camlDemangle..$25$7b$3b..$7d$3c$2d_394", false);
      ("camlOcamlfilt__Demangle..$25$5b$5d$3c$2d_288", false);
      ("camlOcamlfilt__Demangle..$25$5b$5d_386", false);
      (* OCaml 5.4 identifiers *)
      ("camlStdlib$$$40_dps_865", false);
      ("camlStdlib$float_of_string_opt_193", false);
      ("camlStdlib__Bytes$$$2b$$2b_317", false);
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check bool)
        "demangle run-length" expected
        (Demangle.is_ocaml_runlength input))
    examples

let test_escape () =
  let examples =
    [
      ("some$27_988", "some'_988");
      ("$40_dps_865", "@_dps_865");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string) "unescape" expected (Demangle.unescape input))
    examples

let () =
  let open Alcotest in
  run "OCaml"
    [
      ( "identify",
        [
          test_case "identify::classic" `Quick identify_classic_encodings;
          test_case "identify::classic 5.4" `Quick
            identify_classic_5_4_encodings;
          test_case "identify::run-length" `Quick identify_run_length;
        ] );
      ( "demangling",
        [
          test_case "demangling::classic" `Quick demangle_classic;
          test_case "demangling::classic 5.4" `Quick demangle_classic_5_4;
          test_case "demangling:run-length" `Quick demangle_run_length;
        ] );
      ( "punycode_two",
        [
          test_case "punycode::encode" `Quick test_encode_two;
          test_case "punycode::decode" `Quick test_decode_two;
        ] );
      ("percent_escape", [ test_case "escape::decode" `Quick test_escape ]);
    ]
