(* asllib-www/jaslref.ml *)

open Aslref
open Js_of_ocaml

let dbg = false

(* Redirect OCaml’s stdout/stderr into the web page *)
let install_channel_flushers () =
  let out s =
    Js.Unsafe.fun_call
      (Js.Unsafe.pure_js_expr "asl_output")
      [| Js.Unsafe.inject (Js.string s) |]
  in
  Sys_js.set_channel_flusher stdout out;
  if not dbg then
    let err s =
      Js.Unsafe.fun_call
        (Js.Unsafe.pure_js_expr "asl_stderr")
        [| Js.Unsafe.inject (Js.string s) |]
    in
    Sys_js.set_channel_flusher stderr err

(* The single entry point visible from JavaScript                 *)
(*   – code   : string that contains the ASL program              *)
(*   – version: "v0" | "v1" etc.                                  *)
(*   – exec   : boolean, run or just parse/type-check             *)
let run_asl code version exec =
  install_channel_flushers ();

  let _code = Js.to_string code
  and _version = Js.to_string version
  and _exec = Js.to_bool exec in

  let argv : args =
    {
      exec = _exec;
      files =
        [
          ( (if String.equal _version "v0" then NormalV0 else NormalV1),
            "web-input.asl" );
        ];
      opn = None;
      (* … rest of the boolean flags left to their defaults … *)
      print_ast = false;
      print_lisp = false;
      print_serialized = false;
      print_typed = false;
      show_rules = false;
      strictness = (if String.equal _version "v0" then Silence else TypeCheck);
      output_format = Asllib.Error.HumanReadable;
      use_field_getter_extension = false;
      use_fine_grained_side_effects = false;
      use_conflicting_side_effects_extension = false;
      override_mode = Permissive;
      no_primitives = false;
      no_stdlib = false;
      no_stdlib0 = false;
      v0_use_split_chunks = false;
    }
  in

  let () =
    try Js_of_ocaml.Sys_js.create_file ~name:"web-input.asl" ~content:_code
    with Sys_error _ ->
      Js_of_ocaml.Sys_js.update_file ~name:"web-input.asl" ~content:_code
  in

  try
    run_with argv;
    if not _exec then Printf.printf "ASL: type-check completed.\n%!"
  with
  | Asllib.Error.ASLException e ->
      Printf.eprintf "%s\n%!" (Asllib.Error.error_to_string e)
  | Exit _ -> ()

(* Expose to JavaScript *)
let () = Js.Unsafe.global##.runAsl := Js.wrap_callback run_asl
