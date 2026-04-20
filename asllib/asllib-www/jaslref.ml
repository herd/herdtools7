(* asllib-www/jaslref.ml *)

open Asllib
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

  let argv : Runner.args =
    Runner.
      {
        default_args with
        exec = _exec;
        files = [ (NormalV1, "web-input.asl") ];
      }
  in

  let () =
    try Js_of_ocaml.Sys_js.create_file ~name:"web-input.asl" ~content:_code
    with Sys_error _ ->
      Js_of_ocaml.Sys_js.update_file ~name:"web-input.asl" ~content:_code
  in

  try Runner.run_with argv with
  | Asllib.Error.ASLException _ ->
      (* Unreachable due to the use of [or_exit] in Runner.ml:
         any [ASLException] is intercepted and re-raised as an [Exit]. *)
      assert false
  | Runner.Exit code ->
      if not _exec then Printf.printf "ASL: parse/type-check completed.\n%!"
      else
        Printf.printf "ASL: interpretation completed (exit code %n).\n%!" code

(* Expose to JavaScript *)
let () = Js.Unsafe.global##.runAsl := Js.wrap_callback run_asl
