(****************************************************************************************************************)
(*  SPDX-FileCopyrightText: Copyright 2022-2024 Arm Limited and/or its affiliates <open-source-office@arm.com>  *)
(*  SPDX-License-Identifier: BSD-3-Clause                                                                       *)
(****************************************************************************************************************)

let _log_src =
  Logs.Src.create ~doc:"Comparaison between different interpreter results."
    "carpenter.comparator"

let set_log_level = Logs.Src.set_level _log_src
let info f = Logs.info ~src:_log_src f
let debug f = Logs.debug ~src:_log_src f

let get_ref_result ast =
  let open Asllib in
  try
    match Native.interprete ~instrumentation:false ast with
    | 0, _ -> Ok ()
    | i, _ -> Error ("Bad return code: " ^ string_of_int i)
  with
  | Error.ASLException e -> Error (Error.error_to_string e)
  | e ->
      let msg =
        Printf.sprintf "ASLRef failed with uncaught error: %s."
          (Printexc.to_string e)
      in
      failwith msg

let get_ref_result_instr =
  let open Asllib in
  let open Native in
  let module B = Instrumentation.SemanticsSingleSetBuffer in
  let module I = NativeInterpreter (Instrumentation.SemMake (B)) in
  fun ast ->
    let ast = List.rev_append Native.primitive_decls ast in
    B.reset ();
    let res =
      try
        match I.run ast with
        | NV_Literal (L_Int z) when Z.equal z Z.zero -> Ok ()
        | NV_Literal (L_Int z) -> Error ("Bad return code: " ^ Z.to_string z)
        | _ -> Error "Bad return code (not integer)."
      with
      | Error.ASLException e -> Error (Error.error_to_string e)
      | e ->
          let msg =
            Printf.sprintf "ASLRef failed with uncaught error: %s."
              (Printexc.to_string e)
          in
          Error msg
    in
    (res, B.get ())

let compare_results ~ref_result ~binterp_result =
  match (ref_result, binterp_result) with
  | Ok (), Ok () ->
      debug (fun m -> m "Executions successful --> probably no mismatch.");
      true
  | Error ref_s, Error binterp_s ->
      debug (fun m ->
          m "Error comparison between %S and %S. Considered true.\n%!" ref_s
            binterp_s);
      true (* TODO *)
  | _ ->
      debug (fun m -> m "Result mismatch --> probable discrepancy.");
      false

let compare_with_ref ast binterp_result =
  let ref_result = get_ref_result ast in
  compare_results ~ref_result ~binterp_result
