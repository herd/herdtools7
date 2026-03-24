(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Log = (val Logs.src_log (Logs.Src.create "definition") : Logs.LOG)
module P = Parser.Definition

module MakeInterpreter (NP : NameParser.S) = struct
  module C = Constraint.MakeInterpreter (NP)

  let top ~(infer_rec : bool) ?(name = None) str : string list =
    let def = P.parse str in
    Log.debug (fun m ->
        m "Parsed structure:@.%a"
          (Structure.pp Format.pp_print_string)
          def.P.structure);
    match def.P.endpoints with
    | [ e1; e2 ] ->
        let name =
          match name with
          | Some s -> s
          | None -> (
              let pre_rel = C.interpret_rel_exp e1 e2 def.P.head_phrase in
              match pre_rel with
              | None -> "???"
              | Some exp -> (
                  match Cat.RelExp.as_ident exp with
                  | Some nm -> nm
                  | None ->
                      let msg = "invalid head phrase" in
                      let context = def.P.head_phrase in
                      raise (Util.Interpret_error { msg; context })))
        in
        let post_stru = C.interpret_structure def.P.structure in
        Log.debug (fun m ->
            m "Interpreted structure:@.%a"
              (Structure.pp Constraint.pp)
              post_stru);
        let exps =
          Reconstruction.exp_of_structure ~src_eff:e1 ~tgt_eff:e2 post_stru
        in
        let binding =
          let has_self_ref () =
            exps |> Misc.List.concat_map Cat.RelExp.identifiers |> List.mem name
          in
          if infer_rec && has_self_ref () then "let rec" else "let"
        in
        let results =
          List.map
            (fun e -> Format.asprintf "%s %s = %a" binding name Cat.RelExp.pp e)
            exps
        in
        results
    | _ ->
        let msg =
          "definition preamble must mention exactly two effects, with the \
           phrasing \"an Effect En\" for natural number `n`."
        in
        let context = def.P.head_phrase in
        raise (Util.Interpret_error { msg; context })
end
