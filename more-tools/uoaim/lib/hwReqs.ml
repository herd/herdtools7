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

module Log = (val Logs.src_log (Logs.Src.create "hwreqs") : Logs.LOG)

module Preprocess = struct
  let normalize_effs =
    let regexp = Re.(compile (seq [ char '('; group alpha; char ')' ])) in
    fun str ->
      let str, _ =
        Util.replace_all regexp
          (fun (next, m) re_match ->
            let c = Re.Group.get re_match 1 in
            let n, next, m =
              match StringMap.find_opt c m with
              | None -> (next, next + 1, StringMap.add c next m)
              | Some n -> (n, next, m)
            in
            (Format.sprintf "E%d" n, (next, m)))
          (1, StringMap.empty) str
      in
      str

  let effect_re = Re.(seq [ char 'E'; digit ])

  let extract_eff_constraints : string -> string * string list =
    let regexp =
      let effect_word = Re.seq [ Re.rg 'A' 'Z'; Re.rep Re.alpha ] in
      let words =
        Re.seq [ effect_word; Re.rep (Re.seq [ Re.char ' '; effect_word ]) ]
      in
      Re.compile
        Re.(seq [ str "the "; group words; str " Effect "; group effect_re ])
    in
    Util.replace_all regexp
      (fun l re_match ->
        let eff_type = Re.Group.get re_match 1 in
        let eff_name = Re.Group.get re_match 2 in
        (eff_name, Format.sprintf "%s is a %s Effect" eff_name eff_type :: l))
      []

  let replace_mere_effs : string -> string =
    let regexp = Re.compile Re.(seq [ str "Effect "; group effect_re ]) in
    fun str ->
      Util.replace_all regexp
        (fun _ re_match ->
          let eff_name = Re.Group.get re_match 1 in
          (eff_name, ()))
        () str
      |> fst

  let preprocess_effs s : string * string list =
    let s, eff_phrases = extract_eff_constraints s in
    let s = replace_mere_effs s in
    (s, eff_phrases)
end

module EffSet = Set.Make (Eff)
module P = Parser.HwReq

module MakeInterpreter (NP : NameParser.S) = struct
  module C = Constraint.MakeInterpreter (NP)

  let top ~irrefl ?(name = None) str =
    let str = Preprocess.normalize_effs str in
    let parsed = P.parse str in
    Log.debug (fun m -> m "Parsed:@.  @[<v 0>%a@]" P.pp parsed);
    let endpoints = Eff.detect parsed.P.post |> Misc.List.uniq ~eq:Eff.equal in
    let stru = Structure.And [ parsed.P.pre; Structure.constr parsed.P.post ] in
    let eff_constrs, stru =
      Structure.fold_map
        (fun eff_constrs c ->
          let c, new_constrs = Preprocess.preprocess_effs c in
          (eff_constrs @ new_constrs, c))
        [] stru
    in
    let stru = Structure.And [ stru; Structure.of_conj_list eff_constrs ] in
    Log.debug (fun m ->
        m "Preprocessed structure:@.  @[<v 0>%a@]"
          (Structure.pp Format.pp_print_string)
          stru);
    let stru = C.interpret_structure stru in
    Log.debug (fun m ->
        m "Interpreted structure:@.  @[<v 0>%a@]"
          (Structure.pp Constraint.pp)
          stru);
    let e1, e2 =
      match endpoints with
      | [ e ] -> (e, e)
      | e :: _ when irrefl -> (e, e)
      | [ e1; e2 ] -> (e1, e2)
      | _ ->
          raise (Misc.Fatal "Cannot determine hardware requirement endpoints")
    in
    (* Format.printf "Detected endpoints in post %S: %a@." post (Util.pp_list_semicolon Eff.pp) endpoints; *)
    let exps = Reconstruction.exp_of_structure ~src_eff:e1 ~tgt_eff:e2 stru in
    let exps =
      exps
      |> List.map (fun exp ->
          let should_invert = Cat.RelExp.inverted_idents exp |> List.mem "po" in
          if should_invert then Cat.RelExp.invert exp else exp)
    in
    let test_type = if irrefl then "irreflexive" else "empty" in
    let name_str =
      match name with None -> "" | Some s -> Format.sprintf " as %s" s
    in
    exps
    |> List.map (fun exp ->
        Format.asprintf "%s %a%s" test_type Cat.RelExp.pp exp name_str)
end
