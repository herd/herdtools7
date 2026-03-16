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

module type Config = sig
  val includes : string list
  val libdir : string
  val cat : string
end

module FreeVars (O : Config) : sig
  val free_vars : string -> StringSet.t
  (** Get all free variables mentioned in the file within let(rec)-bound
      expressions. Follows included files recursively. *)
end = struct
  module ML = MyLib.Make (struct
    include O

    let env = None
    let debug = false
  end)

  module PM = ParseModel.Make (struct
    let debug = false
    let libfind = ML.find
  end)

  let rec file_free_vars (seen, fvs) (fname : string) =
    let fname = ML.find fname in
    if StringSet.mem fname seen then (seen, fvs)
    else
      let _, _, ast = PM.parse fname in
      List.fold_left ins_free_vars (seen, fvs) ast

  and ins_free_vars (seen, fvs) =
    let open AST in
    function
    | Let (_, bds) | Rec (_, bds, _) ->
        List.fold_left
          (fun (seen, fvs) -> function
            | _, Pvar (Some name), e ->
                let new_fvs = StringSet.add name (ASTUtils.free_body [] e) in
                (seen, StringSet.union fvs new_fvs)
            | _ -> (seen, fvs))
          (seen, fvs) bds
    | Include (_, fname) -> file_free_vars (seen, fvs) fname
    | _ -> (seen, fvs)

  let free_vars fname =
    let _, fvs = file_free_vars (StringSet.empty, StringSet.empty) fname in
    fvs
end

(* A miaou pair consists of a cat identifier and its corresponding LaTeX
   command name. *)
module MiaouPair = struct
  let compare (_, latex_name) (_, latex_name') =
    Int.compare (String.length latex_name') (String.length latex_name)

  let make cat_name = (cat_name, MiaouNames.to_csname cat_name)
end

module type S = sig
  val parse_names : string -> string list
end

module Make (O : Config) : S = struct
  module FV = FreeVars (O)
  module A = Angstrom

  let extra_pairs = [ ("IC-after", "ICa"); ("lrs", "lrs") ]

  let miaou_pairs =
    let cat_names = FV.free_vars O.cat in
    cat_names |> StringSet.elements |> List.map MiaouPair.make
    |> List.append extra_pairs
    |> List.sort MiaouPair.compare

  let miaou_tokens =
    miaou_pairs
    |> List.map
         A.(fun (cat_name, latex_name) -> string latex_name *> return cat_name)

  let name_token = Util.List.fold_left_ne A.( <|> ) miaou_tokens
  let name_sequence = A.many1 name_token

  let parse_names str =
    match A.parse_string ~consume:A.Consume.All name_sequence str with
    | Ok result -> result
    | Error _ -> []
end
