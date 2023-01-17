(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let check_arch_model a m =
  match m with
  | Model.Generic (o,_,_) ->
      begin match o.ModelOption.arch with
      | None -> m
      | Some b ->
          if a = b then m
          else
            Warn.user_error
              "Architecture mismatch between test and model (%s vs. %s)"
              (Archs.pp a)  (Archs.pp b)
        end
  | m -> m

let parse archcheck arch libfind variant model =
  let m = match model with
  | None -> Model.get_default_model variant arch
  | Some m -> m in
  let m = match m with
  | Model.File fname ->
      let module P =
        ParseModel.Make
          (struct
            include LexUtils.Default
            let libfind = libfind
          end) in
      Model.Generic (P.parse fname)
  | _ -> m in
  if archcheck then check_arch_model arch m
  else m
