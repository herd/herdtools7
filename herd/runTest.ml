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

module type Config = sig
  val model : Model.t option
  val archcheck : bool
  val through : Model.through
  val strictskip : bool
  val cycles : StringSet.t
  val bell_model_info : (string * BellModel.info) option
  val macros : string option
  val check_name : string -> bool
  val check_rename : string -> string option
  val libfind : string -> string
  include GenParser.Config
  include Top_herd.CommonConfig
  include Sem.Config

  val statelessrc11 : bool
  val dumpallfaults : bool
  val byte : MachSize.Tag.t
  val sve_vector_length : int
  val sme_vector_length : int
end

module type Outcome = sig
  module M : XXXMem.S
  type t = Top_herd.TestResult.Make(M.S).t

  val test : M.S.test
  val result : t
end

type runfun =
  DirtyBit.t option ->
  filename:string option (* file name *) ->
  contents:string (* litmus test contents *) ->
  TestHash.env ->
  Splitter.result ->
  TestHash.env * (module Outcome) option

module Make
    (S:Sem.Semantics)
    (P:sig
      type pseudo
      val parse_string : string -> Splitter.result ->  pseudo MiscParser.t
    end with type pseudo = S.A.pseudo)
    (M:XXXMem.S with module S = S)
    (C:Config) =
  struct
    module T = Test_herd.Make(S.A)

    let run dirty ~filename ~contents env splitted =
      try
         let parsed = P.parse_string contents splitted in
        let name = splitted.Splitter.name in
        let hash = MiscParser.get_hash parsed in
        let env = match hash, filename with
          | Some hash, Some filename ->
              TestHash.check_env env name.Name.name filename hash
          | _ -> env
        in
        let test = T.build name parsed in
(* Compute basic machine size *)
        let sz = T.compute_size C.byte test in
(* And run test *)
        let module T =
          Top_herd.Make
            (struct
              include C
              let byte = sz
              let dirty = dirty
            end)(M) in
        let test, result = T.run test in
        let module R = struct
          module M = M
          type t = Top_herd.TestResult.Make(S).t
          let test = test
          let result = result
        end
        in
        env, Some (module R : Outcome)
      with TestHash.Seen -> env, None
  end
