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
end

type runfun =
  CacheType.t option ->
  DirtyBit.t option ->
  float (* start time *) ->
  string (* file name *) ->
  in_channel (* source channel *) ->
  TestHash.env ->
  Splitter.result ->
  TestHash.env

module Make
    (S:Sem.Semantics)
    (P:sig
      type pseudo
      val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
    end with type pseudo = S.A.pseudo)
    (M:XXXMem.S with module S = S)
    (C:Config) =
  struct
    module T = Test_herd.Make(S.A)
     let run cache_type dirty start_time filename chan env splitted =
      try
         let parsed = P.parse chan splitted in
        let name = splitted.Splitter.name in
        let hash = MiscParser.get_hash parsed in
        let env = match hash with
        | None -> env
        | Some hash ->
            TestHash.check_env env name.Name.name filename hash in
        let test = T.build name parsed in
(* Compute basic machine size *)
        let sz =
          if S.A.is_mixed then begin match C.byte with
          | MachSize.Tag.Size sz -> sz
          | MachSize.Tag.Auto ->
              let szs = test.Test_herd.access_size in
              match szs with
              | [] -> MachSize.Byte
              | [sz] -> MachSize.pred sz
              | sz::_ -> sz
          end else begin
            (* Cannot that easily check the test not to mix sizes,
               as there are several locations in test that may be of
               different sizes *)
            MachSize.Byte
          end in
(* And run test *)
        let module T =
          Top_herd.Make
            (struct
              include C
              let byte = sz
              let cache_type = cache_type
              let dirty = dirty
            end)(M) in
        T.run start_time test ;
        env
      with TestHash.Seen -> env
  end
