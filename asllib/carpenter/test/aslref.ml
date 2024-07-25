(****************************************************************************************************************)
(*  SPDX-FileCopyrightText: Copyright 2022-2024 Arm Limited and/or its affiliates <open-source-office@arm.com>  *)
(*  SPDX-License-Identifier: BSD-3-Clause                                                                       *)
(****************************************************************************************************************)

let check_no_strange_error ast =
  let open Asllib in
  try
    let _ = Native.interprete ~instrumentation:false ast in
    true
  with
  | Error.ASLException _ -> true
  | _ -> false

let no_strange_error ast =
  let open Asllib in
  try
    let _ = Native.interprete ~instrumentation:false ast in
    ()
  with
  | Error.ASLException _ -> ()
  | e ->
      Format.eprintf "@[<v 2>Found an error with:@ %a@]@." PP.pp_t ast;
      raise e

module C = struct
  module Syntax = struct
    include Carpenter_lib.CConfig.Stable

    let s_while = false
    let s_repeat = false
  end
end

module RandomTypedAST = Carpenter_lib.RandomAST.Typed (C)
module EnumAST = Carpenter_lib.ASTEnums.Make (C)

let no_unsupported_error_random =
  QCheck2.Test.make ~count:1000 ~name:"aslref doesn't raise strange errors"
    ~print:Asllib.PP.t_to_string
    (QCheck2.Gen.sized RandomTypedAST.ast)
    check_no_strange_error

let () =
  if false then
    QCheck_runner.run_tests ~long:true [ no_unsupported_error_random ] |> exit

let no_unsupported_error_enum () =
  let count = Z.of_int 10000 in
  let module IFSeq = Feat.IFSeq in
  let rec loop n count acc =
    let seq = EnumAST.asts n in
    let len = IFSeq.length seq in
    if Z.leq len count then
      loop (succ n) Z.(sub count len) (IFSeq.to_seq seq acc)
    else IFSeq.sample (Z.to_int count) seq acc
  in
  let alls = loop 0 count Seq.empty in
  Seq.iter no_strange_error alls

let () = no_unsupported_error_enum ()
