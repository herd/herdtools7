(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module StringScalar = struct
  type t = string
  let zero = "0" and one = "1"

  let of_string s = s
  let compare = String.compare
  let to_int k =  Warn.fatal "translate parsed constant '%s' to int" k
  let of_int i = Printf.sprintf "%i" i
  let pp _ s = s

  let op1 name  _ = Warn.fatal "unary operation '%s' on parsed constant" name
  let op2 name _ _ = Warn.fatal "binary operation '%s' on parsed constant" name

  let add =  op2 "add"
  let sub =  op2 "sub"
  let mul =  op2 "mul"
  let div =  op2 "div"
  let logor = op2 "logor"
  let logand = op2 "logand"
  let logxor = op2 "logxor"
  let lognot = op1 "lognot"
  let shift_left _ _ =  Warn.fatal "shift on parsed constant"
  let lt = op2 "(<)"
  let le = op2 "(<=)"
end

include SymbConstant.Make(StringScalar)
