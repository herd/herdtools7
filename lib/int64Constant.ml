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

module Int64Scalar = struct
  include Int64

  let shift_right_arithmetic = Int64.shift_right

  let addk x k = match k with
  | 0 -> x
  | 1 -> succ x
  | _ -> add x (of_int k)

  let machsize = MachSize.Quad
  let pp hexa v =
    Printf.sprintf (if hexa then "0x%Lx" else "%Li") v
  let lt v1 v2 = compare v1 v2 < 0
  let le v1 v2 = compare v1 v2 <= 0
  let bit_at k v = Int64.logand v (Int64.shift_left Int64.one k)
  let mask sz =
    let open MachSize in
    match sz with
    | Byte -> fun v -> logand v 0xffL
    | Short -> fun v -> logand v 0xffffL
    | Word -> fun v ->  logand v 0xffffffffL
    | Quad -> fun v -> v
end

include SymbConstant.Make(Int64Scalar)
