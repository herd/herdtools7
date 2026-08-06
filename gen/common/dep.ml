(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Dependency interfaces and implementations *)

module type S = sig
  type dp

  val equal_dp : dp -> dp -> bool
  val pp_dp : dp -> string
  val fold_dp : (dp -> 'a -> 'a) -> 'a -> 'a
  val fold_dpr : (dp -> 'a -> 'a) -> 'a -> 'a
  val fold_dpw : (dp -> 'a -> 'a) -> 'a -> 'a

  val ddr_default : dp option
  val ddw_default : dp option
  val ctrlr_default : dp option
  val ctrlw_default : dp option

  val is_ctrlr : dp -> bool
  val is_addr : dp -> bool
  val is_data : dp -> bool

  val fst_dp : dp -> dp list
  val sequence_dp : dp -> dp -> dp list
  val expand_dp_dir : dp -> Code.dir list
end

module No = struct
  type dp

  let equal_dp _ _ = assert false
  let pp_dp _ = assert false
  let fold_dp _f r = r
  let fold_dpr _f r = r
  let fold_dpw _f r = r

  let ddr_default = None
  let ddw_default = None
  let ctrlr_default = None
  let ctrlw_default = None

  let is_ctrlr _ = assert false
  let is_addr _ = assert false
  let is_data _ = assert false

  let fst_dp _ = assert false
  let sequence_dp _ _ = assert false
  let expand_dp_dir _ = assert false
end

module Basic = struct
  type dp = ADDR | DATA | CTRL

  let equal_dp dp1 dp2 = match dp1,dp2 with
    | ADDR,ADDR
    | DATA,DATA
    | CTRL,CTRL -> true
    | (ADDR|DATA|CTRL),_ -> false

  let pp_dp = function
    | ADDR -> "Addr"
    | DATA -> "Data"
    | CTRL -> "Ctrl"

  let fold_dp f r = f ADDR (f DATA (f CTRL r))
  let fold_dpr f r = f ADDR (f CTRL r)
  let fold_dpw = fold_dp

  let ddr_default = Some ADDR
  let ddw_default = Some DATA
  let ctrlr_default = Some CTRL
  let ctrlw_default = Some CTRL

  let is_ctrlr = function CTRL -> true | _ -> false
  let is_addr = function ADDR -> true | _ -> false
  let is_data = function DATA -> true | _ -> false

  let fst_dp = function
    | CTRL -> [CTRL]
    | ADDR|DATA -> []

  let sequence_dp d1 d2 = match d1 with
    | ADDR -> [d2]
    | DATA|CTRL -> []

  open Code
  let expand_dp_dir = function
    | CTRL | ADDR -> [R;W]
    | DATA -> [W]
end

module Bell = struct
  include Basic

  (* Bell dependencies are labels, not architectural address or read-control dependencies. *)
  let is_ctrlr _ = false
  let is_addr _ = false
end

module Full = struct
type dp = ADDR | DATA | CTRL | CTRLISYNC

let equal_dp dp1 dp2 = match dp1,dp2 with
  | ADDR,ADDR
  | DATA,DATA
  | CTRL,CTRL
  | CTRLISYNC,CTRLISYNC -> true
  | (ADDR|DATA|CTRL|CTRLISYNC),_ -> false

let pp_dp = function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"
  | CTRLISYNC -> "CtrlIsync"

let fold_dp f r =  f ADDR (f DATA (f CTRL (f CTRLISYNC r)))
let fold_dpr f r =  f ADDR (f CTRL (f CTRLISYNC r))
let fold_dpw = fold_dp

let ddr_default = Some ADDR
let ddw_default = Some DATA
let ctrlr_default = Some CTRLISYNC
let ctrlw_default = Some CTRL

let is_ctrlr = function
  | CTRLISYNC -> true
  | _ -> false

let is_addr = function
  | ADDR -> true
  | _ -> false

let is_data = function
  | DATA -> true
  | _ -> false

let fst_dp = function
  | CTRL -> [CTRL]
  | CTRLISYNC -> [CTRL;CTRLISYNC]
  | ADDR|DATA -> []

let sequence_dp d1 d2 = match d1 with
| ADDR -> [d2]
| DATA|CTRL|CTRLISYNC -> []

open Code
let expand_dp_dir = function
  | CTRL | CTRLISYNC | ADDR -> [R;W]
  | DATA -> [W]
end
