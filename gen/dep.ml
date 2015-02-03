(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Power / ARM dependencies *)

type dp = ADDR | DATA | CTRL | CTRLISYNC

let sig_of_dp = function
      | ADDR -> 'A'
      | DATA -> 'D'
      | CTRL -> 'C'
      | CTRLISYNC -> 'I'

let fold_dpr f r =  f ADDR (f CTRL (f CTRLISYNC r))
let fold_dpw f r =  f ADDR (f DATA (f CTRL (f CTRLISYNC r)))
    
let ddr_default = Some ADDR
let ddw_default = Some DATA
let ctrlr_default = Some CTRLISYNC
let ctrlw_default = Some CTRL
    
let is_ctrlr = function
  | CTRLISYNC -> true
  | _ -> false

let fst_dp = function
  | CTRL -> [CTRL]
  | CTRLISYNC -> [CTRL;CTRLISYNC]
  | ADDR|DATA -> []

let sequence_dp d1 d2 = match d1 with
| ADDR -> [d2]
| DATA|CTRL|CTRLISYNC -> []
