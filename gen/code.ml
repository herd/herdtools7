(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Event components *)
type loc = Data of string | Code of Label.t

let as_data = function
  | Data loc -> loc
  | Code _ -> assert false

let is_data = function
  | Data _ -> true
  | Code _ -> false

let pp_loc = function Data s | Code s -> s

let loc_eq loc1 loc2 = match loc1,loc2 with
| (Data s1,Data s2)
| (Code s1,Code s2)
  -> Misc.string_eq s1 s2
| (Data _,Code _)
| (Code _,Data _)
  -> false

let loc_compare loc1 loc2 = match loc1,loc2 with
| Data _,Code _ -> -1
| Code _,Data _ -> 1
| (Data s1,Data s2)
| (Code s1,Code s2)
    -> compare s1 s2

module LocOrd = struct
  type t = loc
  let compare = loc_compare
end

module LocSet = MySet.Make(LocOrd)
module LocMap = MyMap.Make(LocOrd)

let loc_none = Data "*"
let ok_str = "ok"
let ok = Data ok_str

let myok p n = Data (Printf.sprintf "ok%i%i" p n)
let myok_proc p = Data (Printf.sprintf "ok%i" p)

type v = int

let pp_v ?(hexa=false) =
  Printf.sprintf
    (if hexa then "0x%x" else "%d")

type proc = Proc.t
let pp_proc p = Proc.pp p

type env = (string * v) list

(* Direction of event *)
type dir = W | R | J | D | I

(* Edges compoments that do not depend on architecture *)

(* Change or proc accross edge *)
type ie = Int|Ext

(* Change of location across edge *)
type sd = Same|Diff

(* Direction of related events *)
type extr = Dir of dir | Irr | NoDir

(* Associated pretty print & generators *)
let pp_dir = function
  | W -> "W"
  | R -> "R"
  | J -> "J"
  | D -> "{DC.CVAU}"
  | I -> "{IC.IVAU}"

let pp_ie = function
  | Int -> "i"
  | Ext -> "e"

let pp_extr = function
  | Dir d -> pp_dir d
  | Irr -> "*"
  | NoDir -> ""

let pp_sd = function
  | Same -> "s"
  | Diff -> "d"

let seq_sd sd1 sd2 =
  match sd1,sd2 with
  | Same,Same -> Same
  | Diff,_|_,Diff -> Diff

let fold_ie f r = f Ext (f Int r)
let fold_sd f r = f Diff (f Same r)
let do_fold_extr withj f r =
  let r = f (Dir W) (f (Dir R) (f (Dir D) (f (Dir I) (f Irr r)))) in
  if withj then f (Dir J) r
  else r
let fold_extr f r = do_fold_extr false f r
let fold_sd_extr f = fold_sd (fun sd -> fold_extr (fun e -> f sd e))
let fold_sd_extr_extr f =
  fold_sd_extr (fun sd e1 -> fold_extr (fun e2 -> f sd e1 e2))

type check =
  | Default | Sc | Uni | Thin | Critical
  | Free | Ppo | Transitive | Total | MixedCheck

let pp_check =
  function
    | Default -> "default"
    | Sc -> "sc"
    | Uni -> "uni"
    | Thin -> "thin"
    | Critical -> "critical"
    | Free -> "free"
    | Ppo -> "ppo"
    | Transitive -> "transitive"
    | Total -> "total"
    | MixedCheck -> "mixedcheck"

let checks =
  [
   "default";
   "sc";
   "uni";
   "thin";
   "critical";
   "free";
   "ppo";
   "transitive";
   "total";
   "mixedcheck";
 ]


(* Com relation *)
type com =  CRf | CFr | CWs

let pp_com = function
  | CRf -> "Rf"
  | CFr -> "Fr"
  | CWs -> "Co"

let fold_com f r = f CRf (f CFr (f CWs r))

(* Info in tests *)
type info = (string * string) list

let plain = "Na"

(* Memory Space *)
type 'a bank = Ord | Tag | CapaTag | CapaSeal | Pte | VecReg of 'a | Pair

let pp_bank = function
  | Ord -> "Ord"
  | Tag -> "Tag"
  | CapaTag -> "CapaTag"
  | CapaSeal -> "CapaSeal"
  | Pte -> "Pte"
  | VecReg _ -> "VecReg"
  | Pair -> "Pair"

let tag_of_int  = function
  | 0 -> "green"
  | 1 -> "red"
  | 2 -> "blue"
  | 3 -> "black"
  | 4 -> "white"
  | 5 -> "cyan"
  | 6 -> "yellow"
  | 7 -> "magenta"
  | n -> Warn.fatal "Sorry, not pretty tag for number %i" n

let add_tag s t = Printf.sprintf "%s:%s" s (tag_of_int t)

let add_capability s t = Printf.sprintf "0xffffc0000:%s:%i" s (if t = 0 then 1 else 0)

let add_vector hexa v =
  let open Printf in
  let pp = pp_v ~hexa:hexa in
  sprintf "{%s}"
    (String.concat "," (List.map pp  v))
