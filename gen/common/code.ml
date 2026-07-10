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
(* TODO introduce a monad operation? *)
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

type proc = Proc.t
let pp_proc p = Proc.pp p

(* Direction of event *)
type dir = W | R

(* Edges compoments that do not depend on architecture *)

(* Change or proc accross edge *)
type ie = Int|Ext

(* Change of location across edge *)
type sd = Same|Diff

(* Direction of related events *)
type extr = Dir of dir | Irr | NoDir

let equal_ie ie1 ie2 = match ie1,ie2 with
  | Int,Int
  | Ext,Ext -> true
  | (Int|Ext),_ -> false

let equal_sd sd1 sd2 = match sd1,sd2 with
  | Same,Same
  | Diff,Diff -> true
  | (Same|Diff),_ -> false

let equal_extr e1 e2 = match e1,e2 with
  | Dir W,Dir W
  | Dir R,Dir R
  | Irr,Irr
  | NoDir,NoDir -> true
  | (Dir _|Irr|NoDir),_ -> false

(* Associated pretty print & generators *)
let pp_dir = function
  | W -> "W"
  | R -> "R"

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

let pp_sd_macro = function
| None -> "*"
| Some sd -> pp_sd sd

let pp_dir_macro = function
| None -> "*"
| Some d -> pp_dir d

let is_same_loc = function
  | Same -> true
  | _ -> false

let is_diff_loc = function
  | Diff -> true
  | _ -> false

let seq_sd sd1 sd2 =
  match sd1,sd2 with
  | Same,Same -> Same
  | Diff,_|_,Diff -> Diff

let fold_ie f r = f Int (f Ext r)
let fold_sd f r = f Diff (f Same r)
let fold_extr f r = f (Dir W) (f (Dir R) r)
let fold_sd_extr_extr f =
  fold_sd (fun sd -> fold_extr (fun e1 -> fold_extr (fun e2 -> f sd e1 e2)))

let expand_sd_macro sd f r = match sd with
| None -> f Same (f Diff r)
| Some sd -> f sd r

let expand_dir_macro d f r = match d with
| None -> f W (f R r)
| Some d -> f d r

(* Macro components for location: `None` represents any Same/Diff location. *)
let fold_sd_macro_component f r =
  f None (f (Some Diff) (f (Some Same) r))

(* Macro components for direction: `None` represents any R/W direction. *)
let fold_dir_macro_component f r =
  f None (f (Some W) (f (Some R) r))

(* Fold over Same/Diff and direction macro forms, keeping only names with at
   least one wildcard component.  The `choices` argument passed to `f` is the
   concrete expansion of that macro form. *)
let fold_sd_extr_macros f r =
  fold_sd_macro_component
    (fun sd ->
      fold_dir_macro_component
        (fun d r ->
          match sd,d with
          (* Keep only forms that still contain at least one wildcard component. *)
          | Some _,Some _ -> r
          | _,_ ->
              let choices =
                expand_dir_macro d
                  (fun d ->
                    expand_sd_macro sd
                      (fun sd k -> (sd,d)::k))
                  [] in
              f sd d choices r))
    r

(* Fold over all Same/Diff and direction macro triples that contain at least
   one wildcard component.  The `choices` argument passed to `f` is the
   concrete expansion of that triple. *)
let fold_sd_extr_extr_macros f r =
  fold_sd_macro_component
    (fun sd ->
      fold_dir_macro_component
        (fun d1 ->
          fold_dir_macro_component
            (fun d2 r ->
              match sd,d1,d2 with
              (* Keep only forms that still contain at least one wildcard component. *)
              | Some _,Some _,Some _ -> r
              | _,_,_ ->
                  let choices =
                    expand_dir_macro d1
                      (fun d1 ->
                        expand_dir_macro d2
                          (fun d2 ->
                            expand_sd_macro sd
                              (fun sd k -> (sd,d1,d2)::k)))
                      [] in
                  f sd d1 d2 choices r)))
    r

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
type com =  Rf | Fr | Co

let equal_com c1 c2 = match c1,c2 with
  | Rf,Rf
  | Fr,Fr
  | Co,Co -> true
  | (Rf|Fr|Co),_ -> false

let pp_com = function
  | Rf -> "Rf"
  | Fr -> "Fr"
  | Co -> "Co"

let fold_com f r = f Rf (f Fr (f Co r))

(* Info in tests *)
type info = (string * string) list

let plain = "Na"

(* Memory Space *)
type 'a bank = Ord | Tag | CapaTag | CapaSeal | Pte | VecReg of 'a | Pair | Instr

let pp_bank = function
  | Ord -> "Ord"
  | Tag -> "Tag"
  | CapaTag -> "CapaTag"
  | CapaSeal -> "CapaSeal"
  | Pte -> "Pte"
  | VecReg _ -> "VecReg"
  | Pair -> "Pair"
  | Instr -> "Instr"

let add_tag s t = Misc.pp_tagged s t

let add_capability s t = Printf.sprintf "0xffffc0000:%s:%i" s (if t = 0 then 1 else 0)

let add_vector hexa v =
  let open Printf in
  let pp value = sprintf (if hexa then "0x%x" else "%d") value in
  sprintf "{%s}"
    (String.concat "," (List.map pp v))
