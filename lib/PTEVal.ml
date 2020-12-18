(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

type t = {
  oa : string;
  valid : int;
  af : int;
  db : int;
  dbm : int;
  }

(* For ordinary tests not to fault, the dirty bit has to be set. *)
let prot_default =  { oa=""; valid=1; af=1; db=1; dbm=0; }
let default s = { prot_default with  oa=Misc.add_physical s; }

let pp_field ok pp eq ac p k =
  let f = ac p in if not ok && eq f (ac prot_default) then k else pp f::k

let pp_int_field ok name = pp_field ok (sprintf "%s:%i" name) Misc.int_eq
let pp_valid ok = pp_int_field ok "valid" (fun p -> p.valid)
and pp_af ok = pp_int_field ok "af" (fun p -> p.af)
and pp_db ok = pp_int_field ok "db" (fun p -> p.db)
and pp_dbm ok = pp_int_field ok "dbm" (fun p -> p.dbm)

let set_oa p s = { p with oa = Misc.add_physical s; }

let is_default t = t.valid=1 && t.af=1 && t.db=1 && t.dbm=0

let do_pp showall p =
  let k = pp_valid showall p [] in
  let k = pp_dbm showall p k in
  let k = pp_db showall p k in
  let k = pp_af showall p k in
  let k = sprintf "oa:%s" p.oa::k  in
  let fs = String.concat ", " k in
  sprintf "(%s)" fs

(* By default pp does not list fields whose value is default *)
let pp = do_pp false
(* For initial values dumped for hashing, pp_hash is different,
   for not altering hashes as much as possible *)
let pp_hash = do_pp true

let my_int_of_string s v =
  let v = try int_of_string v with
    _ -> Warn.user_error "PTE field %s should be an integer" s
  in v

let of_list pte l =

  let add_field a (s,v) = match s with
  | "oa" -> { a with oa = v }
  | "af" -> { a with af = my_int_of_string s v }
  | "db" -> { a with db = my_int_of_string s v }
  | "dbm" -> { a with dbm = my_int_of_string s v }
  | "valid" -> { a with valid = my_int_of_string s v }
  | _ ->
      Warn.user_error "Illegal PTE property %s" s in

  let rec of_list a = function
    | [] -> a
    | h::t -> of_list (add_field a h) t in

  of_list (default pte) l

let lex_compare c1 c2 x y  = match c1 x y with
| 0 -> c2 x y
| r -> r

let compare =
  lex_compare
    (fun p1 p2 -> String.compare p1.oa p2.oa)
    (lex_compare
       (fun p1 p2 -> Misc.int_compare p1.af p2.af)
       (lex_compare
          (fun p1 p2 -> Misc.int_compare p1.db p2.db)
          (lex_compare
             (fun p1 p2 -> Misc.int_compare p1.dbm p2.dbm)
             (fun p1 p2 -> Misc.int_compare p1.valid p2.valid))))

let eq p1 p2 =
  Misc.string_eq p1.oa p2.oa &&
  Misc.int_eq p1.af p2.af &&
  Misc.int_eq p1.db p2.db &&
  Misc.int_eq p1.dbm p2.dbm &&
  Misc.int_eq p1.valid p2.valid
