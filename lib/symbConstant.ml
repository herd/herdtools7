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

module Make(Scalar:Scalar.S) = struct
  open Printf

  module Scalar = Scalar

  type v = Scalar.t Constant.t
  open Constant

  let intToV i = Concrete (Scalar.of_int i)
  and nameToV s = Symbolic {default_symbolic_data with name=s}

  let bit_at k v = Scalar.bit_at k v

  let zero = Concrete Scalar.zero
  and one = Concrete Scalar.one

  let tag_compare = Misc.opt_compare String.compare

  let pp_location {name=s;tag=t;cap=c; _} = match t,c with
   | None, 0 -> s
   | None, _ -> sprintf "%#x:%s:%i" ((c land 0x1ffffffff) lsl 3) s (c lsr 33)
   | Some t, 0 -> sprintf "%s:%s" s t
   | Some t, _ -> sprintf "%#x:%s:%i:%s" ((c land 0x1ffffffff) lsl 3) s (c lsr 33) t

  let rec pp hexa = function
    | Concrete i -> Scalar.pp hexa i
    | ConcreteVector (_,vs) ->
      let s = String.concat "," (List.map (pp hexa) vs)
      in sprintf "[%s]" s
    | Symbolic ({offset=0; _} as s) -> pp_location s
    | Symbolic ({offset=o; _} as s) -> sprintf "%s+%i" (pp_location s) o
    | Label (p,lbl)  -> sprintf "%i:%s" p lbl
    | Tag s -> sprintf ":%s" s

  let pp_v = pp false

  let rec compare c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2
  | ConcreteVector (sz1, v1), ConcreteVector (sz2, v2) ->
    let vs = List.map2 (fun i1 i2 -> (i1,i2)) v1 v2 in
    let check_vec = List.fold_right
      (fun (i1,i2) s -> (compare i1 i2) + s)
      vs
      0 in
    (Misc.int_compare sz1 sz2) + check_vec
  | Symbolic s1,Symbolic s2 ->
      (* We do not commpare vector metadata as mk_sym can be called *)
      (* in arbitrary places where the metadata is not available e.g in Sem files *)
      (* this would mean `compare v[8] (v[8] (with metadata))` would fail *)
      begin match String.compare s1.name s2.name with
      | 0 ->
          begin match tag_compare s1.tag s2.tag with
          | 0 ->
              begin match Misc.int_compare s1.cap s2.cap with
              | 0 -> Misc.int_compare s1.offset s2.offset
              | r -> r
              end
          | r -> r
          end
      | r -> r
      end
  | Label (p1,s1),Label (p2,s2) ->
      begin match String.compare s1 s2 with
      | 0 -> Proc.compare p1 p2
      | r -> r
      end
  | Tag t1,Tag t2 -> String.compare t1 t2
  | (Concrete _,(Symbolic _|Label _|Tag _|ConcreteVector _))
  | (Symbolic _,(Label _|Tag _|ConcreteVector _))
  | (Label _,Tag _)
      -> -1
  | (Symbolic _|Label _|Tag _|ConcreteVector _),Concrete _
  | ((Label _|Tag _|ConcreteVector _),(Symbolic _| ConcreteVector _))
  | (Tag _,Label _)
  | (ConcreteVector _, (Label _ | Tag _))
      -> 1

  let tag_eq = Misc.opt_eq Misc.string_eq

  let location_eq
    {name=s1; tag=t1; cap=c1; vdata=v1; offset=o1}
    {name=s2; tag=t2; cap=c2; vdata=v2; offset=o2} =
    Misc.string_eq s1 s2 && tag_eq t1 t2 && Misc.int_eq c1 c2
    && Misc.opt_eq
        (fun (x1,y1) (x2,y2) -> Misc.int_eq x1 x2 && Misc.int_eq y1 y2)
        v1 v2
    && Misc.int_eq o1 o2

  let rec eq c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Scalar.compare i1 i2 = 0
  | Symbolic s1, Symbolic s2 -> location_eq s1 s2
  | Label (p1,s1),Label (p2,s2) ->
      Misc.string_eq  s1 s2 && Misc.int_eq p1 p2
  | ConcreteVector (sz1,v1), ConcreteVector (sz2,v2) ->
      let vs = List.map2 (fun i1 i2 -> eq i1 i2) v1 v2 in
      Misc.int_eq sz1 sz2 && List.for_all (fun x -> x) vs
  | Tag t1,Tag t2 -> Misc.string_eq t1 t2
  | (ConcreteVector _,(Symbolic _|Label _|Tag _|Concrete _))
  | (Concrete _,(Symbolic _|Label _|Tag _|ConcreteVector _))
  | (Symbolic _,(Concrete _|Label _|Tag _|ConcreteVector _))
  | (Label _,(Concrete _|Symbolic _|Tag _|ConcreteVector _))
  | (Tag _,(Concrete _|Symbolic _|Label _|ConcreteVector _))
    -> false

 (* For building code symbols, significant for symbols only ? *)
  let vToName = function
    | Symbolic {name=s;offset=0;_} -> s
    | Symbolic {name=s;offset=idx;_} -> Printf.sprintf "%s[%d]" s idx
    | Concrete _|Label _|Tag _| ConcreteVector _ -> assert false
end
