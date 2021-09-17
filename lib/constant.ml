(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

(** Constants in code *)

(*Metadata for when  a constant is a vector element *)
(*vector metadata - prim size (arg1) and total array size (arg2) *)
(*needed for is-non-mixed-symbol and global vectors *)

type tag = string option
type cap = Int64.t
type offset = int

let compare_tag = Misc.opt_compare String.compare

(* Symbolic location metadata*)
(* Memory cell, with optional tag, capability<128:95>,optional vector metadata, and offset *)

type symbolic_data =
  {
   name : string ;
   tag : tag ;
   cap : cap ;
   offset : offset ;
  }

let default_symbolic_data =
  {
   name = "" ;
   tag = None ;
   cap = 0x0L ;
   offset = 0 ;
  }

let capa_low c = Int64.shift_left (Int64.logand c 0x1ffffffffL)  3
and capa_high c = Int64.shift_right_logical c 33
let pp_symbolic_data {name=s; tag=t; cap=c; _} = match t,c with
  | None, 0L -> s
  | None, _ ->
     sprintf "%#Lx:%s:%Li" (capa_low c) s (capa_high c)

  | Some t, 0L -> sprintf "%s:%s" s t
  | Some t, _ ->
      sprintf "%#Lx:%s:%Li:%s" (capa_low c) s (capa_high c) t

let compare_symbolic_data s1 s2 =
  begin match String.compare s1.name s2.name with
  | 0 ->
      begin match compare_tag s1.tag s2.tag with
      | 0 ->
          begin match Int64.compare s1.cap s2.cap with
          | 0 -> Misc.int_compare s1.offset s2.offset
          | r -> r
          end
      | r -> r
      end
  | r -> r
  end

let symbolic_data_eq s1 s2 =
  Misc.string_eq s1.name s2.name
  && Misc.opt_eq Misc.string_eq s1.tag s2.tag
  && Int64.equal s1.cap s2.cap
  && Misc.int_eq s1.offset s2.offset

type syskind = PTE|PTE2|TLB|TAG

type symbol =
  | Virtual of symbolic_data
  | Physical of string * int                  (* symbol, index *)
  | System of (syskind * string)              (* System memory *)

let pp_index base o = match o with
| 0 -> base
| i -> sprintf "%s+%i" base i

let pp_symbol_old = function
  | Virtual s -> pp_index (pp_symbolic_data s) s.offset
  | Physical (s,o) -> pp_index (Misc.add_physical s) o
  | System (TLB,s) -> Misc.add_tlb s
  | System (PTE,s) -> Misc.add_pte s
  | System (PTE2,s) -> Misc.add_pte (Misc.add_pte s)
  | System (TAG,s) -> sprintf "tag(%s)" s

let pp_symbol = function
  | Virtual s -> pp_index (pp_symbolic_data s) s.offset
  | Physical (s,o) -> pp_index (sprintf "PA(%s)" s) o
  | System (TLB,s) -> sprintf "TLB(%s)" s
  | System (PTE,s) -> sprintf "PTE(%s)" s
  | System (PTE2,s) -> sprintf "PTE(PTE(%s))" s
  | System (TAG,s) -> sprintf "tag(%s)" s

let compare_symbol sym1 sym2 = match sym1,sym2 with
| Virtual s1,Virtual s2 -> compare_symbolic_data s1 s2
| Physical (s1,o1),Physical (s2,o2) ->
    begin match String.compare s1 s2 with
    | 0 -> Misc.int_compare o1 o2
    | r -> r
    end
| System (t1,s1),System (t2,s2) ->
    begin match compare t1 t2 with
    | 0 -> String.compare s1 s2
    | r -> r
    end
| (Virtual _,(Physical _|System _ ))
| (Physical _,System _) -> -1
| ((Physical _|System _),Virtual _)
| (System _,Physical _) -> 1

let symbol_eq s1 s2 = match s1,s2 with
  | Virtual s1,Virtual s2 -> symbolic_data_eq s1 s2
  | Physical (s1,o1),Physical (s2,o2) ->
      Misc.string_eq s1 s2 && Misc.int_eq o1 o2
  | System (k1,s1),System (k2,s2) ->
      k1=k2 && Misc.string_eq s1 s2
  | (Virtual _,(Physical _|System _))
  | (Physical _,(Virtual _|System _))
  | (System _,(Virtual _|Physical _))
    -> false

let as_address = function
  | Virtual {name=s; offset=0;_} -> s
  | sym -> Warn.fatal "symbol '%s' is not an address" (pp_symbol sym)

let oa2symbol oa =
  match PTEVal.as_physical oa with
  | Some s -> Physical (s,0)
  | None ->
     begin match PTEVal.as_pte oa with
     | Some s -> System (PTE,s)
     | None -> assert false
     end

let virt_match_phy s1 s2 = match s1,s2 with
| Virtual {name=s1; offset=i1;_},Physical (s2,i2) ->
    Misc.string_eq s1 s2 && Misc.int_eq i1 i2
| _,_ -> false

module SC = struct
  type t = symbol
  let compare = compare_symbol
end

module SymbolSet = MySet.Make(SC)
module SymbolMap = MyMap.Make(SC)

type 'scalar t =
  | Concrete of 'scalar
  | ConcreteVector of int * 'scalar t list
  | Symbolic  of symbol
  | Label of Proc.t * string     (* In code *)
  | Tag of string
  | PteVal of PTEVal.t

let _debug = function
| Concrete _ -> "Concrete _"
| ConcreteVector (sz,_) -> sprintf "ConcreteVector (%d,_)" sz
| Symbolic sym -> sprintf "Symbol %s" (pp_symbol sym)
| Label (p,s) -> sprintf "Label (%s,%s)" (Proc.pp p) s
| Tag s -> sprintf "Tag %s" s
| PteVal p -> sprintf "PteVal %s" (PTEVal.pp p)

let rec map_scalar f = function
| Concrete  s -> Concrete (f s)
| ConcreteVector (sz,cs) -> ConcreteVector (sz,List.map (map_scalar f) cs)
| (Symbolic _|Label _ |Tag _|PteVal _) as c -> c

let do_mk_virtual s = Virtual { default_symbolic_data with name=s; }

let do_mk_sym sym = match Misc.tr_pte sym with
| Some s -> System (PTE,s)
| None -> match Misc.tr_atag sym with
  | Some s -> System (TAG,s)
  | None -> match Misc.tr_physical sym with
    | Some s -> Physical (s,0)
    | None -> do_mk_virtual sym

let mk_sym_virtual s = Symbolic (do_mk_virtual s)
let mk_sym s = Symbolic (do_mk_sym s)

let as_virtual s =
  if Misc.is_pte s || Misc.is_physical s || Misc.is_atag s then
    Warn.user_error "Non-virtual id %s" s ;
  s


let mk_sym_pte s =
  let s = as_virtual s in
  Symbolic (System (PTE,s))

let mk_sym_pte2 s =
  let s = as_virtual s in
  Symbolic (System (PTE2,s))

let mk_sym_pa s =
  let s = as_virtual s in
  Symbolic (Physical (s,0))

let old2new s =
 match Misc.tr_pte s with
| Some s ->
   begin match Misc.tr_pte s with
   | Some s -> Misc.pp_pte (Misc.pp_pte s)
   | None -> Misc.pp_pte s
   end
| None ->
   begin match Misc.tr_physical s with
   | Some s -> Misc.pp_physical s
   | None -> s
   end

let mk_vec sz v =
  assert (sz == (List.length v));
  ConcreteVector (sz, v)

let mk_replicate sz v = ConcreteVector (sz, Misc.replicate sz v)

let is_symbol = function
  | Symbolic _ -> true
  | Concrete _|ConcreteVector _| Label _| Tag _ | PteVal _ -> false

let is_non_mixed_symbol = function
  | Virtual {offset=idx;_}
  | Physical (_,idx) -> idx=0
  | System _ -> true

let default_tag = Tag "green"

let check_sym v =  match v with
| Symbolic _|Label _|Tag _ as sym -> sym
| Concrete _|ConcreteVector (_,_)|PteVal _ ->  assert false

let is_virtual v = match v with
| Symbolic (Virtual _) -> true
| _ -> false

let as_virtual v = match v with
| Symbolic (Virtual {name=s;_}) -> Some s
| _ -> None

let as_symbol = function
  | Symbolic sym -> Some sym
  | _ -> None

let as_symbolic_data =function
| Symbolic (Virtual sym) -> Some sym
| _ -> None

let of_symbolic_data sym = Symbolic (Virtual sym)

let as_pte v = match v with
| Symbolic (System ((PTE|PTE2),_)) -> Some v
| _ -> None

let is_pt v = match v with
| Symbolic (System ((PTE|PTE2),_)) -> true
| _ -> false

let same_oa v1 v2 =
  let open PTEVal in
  match v1,v2 with
  | PteVal p1,PteVal p2 ->  PTEVal.oa_eq p1.oa p2.oa
  | _ -> false

let writable ha hd v =
  let open PTEVal in
  match v with
  | PteVal p ->
      (p.af=1 || ha) && (* access allowed *)
      (p.db=1 || (p.dbm=1 && hd)) (* write allowed *)
  | _ -> false

module type S =  sig

  module Scalar : Scalar.S

  type v = Scalar.t t
  val intToV  : int -> v
  val stringToV  : string -> v
  val nameToV  : string -> v
  val zero : v
  val one : v
  val bit_at : int -> Scalar.t -> Scalar.t
  val pp : bool -> v -> string (* true -> hexa *)
  val pp_unsigned : bool -> v -> string (* true -> hexa *)
  val pp_v  : v -> string
  val pp_v_old  : v -> string
  val compare : v -> v -> int
  val eq : v -> v -> bool
  val vToName : v -> string

  exception Result of Archs.t * v * string
end
