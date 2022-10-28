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

type syskind = TLB|TAG

type symbol =
  | Virtual of symbolic_data
  | Physical of string * int                  (* symbol, index *)
  | System of (syskind * string)              (* System memory *)

let get_index = function
  | Virtual s -> Some s.offset
  | Physical (_,o) -> Some o
  | System _ -> None

let pp_index base o = match o with
| 0 -> base
| i -> sprintf "%s+%i" base i

let rec old2new s =
  match Misc.tr_pte s with
  | Some s ->
     Misc.pp_pte (old2new s)
  | None ->
     begin match Misc.tr_physical s with
     | Some s -> Misc.pp_physical (old2new s)
     | None -> s
     end

let pp_symbol_old = function
  | Virtual s -> pp_index (pp_symbolic_data s) s.offset
  | Physical (s,o) when Misc.is_pte s -> pp_index s o
  | Physical (s,o) -> pp_index (Misc.add_physical s) o
  | System (TLB,s) -> Misc.add_tlb s
  | System (TAG,s) -> sprintf "tag(%s)" s

let pp_symbol = function
  | Virtual s when Misc.is_pte s.name ->
     let name = old2new s.name in
     pp_index (pp_symbolic_data {s with name=name}) s.offset
  | Virtual s -> pp_index (pp_symbolic_data s) s.offset
  | Physical (s,o) when Misc.is_pte s -> pp_index (old2new s) o
  | Physical (s,o) -> pp_index (Misc.pp_physical (old2new s)) o
  | System (TLB,s) -> sprintf "TLB(%s)" s
  | System (TAG,s) -> sprintf "tag(%s)" s

let compare_symbol sym1 sym2 = match sym1,sym2 with
| Virtual s1,Virtual s2 when Misc.is_pte s1.name && not (Misc.is_pte s2.name) -> 1
| Virtual s1,Virtual s2 when not (Misc.is_pte s1.name) && Misc.is_pte s2.name -> -1
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
  match OutputAddress.as_physical oa with
  | Some s -> Physical (s,0)
  | None ->
     begin match OutputAddress.as_pte oa with
     | Some s -> Physical (Misc.add_pte s, 0)
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

type ('scalar,'pte,'instr) t =
  | Concrete of 'scalar
  | ConcreteVector of ('scalar,'pte,'instr) t list
  | Symbolic  of symbol
  | Label of Proc.t * string     (* In code *)
  | Tag of string
  | PteVal of 'pte
  | Instruction of 'instr

let rec compare scalar_compare pteval_compare instr_compare c1 c2 =
  match c1,c2 with
  | Concrete i1, Concrete i2 -> scalar_compare i1 i2
  | ConcreteVector v1, ConcreteVector v2 ->
     Misc.list_compare
       (compare scalar_compare pteval_compare instr_compare) v1 v2
  | Symbolic sym1,Symbolic sym2 -> compare_symbol sym1 sym2
  | Label (p1,s1),Label (p2,s2) ->
      Misc.pair_compare Proc.compare String.compare (p1,s1) (p2,s2)
  | Tag t1,Tag t2 -> String.compare t1 t2
  | PteVal p1,PteVal p2 -> pteval_compare p1 p2
  | Instruction i1,Instruction i2 -> instr_compare i1 i2
  | (Concrete _,(ConcreteVector _|Symbolic _|Label _|Tag _|PteVal _|Instruction _))
  | (ConcreteVector _,(Symbolic _|Label _|Tag _|PteVal _|Instruction _))
  | (Symbolic _,(Label _|Tag _|PteVal _|Instruction _))
  | (Label _,(Tag _|PteVal _|Instruction _))
  | (Tag _,(PteVal _|Instruction _))
  | (PteVal _,Instruction _)
    -> -1
  | (Instruction _,(PteVal _|Tag _|Label _|Symbolic _|ConcreteVector _|Concrete _))
  | (PteVal _,(Tag _|Label _|Symbolic _|ConcreteVector _|Concrete _))
  | (Tag _,(Label _|Symbolic _|ConcreteVector _|Concrete _))
  | (Label _,(Symbolic _|ConcreteVector _|Concrete _))
  | (Symbolic _,(ConcreteVector _|Concrete _))
  | (ConcreteVector _,Concrete _)
    -> 1

let rec eq scalar_eq pteval_eq instr_eq c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> scalar_eq i1 i2
  | ConcreteVector v1, ConcreteVector v2 ->
     Misc.list_eq (eq scalar_eq pteval_eq instr_eq) v1 v2
  | Symbolic s1, Symbolic s2 -> symbol_eq s1 s2
  | Label (p1,s1),Label (p2,s2) ->
      Misc.string_eq  s1 s2 && Misc.int_eq p1 p2
  | Tag t1,Tag t2 -> Misc.string_eq t1 t2
  | PteVal p1,PteVal p2 -> pteval_eq p1 p2
  | Instruction i1,Instruction i2 -> instr_eq i1 i2
  | (Instruction _,(Symbolic _|Concrete _|ConcreteVector _|Label _|Tag _|PteVal _))
  | (PteVal _,(Symbolic _|Concrete _|ConcreteVector _|Label _|Tag _|Instruction _))
  | (ConcreteVector _,(Symbolic _|Label _|Tag _|Concrete _|PteVal _|Instruction _))
  | (Concrete _,(Symbolic _|Label _|Tag _|ConcreteVector _|PteVal _|Instruction _))
  | (Symbolic _,(Concrete _|Label _|Tag _|ConcreteVector _|PteVal _|Instruction _))
  | (Label _,(Concrete _|Symbolic _|Tag _|ConcreteVector _|PteVal _|Instruction _))
  | (Tag _,(Concrete _|Symbolic _|Label _|ConcreteVector _|PteVal _|Instruction _))
    -> false

let rec mk_pp pp_symbol pp_scalar pp_pteval pp_instr = function
    | Concrete i -> pp_scalar i
    | ConcreteVector vs ->
        let s =
          String.concat ","
            (List.map (mk_pp pp_symbol pp_scalar pp_pteval pp_instr) vs)
        in sprintf "{%s}" s
    | Symbolic sym -> pp_symbol sym
    | Label (p,lbl)  -> sprintf "%i:%s" p lbl
    | Tag s -> sprintf ":%s" s
    | PteVal p -> pp_pteval p
    | Instruction i -> pp_instr i

let pp pp_scalar pp_pteval pp_instr =
  mk_pp pp_symbol pp_scalar pp_pteval pp_instr
and pp_old pp_scalar pp_pteval pp_instr =
  mk_pp pp_symbol_old  pp_scalar pp_pteval pp_instr

let _debug = function
| Concrete _ -> "Concrete _"
| ConcreteVector vs -> sprintf "ConcreteVector (%d,_)" (List.length vs)
| Symbolic sym -> sprintf "Symbol %s" (pp_symbol sym)
| Label (p,s) -> sprintf "Label (%s,%s)" (Proc.pp p) s
| Tag s -> sprintf "Tag %s" s
| PteVal _ -> "PteVal"
| Instruction i -> sprintf "Instruction %s" (InstrLit.pp i)


let rec map_scalar f = function
| (Symbolic _|Label _ |Tag _|PteVal _|Instruction _) as c -> c
| Concrete s -> Concrete (f s)
| ConcreteVector cs -> ConcreteVector (List.map (map_scalar f) cs)

let rec map_label f = function
  | Label (p,lbl) -> Label (p,f lbl)
  | ConcreteVector cs -> ConcreteVector (List.map (map_label f) cs)
  | Symbolic _|Concrete _ |Tag _|PteVal _|Instruction _ as m -> m

let rec map f_scalar f_pteval f_instr = function
| Symbolic _ | Label _ | Tag _ as m -> m
| PteVal p -> PteVal (f_pteval p)
| Instruction i -> Instruction (f_instr i)
| Concrete s -> Concrete (f_scalar s)
| ConcreteVector cs ->
   ConcreteVector (List.map (map f_scalar f_pteval f_instr) cs)

let do_mk_virtual s = Virtual { default_symbolic_data with name=s; }

let as_virtual s =
  if Misc.is_physical s || Misc.is_atag s then
    Warn.user_error "Non-virtual id %s" s ;
  s

let do_mk_pte s =
  let s = as_virtual s in
  do_mk_virtual (Misc.add_pte s)

let mk_sym_pte s = Symbolic (do_mk_pte s)

let do_mk_sym sym = match Misc.tr_pte sym with
| Some s -> do_mk_pte s
| None -> match Misc.tr_atag sym with
  | Some s -> System (TAG,s)
  | None -> match Misc.tr_physical sym with
    | Some s -> Physical (s,0)
    | None -> do_mk_virtual sym

let mk_sym_virtual s = Symbolic (do_mk_virtual s)
let mk_sym s = Symbolic (do_mk_sym s)

let mk_sym_pa s =
  let s = as_virtual s in
  Symbolic (Physical (s,0))

let mk_vec sz vs =
  assert (sz == (List.length vs));
  ConcreteVector vs

let mk_replicate sz v = ConcreteVector (Misc.replicate sz v)

let is_symbol = function
  | Symbolic _ -> true
  | Concrete _|ConcreteVector _|Label _|Tag _| PteVal _|Instruction _ -> false

let is_label = function
  | Label _ -> true
  | Concrete _|ConcreteVector _|Symbolic _|Tag _ |PteVal _|Instruction _ -> false

let is_non_mixed_symbol = function
  | Virtual {offset=idx;_}
  | Physical (_,idx) -> idx=0
  | System _ -> true

let default_tag = Tag "green"

let check_sym v =  match v with
| Symbolic _|Label _|Tag _ as sym -> sym
| Concrete _|ConcreteVector _|PteVal _|Instruction _ ->  assert false

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
| Symbolic (Physical (s, 0)) when Misc.is_pte s -> Some v
| _ -> None

let is_pte v = match v with
| Symbolic (Physical (s, 0)) -> Misc.is_pte s
| _ -> false

module type S =  sig

  module Scalar : Scalar.S
  module PteVal : PteVal.S
  module Instr : Instr.S

  type v = (Scalar.t,PteVal.t,Instr.t) t

  val tr : (string,ParsedPteVal.t,InstrLit.t) t -> v
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
  val is_nop : v -> bool
end
