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

module Symbol = struct
  type t = Data of string | Label of Label.Full.full

  let compare n1 n2 = match n1, n2 with
    | Data s1, Data s2 -> String.compare s1 s2
    | Label l1, Label l2 -> Label.Full.compare l1 l2
    | Data _, Label _ -> 1
    | Label _, Data _ -> -1

  let equal n1 n2 = match n1, n2 with
    | Data s1, Data s2 -> String.equal s1 s2
    | Label l1, Label l2 -> Label.Full.equal l1 l2
    | _, _ -> false

  let pp = function
    | Data s -> s
    | Label (p,lbl) -> sprintf "%d:%s" p lbl

  let map f = function
    | Data s -> Data (f s)
    | Label (p,s) -> Label (p, f s)

  let of_string s =
    try
      Scanf.sscanf s "%d:%s" (fun p lbl -> Label (p, lbl))
    with Scanf.Scan_failure _ ->
      Data s

  let is_data = function
    | Data _ -> true
    | Label _ -> false

  let is_label = function
    | Data _ -> false
    | Label _ -> true
end

type tag = string option
type cap = Int64.t
type offset = int

let compare_tag = Misc.opt_compare String.compare

(* Symbolic location metadata*)
(* Memory cell, with optional tag, capability<128:95>,optional vector metadata, and offset *)

type symbolic_data =
  {
   name : Symbol.t ;
   tag : tag ;
   cap : cap ;
   offset : offset ;
  }

let default_symbolic_data =
  {
   name = Symbol.Data "" ;
   tag = None ;
   cap = 0x0L ;
   offset = 0 ;
  }

let capa_low c = Int64.shift_left (Int64.logand c 0x1ffffffffL)  3
and capa_high c = Int64.shift_right_logical c 33
let pp_symbolic_data {name=n; tag=t; cap=c; _} =
  let s = Symbol.pp n in
  match t,c with
  | None, 0L -> s
  | None, _ ->
     sprintf "%#Lx:%s:%Li" (capa_low c) s (capa_high c)

  | Some t, 0L -> sprintf "%s:%s" s t
  | Some t, _ ->
      sprintf "%#Lx:%s:%Li:%s" (capa_low c) s (capa_high c) t

let compare_symbolic_data s1 s2 =
  begin match Symbol.compare s1.name s2.name with
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
  Symbol.equal s1.name s2.name
  && Misc.opt_eq Misc.string_eq s1.tag s2.tag
  && Int64.equal s1.cap s2.cap
  && Misc.int_eq s1.offset s2.offset

type syskind = PTE|PTE2|TLB|TAG

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
  | Virtual {name=n; offset=0;_} -> Symbol.pp n
  | sym -> Warn.fatal "symbol '%s' is not an address" (pp_symbol sym)

let oa2symbol oa =
  match OutputAddress.as_physical oa with
  | Some s -> Physical (s,0)
  | None ->
     begin match OutputAddress.as_pte oa with
     | Some s -> System (PTE,s)
     | None -> assert false
     end

let virt_match_phy s1 s2 = match s1,s2 with
  | Virtual {name=n1; offset=i1;_},Physical (s2,i2) ->
    String.equal (Symbol.pp n1) s2 &&
    Misc.int_eq i1 i2
| _,_ -> false

module SC = struct
  type t = symbol
  let compare = compare_symbol
end

module SymbolSet = MySet.Make(SC)
module SymbolMap = MyMap.Make(SC)

type ('scalar, 'pte, 'instr) t =
  | Concrete of 'scalar
  | ConcreteVector of ('scalar, 'pte, 'instr) t list
  | ConcreteRecord of ('scalar, 'pte, 'instr) t StringMap.t
  | Symbolic of symbol
  | Tag of string
  | PteVal of 'pte
  | Instruction of 'instr
  | Frozen of int

let rec compare scalar_compare pteval_compare instr_compare c1 c2 =
  match c1,c2 with
  | Concrete i1, Concrete i2 -> scalar_compare i1 i2
  | ConcreteVector v1, ConcreteVector v2 ->
     Misc.list_compare
       (compare scalar_compare pteval_compare instr_compare) v1 v2
  | ConcreteRecord li1, ConcreteRecord li2 ->
     StringMap.compare
       (compare scalar_compare pteval_compare instr_compare) li1 li2
  | Symbolic sym1,Symbolic sym2 -> compare_symbol sym1 sym2
  | Tag t1,Tag t2 -> String.compare t1 t2
  | PteVal p1,PteVal p2 -> pteval_compare p1 p2
  | Instruction i1,Instruction i2 -> instr_compare i1 i2
  | Frozen i1,Frozen i2 -> Int.compare i1 i2
  | (Concrete _,(ConcreteRecord _|ConcreteVector _|Symbolic _|Tag _|PteVal _|Instruction _|Frozen _))
  | (ConcreteVector _,(ConcreteRecord _|Symbolic _|Tag _|PteVal _|Instruction _|Frozen _))
  | (ConcreteRecord _,(Symbolic _|Tag _|PteVal _|Instruction _|Frozen _))
  | (Symbolic _,(Tag _|PteVal _|Instruction _|Frozen _))
  | (Tag _,(PteVal _|Instruction _|Frozen _))
  | (PteVal _,(Instruction _|Frozen _))
  | (Instruction _,Frozen _)
    -> -1
  | (Frozen _,(Instruction _|PteVal _|Tag _|Symbolic _|ConcreteRecord _|ConcreteVector _|Concrete _))
  | (Instruction _,(PteVal _|Tag _|Symbolic _|ConcreteRecord _|ConcreteVector _|Concrete _))
  | (PteVal _,(Tag _|Symbolic _|ConcreteRecord _|ConcreteVector _|Concrete _))
  | (Tag _,(Symbolic _|ConcreteRecord _|ConcreteVector _|Concrete _))
  | (Symbolic _,(ConcreteRecord _|ConcreteVector _|Concrete _))
  | (ConcreteRecord _,(ConcreteVector _|Concrete _))
  | (ConcreteVector _,Concrete _)
    -> 1

let rec eq scalar_eq pteval_eq instr_eq c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> scalar_eq i1 i2
  | ConcreteVector v1, ConcreteVector v2 ->
     Misc.list_eq (eq scalar_eq pteval_eq instr_eq) v1 v2
  | ConcreteRecord li1, ConcreteRecord li2 ->
    StringMap.equal (eq scalar_eq pteval_eq instr_eq) li1 li2
  | Symbolic s1, Symbolic s2 -> symbol_eq s1 s2
  | Tag t1,Tag t2 -> Misc.string_eq t1 t2
  | PteVal p1,PteVal p2 -> pteval_eq p1 p2
  | Instruction i1,Instruction i2 -> instr_eq i1 i2
  | Frozen i1,Frozen i2 -> Misc.int_eq i1 i2
  | (Frozen _,(Instruction _|Symbolic _|Concrete _|ConcreteRecord _|ConcreteVector _|Tag _|PteVal _))
  | (Instruction _,(Symbolic _|Concrete _|ConcreteRecord _|ConcreteVector _|Tag _|PteVal _|Frozen _))
  | (PteVal _,(Symbolic _|Concrete _|ConcreteRecord _|ConcreteVector _|Tag _|Instruction _|Frozen _))
  | (ConcreteRecord _,(ConcreteVector _|Symbolic _|Tag _|Concrete _|PteVal _|Instruction _|Frozen _))
  | (ConcreteVector _,(ConcreteRecord _|Symbolic _|Tag _|Concrete _|PteVal _|Instruction _|Frozen _))
  | (Concrete _,(Symbolic _|Tag _|ConcreteRecord _|ConcreteVector _|PteVal _|Instruction _|Frozen _))
  | (Symbolic _,(Concrete _|Tag _|ConcreteRecord _|ConcreteVector _|PteVal _|Instruction _|Frozen _))
  | (Tag _,(Concrete _|Symbolic _|ConcreteRecord _|ConcreteVector _|PteVal _|Instruction _|Frozen _))
    -> false

let rec mk_pp pp_symbol pp_scalar pp_pteval pp_instr = function
  | Concrete i -> pp_scalar i
  | ConcreteVector vs ->
      let s =
        String.concat ","
          (List.map (mk_pp pp_symbol pp_scalar pp_pteval pp_instr) vs)
      in
      sprintf "{%s}" s
  | ConcreteRecord vs ->
      let b = Buffer.create 10 in
      Buffer.add_char b '{';
      StringMap.iter
        (fun name c ->
          Printf.bprintf b "%s:%s," name
            (mk_pp pp_symbol pp_scalar pp_pteval pp_instr c))
        vs;
      Buffer.add_char b '}';
      Buffer.contents b
  | Symbolic sym -> pp_symbol sym
  | Tag s -> sprintf ":%s" s
  | PteVal p -> pp_pteval p
  | Instruction i -> pp_instr i
  | Frozen i -> sprintf "S%i" i (* Same as for symbolic values? *)

let pp pp_scalar pp_pteval pp_instr =
  mk_pp pp_symbol pp_scalar pp_pteval pp_instr
and pp_old pp_scalar pp_pteval pp_instr =
  mk_pp pp_symbol_old  pp_scalar pp_pteval pp_instr

let _debug = function
  | Concrete _ -> "Concrete _"
  | ConcreteVector vs -> sprintf "ConcreteVector (%d,_)" (List.length vs)
  | ConcreteRecord vs ->
      "ConcreteRecord (" ^ (StringMap.pp_str (fun key _v -> key) vs) ^ ")"
  | Symbolic sym -> sprintf "Symbol %s" (pp_symbol sym)
  | Tag s -> sprintf "Tag %s" s
  | PteVal _ -> "PteVal"
  | Instruction i -> sprintf "Instruction %s" (InstrLit.pp i)
  | Frozen i -> sprintf "Frozen %i" i

let rec map_scalar f = function
  | Concrete s -> Concrete (f s)
  | ConcreteVector cs -> ConcreteVector (List.map (map_scalar f) cs)
  | ConcreteRecord cs -> ConcreteRecord (StringMap.map (map_scalar f) cs)
  | (Symbolic _ | Tag _ | PteVal _ | Instruction _ | Frozen _) as c ->
      c

let rec map_label f = function
  | Symbolic (Virtual ({name=Symbol.Label (p,lbl); _} as symb)) ->
    Symbolic (Virtual ({symb with name=Symbol.Label (p,f lbl);}))
  | ConcreteVector cs -> ConcreteVector (List.map (map_label f) cs)
  | ConcreteRecord cs -> ConcreteRecord (StringMap.map (map_label f) cs)
  | (Symbolic _ | Concrete _ | Tag _ | PteVal _ | Instruction _ | Frozen _) as m
    ->
      m

let rec map f_scalar f_pteval f_instr = function
  | (Symbolic _  | Tag _ | Frozen _) as m -> m
  | PteVal p -> PteVal (f_pteval p)
  | Instruction i -> Instruction (f_instr i)
  | Concrete s -> Concrete (f_scalar s)
  | ConcreteVector cs ->
      ConcreteVector (List.map (map f_scalar f_pteval f_instr) cs)
  | ConcreteRecord cs ->
      ConcreteRecord (StringMap.map (map f_scalar f_pteval f_instr) cs)

let do_mk_virtual_label p s =
  Virtual { default_symbolic_data with name=Symbol.Label (p,s); }

let do_mk_virtual s =
  Virtual { default_symbolic_data with name=Symbol.of_string s; }

let do_mk_sym sym = match Misc.tr_pte sym with
| Some s -> System (PTE,s)
| None -> match Misc.tr_atag sym with
  | Some s -> System (TAG,s)
  | None -> match Misc.tr_physical sym with
    | Some s -> Physical (s,0)
    | None -> do_mk_virtual sym

let mk_sym_virtual_label p lbl = Symbolic (do_mk_virtual_label p lbl)
let mk_sym_virtual s = Symbolic (do_mk_virtual s)
let mk_sym s = Symbolic (do_mk_sym s)

let mk_sym_with_index s i =
  Symbolic
    (Virtual
      {default_symbolic_data
      with name=Symbol.Data s; offset=i})

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

let mk_vec sz vs =
  assert (sz == (List.length vs));
  ConcreteVector vs

let mk_replicate sz v = ConcreteVector (Misc.replicate sz v)

let is_symbol = function
  | Symbolic _ -> true
  | Concrete _ | ConcreteVector _ | ConcreteRecord _ | Tag _
  | PteVal _ | Instruction _ | Frozen _ ->
      false

let is_data = function
  | Symbolic (Virtual ({name=n; _})) ->
    Symbol.is_data n
  | Concrete _| ConcreteVector _| ConcreteRecord _| Symbolic _| Tag _|
    PteVal _| Instruction _| Frozen _ ->
    false

let is_label = function
  | Symbolic (Virtual ({name=n; _})) -> Symbol.is_label n
  | Concrete _ | ConcreteVector _ | ConcreteRecord _ | Symbolic _ | Tag _
  | PteVal _ | Instruction _ | Frozen _ ->
      false

let as_label = function
  | Symbolic (Virtual ({name=Symbol.Label (p,lbl); _})) -> Some (p,lbl)
  | Concrete _ | ConcreteVector _ | ConcreteRecord _ | Symbolic _ | Tag _
  | PteVal _ | Instruction _ | Frozen _ ->
      None

let is_non_mixed_symbol = function
  | Virtual {offset=idx;_}
  | Physical (_,idx) -> idx=0
  | System _ -> true

let default_tag = Tag "green"

let mk_sym_tag s t =
  Symbolic (Virtual {default_symbolic_data with name=Symbol.Data s; tag=Some t;})

let check_sym v =
  match v with
  | (Symbolic _ | Tag _) as sym -> sym
  | Concrete _ | ConcreteVector _ | ConcreteRecord _ | PteVal _ | Instruction _
  | Frozen _ ->
      assert false

let is_virtual v = match v with
| Symbolic (Virtual _) -> true
| _ -> false

let as_virtual v = match v with
| Symbolic (Virtual {name=symb;_}) -> Some (Symbol.pp symb)
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

let mk_sym_morello p s t =
  let p_int = Misc.string_as_int64 p in
  if
    not (Int64.equal (Int64.logand p_int 0x7L) 0L)
    || Int64.compare p_int (Int64.shift_left 1L 36) >= 0
    || Int64.compare p_int 0L < 0
  then Printf.eprintf "Warning: incorrect address encoding: %#Lx\n" p_int ;
  let truncated_perms = Int64.shift_right_logical p_int 3 in
  let tag = if Misc.string_as_int t <> 0 then 1L else 0L in
  Symbolic
    (Virtual
       {default_symbolic_data
       with
         name=Symbol.Data s;
         cap=Int64.logor truncated_perms (Int64.shift_left tag 33); })

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
