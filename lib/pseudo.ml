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

(** Closed signature for basic view of architectures *)

(* Restricted signature, for dumping *)

module type Types = sig
  type pins (* Parsed instruction *)
  type ins  (* Final instruction  *)

  type reg_arg

  type 'ins kpseudo =
    | Nop
    | Label of string * 'ins kpseudo
    | Instruction of 'ins
    | Macro of string * reg_arg list
    | Symbolic of string

  type pseudo = ins kpseudo
  type parsedPseudo = pins kpseudo

  type 'ins prog = (MiscParser.proc * 'ins kpseudo list) list
end

module type S = sig

  include Types

(* Lifting of fold/map *)
  val pseudo_map : ('a -> 'b) -> 'a kpseudo -> 'b kpseudo
  val pseudo_fold : ('a -> 'b -> 'a) -> 'a -> 'b kpseudo -> 'a
  val pseudo_iter : ('a -> unit) -> 'a kpseudo -> unit

(* Fold over instructions in code *)
  val fold_pseudo_code : ('a -> 'b -> 'a ) -> 'a -> 'b kpseudo list -> 'a

(* Fold/Map over labels *)
  val fold_labels : ('a -> string -> 'a) -> 'a -> pseudo -> 'a
  val map_labels : (string -> string) -> pseudo -> pseudo


(* For printing the program, code per processor *)
  type nice_prog = (MiscParser.proc * pseudo list) list

(* Counting (static) memory accesses *)
  val get_naccesses : pseudo list -> int

(* Translate from parsed instructions *)
  val pseudo_parsed_tr : parsedPseudo -> pseudo

(* Lift to pseudo code *)
  val lift_code : 'a list -> 'a kpseudo list

(* Find offest of label (may raise Not_found) *)
  val find_offset : string -> 'a kpseudo list -> int

(* Does exist some instruction s.t. predicate yields true *)
  val code_exists : (ins -> bool) -> pseudo list -> bool

(* Get instructions pointed to by a set of labels *)
  val from_labels :
      Label.Full.Set.t -> 'ins prog -> (Label.Full.full * 'ins) list

  (* Get all labels in the code *)
  val all_labels : ins prog -> Label.Full.full list

end


(* Input signature *)
module type I = sig
  type ins
  type pins
  type reg_arg
(* translate from parsed *)
  val parsed_tr : pins -> ins

(* Number of memory access per instruction *)
  val get_naccesses : ins -> int
(* fold/map over labels in instructions,
   used for label normalisation *)
  val fold_labels : 'a -> ('a -> Label.t -> 'a) -> ins -> 'a
  val map_labels : (Label.t -> Label.t) -> ins -> ins

end

(* Common to all arch, memevents and  litmus *)

module Make
    (I : I) : S
with type ins = I.ins and type pins = I.pins and type reg_arg = I.reg_arg
=
struct
  type ins = I.ins
  type pins = I.pins
  type reg_arg = I.reg_arg
(* Parsed instructions, ie instructions enriched with labels *)
  type 'ins kpseudo =
    | Nop
    | Label of Label.t * 'ins kpseudo
    | Instruction of 'ins
    | Macro of string * reg_arg list
    | Symbolic of string

  type pseudo = ins kpseudo
  type parsedPseudo = pins kpseudo
  type 'ins prog = (MiscParser.proc * 'ins kpseudo list) list


(* Fold/Map lifting *)
  let rec pseudo_map f ins = match ins with
    | Nop -> Nop
    | Instruction ins -> Instruction (f ins)
    | Label (lbl,ins) -> Label (lbl, pseudo_map f ins)
    | Symbolic s -> Symbolic s
    | Macro (_,_) -> assert false

  let rec pseudo_fold f k ins = match ins with
    | Nop -> k
    | Instruction ins -> f k ins
    | Label (_,ins) -> pseudo_fold f k ins
    | Symbolic _ -> k
    | Macro (_,_) -> assert false

  let pseudo_exists p = pseudo_fold (fun b i -> b || p i) false
  let pseudo_iter f ins = pseudo_fold (fun () ins -> f ins) () ins

  let fold_pseudo_code f = List.fold_left (pseudo_fold f)

(* Fold/Map over labels *)

  let rec fold_labels f k ins = match ins with
  | Nop -> k
  | Instruction ins -> I.fold_labels k f ins
  | Label (lbl,ins) -> fold_labels f (f k lbl) ins
  | Symbolic _ -> k
  | Macro _ -> assert false

  let rec map_labels f ins = match ins with
  | Nop -> Nop
  | Instruction ins -> Instruction (I.map_labels f ins)
  | Label (lbl,ins) -> Label (f lbl, map_labels f ins)
  | Symbolic s -> Symbolic s
  | Macro _ -> assert false

(* For printing the program, code per processor *)
  type nice_prog = (MiscParser.proc * pseudo list) list

(* Counting memory accesses *)
  let get_naccesses code =
    List.fold_left
      (pseudo_fold
         (fun k ins -> k + I.get_naccesses ins))
      0 code

(* Translate *)
  let pseudo_parsed_tr p = pseudo_map I.parsed_tr p

(* Useful *)
  let lift_code xs = List.map (fun i -> Instruction i) xs

  let find_offset lbl0 =
    let rec find_rec k = function
      | [] -> raise Not_found
      | Label (lbl,_)::_ when Misc.string_eq lbl lbl0 -> k
      | (Nop|Label (_,Nop))::is -> find_rec k is
      | _::is -> find_rec (k+1) is in
    find_rec 0

  let code_exists p =
    let rec exists = function
      | [] -> false
      | ins::code -> pseudo_exists p ins || exists code in
    exists

(* Extract instructions pointed by label set *)

  let rec add_next_instr lbl code k =
    match code with
    | [] -> k
    | (Nop|Label (_,Nop))::code -> add_next_instr lbl code k
    | (Label (_,Instruction i)|Instruction i)::_ -> (lbl,i)::k
    | Label (_,i)::code -> add_next_instr lbl (i::code) k
    | (Symbolic _|Macro _)::_ -> assert false

  let from_labels_code lbls p code =
    let rec do_rec code k =
      match code with
      | [] -> k
      | Label (lbl,_)::rem ->
          let full_lbl = (p,lbl) in
          let k =
           if Label.Full.Set.mem full_lbl lbls then
             add_next_instr full_lbl code k
           else k in
         do_rec rem k
      | _::rem ->
         do_rec rem k in
    do_rec code

  let from_labels lbls prog =
    if Label.Full.Set.is_empty lbls then []
    else
      List.fold_left
        (fun k (p,code) -> from_labels_code lbls (MiscParser.proc_num p) code k)
        [] prog

  let all_labels prog =
    let rec fold_labels f k ins = match ins with
      | Label (lbl,ins) -> fold_labels f (f k lbl) ins
      | _ -> k
    in
    let flbls =
      List.fold_left
        (fun flbls (p,code) ->
          List.fold_left
            (fun flbls ins ->
              fold_labels
                (fun flbls lbl -> (MiscParser.proc_num p,lbl)::flbls) flbls ins)
            flbls code)
        [] prog in
    List.rev flbls

end
