(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Closed signature for basic view of architectures *)

let func_size = 1000

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
    | Align of int

  type pseudo = ins kpseudo
  type parsedPseudo = pins kpseudo

  type 'ins prog = (MiscParser.proc * 'ins kpseudo list) list
end

module type S = sig

  include Types

(* Lifting of fold/map *)
  val pseudo_map : ('a -> 'b) -> 'a kpseudo -> 'b kpseudo
  val pseudo_fold : ('a -> 'b -> 'a) -> 'a -> 'b kpseudo -> 'a
  val pseudo_exists : ('a -> bool) -> 'a kpseudo -> bool
  val pseudo_dump : ('a -> string) -> 'a kpseudo -> string
  val pseudo_iter : ('a -> unit) -> 'a kpseudo -> unit

(* Fold over instructions in code *)
  val fold_pseudo_code : ('a -> 'b -> 'a ) -> 'a -> 'b kpseudo list -> 'a
  val exists_pseudo_code : ('ins -> bool) -> 'ins kpseudo list -> bool

(* Fold/Map over labels *)
  val fold_labels : ('a -> Label.t -> 'a) -> 'a -> pseudo -> 'a
  val map_labels_base : (BranchTarget.t -> BranchTarget.t) -> ins -> ins
  val map_labels : (Label.t -> Label.t) -> pseudo -> pseudo
  val fold_label_addr :
    (Label.t -> int -> 'a -> 'a) -> 'a -> int -> pseudo list -> 'a

(* For printing the program, code per processor *)
  type nice_prog = (MiscParser.proc * pseudo list) list

(* Counting (static) memory accesses *)
  val get_naccesses : pseudo list -> int

(* Translate from parsed instructions *)
  val pseudo_parsed_tr : parsedPseudo -> pseudo

(* Lift to pseudo code *)
  val lift_code : 'a list -> 'a kpseudo list

(* Does exist some instruction s.t. predicate yields true *)
  val code_exists : (ins -> bool) -> pseudo list -> bool

(* Group code and handler *)
  val code_by_proc : ins prog -> (Proc.t * (pseudo list * pseudo list)) list

(* Get instructions pointed to by a set of labels *)
  val from_labels :
    Label.Full.Set.t -> ins prog -> (Label.Full.full * ins) list

  (* Get all labels in the code *)
  val all_labels : ins prog -> Label.Full.full list

  (* Load code, the herd way *)
  val size_of_ins : ins -> int

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

(* Instruction size, for loading and branch offset computation,
   need not be correct? (herd and printing) *)
  val size_of_ins : ins -> int

(* fold/map over labels in instructions,
   used for label normalisation *)
  val fold_labels : 'a -> ('a -> Label.t -> 'a) -> ins -> 'a
  val map_labels : (BranchTarget.t -> BranchTarget.t) -> ins -> ins

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
    | Align of int

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
    | Align n -> Align n

  let rec pseudo_fold f k ins = match ins with
    | Nop -> k
    | Instruction ins -> f k ins
    | Label (_,ins) -> pseudo_fold f k ins
    | Symbolic _ -> k
    | Macro (_,_) -> assert false
    | Align _ -> k

  let pseudo_exists p = pseudo_fold (fun b i -> b || p i) false
  let pseudo_dump dump = pseudo_fold (fun  _ i -> dump i) ""
  let pseudo_iter f ins = pseudo_fold (fun () ins -> f ins) () ins

  let fold_pseudo_code f = List.fold_left (pseudo_fold f)
  let exists_pseudo_code p = List.exists (pseudo_exists p)

(* Fold/Map over labels *)

  let rec fold_labels f k ins = match ins with
  | Nop -> k
  | Instruction ins -> I.fold_labels k f ins
  | Label (lbl,ins) -> fold_labels f (f k lbl) ins
  | Symbolic _ -> k
  | Macro _ -> assert false
  | Align _ -> k

  let map_labels_base = I.map_labels

  let map_label_ins f =
    let f =
      let open BranchTarget in
      function
      | Lbl lbl -> Lbl (f lbl)
      | Offset _ as o -> o in
    I.map_labels f


  let rec map_labels f ins = match ins with
  | Nop -> Nop
  | Instruction ins -> Instruction (map_label_ins f ins)
  | Label (lbl,ins) -> Label (f lbl, map_labels f ins)
  | Symbolic s -> Symbolic s
  | Macro _ -> assert false
  | Align n -> Align n

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

  let code_exists p =
    let rec exists = function
      | [] -> false
      | ins::code -> pseudo_exists p ins || exists code in
    exists

(* Group code by proc *)
  let code_by_proc prog =
    let m =
      let open MiscParser in
      List.fold_left
        (fun m (p,code) ->
          let np = proc_num p in
          let (c,f) =
            IntMap.safe_find ([],[]) np m in
          let v =
            match proc_func p with
            | Main -> code,f
            | FaultHandler -> c,code in
          IntMap.add np v m)
        IntMap.empty
        prog in
    let code = IntMap.fold (fun p v k -> (p,v)::k) m [] in
    (* Keep threads in increasing id order, although it should not matter. *)
    List.rev code

(* Extract instructions pointed by label set *)

  let rec add_next_instr m addr lbl code k =
    match code with
    | [] -> k
    | (Nop|Label (_,Nop))::code -> add_next_instr m addr lbl code k
    | (Label (_,Instruction i)|Instruction i)::_ ->
       let f lbl =
         let open BranchTarget in
         match lbl with
         | Offset _ -> lbl
         | Lbl s ->
            try
              let tgt_addr = Label.Map.find s m in
              Offset (tgt_addr-addr)
            with Not_found -> lbl in
       (lbl,I.map_labels f i)::k
    | Label (_,i)::code -> add_next_instr m addr lbl (i::code) k
    | (Symbolic _|Macro _)::_ -> assert false
    | (Align _)::_ -> assert false


  let fold_label_addr f =

    let rec fold_ins m addr i code =
      match i with
      | Label (lbl,ins) ->
         fold_ins (f lbl addr m) addr ins code
      | Instruction ins ->
         fold_rec m (addr+I.size_of_ins ins) code
      | Nop ->
         fold_rec m addr code
      | Macro _|Symbolic _ -> assert false
      | Align _ -> fold_rec m addr code

      and fold_rec m addr = function
        | [] -> m
        | i::rem -> fold_ins m addr i rem in

    fold_rec

  let mk_label_map m addr code = fold_label_addr Label.Map.add m addr code

  let from_labels_code lbls p c f k =
    let m = mk_label_map Label.Map.empty 0 c in
    let m = mk_label_map m func_size f in
    let rec do_rec addr code k =
      match code with
      | [] -> k
      | Label (lbl,pseudoi)::rem ->
          let full_lbl = (p,lbl) in
          let k =
           if Label.Full.Set.mem full_lbl lbls then
             add_next_instr m addr full_lbl code k
           else k in
          do_rec addr (pseudoi::rem) k
      | Instruction ins::rem ->
         do_rec (addr+I.size_of_ins ins)  rem k
      | Nop::rem -> do_rec addr rem k
      | (Macro _|Symbolic _)::_ -> assert false 
      | (Align _)::_ -> assert false in
    do_rec 0 c k |> do_rec func_size f


  let from_labels lbls prog =
    if Label.Full.Set.is_empty lbls then []
    else
      let prog = code_by_proc prog in
      List.fold_left
        (fun k (p,(c,f)) ->
          from_labels_code lbls p c f k)
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

  let size_of_ins = I.size_of_ins

end
