(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Config = struct
  let naturalsize = MachSize.Word
  let moreedges = false
  let fullmixed = false
  let variant _ = false
end

module Make
    (C:sig
      val naturalsize : MachSize.sz
      val moreedges : bool
      val fullmixed : bool
      val variant : Variant_gen.t -> bool
    end) = struct

let do_self = C.variant Variant_gen.Self
let do_tag = C.variant Variant_gen.MemTag
let do_morello = C.variant Variant_gen.Morello
let do_kvm = C.variant Variant_gen.KVM
let do_neon = C.variant Variant_gen.Neon
let do_sve = C.variant Variant_gen.SVE
let do_sme = C.variant Variant_gen.SME
let do_mixed = Variant_gen.is_mixed  C.variant
let do_cu = C.variant Variant_gen.ConstrainedUnpredictable

open Code
open Printf

include MakeAArch64Base.Make(struct let is_morello = do_morello end)

(* Little endian *)
let tr_endian = Misc.identity

module ScopeGen = ScopeGen.NoGen

(* AArch64 has more atoms that others *)
let bellatom = false
module SIMD = struct

  type atom = SmV|SmH
             |SvV|Sv1|Sv2i|Sv3i|Sv4i
             |NeP|NeAcqPc|NeRel|Ne1|Ne2|Ne3|Ne4|Ne2i|Ne3i|Ne4i|NePa|NePaN

  let fold_neon f r = r |>
    f NeAcqPc |> f NeRel |>
    f NeP |>
    f NePa |> f NePaN |>
    f Ne1 |> f Ne2 |> f Ne3 |> f Ne4 |>
    f Ne2i |> f Ne3i |> f Ne4i

  let fold_sve f r = r |>
    f SvV  |> f Sv1 |>
    f Sv2i |> f Sv3i |> f Sv4i

  let fold_sme f r = r |>
    f SmV  |> f SmH

  let nregs = function
    | SmV | SmH
    | SvV | Sv1 | Ne1 -> 1
    | Sv2i | Ne2 | Ne2i -> 2
    | Sv3i | Ne3 | Ne3i -> 3
    | Sv4i | Ne4 | Ne4i -> 4
    | _ -> 1

  let nelements = function
    | SmV|SmH
    | SvV|Sv1|Sv2i|Sv3i|Sv4i
    | Ne1|Ne2|Ne2i|Ne3|Ne3i|Ne4|Ne4i -> 4
    | NePa|NePaN -> 2
    | NeP | NeAcqPc | NeRel -> 1

  let pp_opt = function
    | Sv2i | Sv3i | Sv4i
    | Ne2i | Ne3i | Ne4i -> "i"
    | _ -> ""

  let pp n =
    match n with
    | Ne1 | Ne2 | Ne3 | Ne4 | Ne2i | Ne3i | Ne4i ->
       Printf.sprintf "Ne%i%s" (nregs n) (pp_opt n)
    | Sv1 | Sv2i | Sv3i | Sv4i ->
       Printf.sprintf "Sv%i%s" (nregs n) (pp_opt n)
    | SmV -> "SmV"
    | SmH-> "SmH"
    | SvV -> "SvV"
    | NePa -> "NePa"
    | NePaN -> "NePaN"
    | NeP -> "NeP"
    | NeAcqPc -> "NeQ"
    | NeRel -> "NeL"

  let initial sz =
    let sz = if sz <= 0 then 1 else sz in
    Array.make sz 0

  let step n start v =
    let start = start+1 in
    let el = nelements n in
    let sz = nregs n in
    let v = Array.copy v in
    for k = 0 to sz-1 do
      for i=0 to el-1 do
        let j = match n with
          | SmV |Sv2i | Sv3i | Sv4i | Ne2i | Ne3i | Ne4i -> k+i*sz
          | NeP | NeAcqPc | NeRel | NePa | NePaN
          | Ne1 | Ne2 | Ne3 | Ne4
          | SmH | SvV | Sv1 -> i+k*el
        in
       v.(j) <- start+k
      done
    done ;
    v

  let read n v =
    let el = nelements n in
    let sz = nregs n in
    let access r k = match n with
      | SmV | Sv2i | Sv3i | Sv4i | Ne2i | Ne3i | Ne4i -> sz*k + r
      | NeP | NeAcqPc | NeRel | NePa | NePaN
      | Ne1 | Ne2 | Ne3 | Ne4
      | SmH | SvV | Sv1 -> el*r + k
    in
    let rec reg r k =
      if k >= el then []
      else v.(access r k)::reg r (k+1) in
    let rec regs r =
      if r >= sz then []
      else reg r 0::regs (r+1) in
    regs 0

  let reduce vec =
    List.fold_right (+) (List.flatten vec) 0
end

type atom_rw =  PP | PL | AP | AL
type capa = Capability
type capa_opt = capa option

module WPTE = struct

  type pte_field = AF | DB | DBM | VALID
  let all_pte_field = [AF; DB; DBM; VALID;]
  let pp_pte_field = function
    | AF -> "AF"
    | DB -> "DB"
    | DBM -> "DBM"
    | VALID -> "V"

          (* Toggle the value between 0 and 1 *)
  type t = OA
          (* Precise value of 0 to 1 *)
          | One of pte_field
          (* Precise value of 1 to 0 *)
          | Zero of pte_field
          | HA
          | HD

  let all =
    OA :: HA :: HD ::
    ( List.map ( fun field -> [One field; Zero field;] ) all_pte_field
      |> List.flatten )
  let compare = compare
  let pp = function
    | OA -> "OA"
    | One p -> ( pp_pte_field p ) ^ "1"
    | Zero p -> ( pp_pte_field p ) ^ "0"
    | HA -> "HA"
    | HD -> "HD"
end

module WPTESet = MySet.Make(WPTE)

(* check if set only contain `HA` and `HD` *)
let contain_valid_tthm_fields set =
  let open WPTE in
  ( WPTESet.remove HD set
  |> WPTESet.remove HA
  |> WPTESet.cardinal ) = 0

(* Check the `set` contains valid pte fields.
   - NO `HA` or `HD`
   - fields must NOT conflict,
     for example, two different `VALID`, i.e. `V1` and `V0` *)
let contain_valid_pte_fields set =
  let open WPTE in
  not @@ WPTESet.mem HD set
  && not @@ WPTESet.mem HA set
  (* Convert the set to all `One` and check size equal *)
  && WPTESet.cardinal set =
  ( WPTESet.map ( fun field ->
    match field with
    | One p | Zero p -> One p
    | p -> p
    ) set |> WPTESet.cardinal )

type atom_pte =
  | Read|ReadAcq|ReadAcqPc
  | Set of WPTESet.t
  | SetRel of WPTESet.t
  (* Special `Acq` and `AcqPc` read case for `HA`
     Note that the plain read for `HA` share the
     same internal data structure as `Set of WPTESet.t`.
     Due to  `diy` parsing limitation, it is impossible
     to introduce `PteHA` to different internal
     representation. *)
  | ReadHAAcq | ReadHAAcqPc

let pp_w_pte ws = WPTESet.pp_str "." WPTE.pp ws

let pp_atom_pte = function
  | Read -> ""
  | ReadAcq -> "A"
  | ReadAcqPc -> "Q"
  | ReadHAAcq -> "HAA"
  | ReadHAAcqPc -> "HAQ"
  | Set set -> pp_w_pte set
  | SetRel set -> pp_w_pte set ^"L"

type neon_opt = SIMD.atom

type pair_idx = UnspecLoc

type atom_acc =
  | Plain of capa_opt | Acq of capa_opt | AcqPc of capa_opt | Rel of capa_opt
  | Atomic of atom_rw | Tag | CapaTag | CapaSeal | Pte of atom_pte | Neon of neon_opt
  | Pair of pair_opt * pair_idx | Instr

let  plain = Plain None

type atom = atom_acc * MachMixed.t option

module Value = struct

  include Value_gen.Make(struct
    type nonrec atom = atom
    type pte = AArch64PteVal.t
    let pp_pte = AArch64PteVal.pp_v
    let default_pte = AArch64PteVal.default
    let pte_compare = AArch64PteVal.compare

    let toggle_pte_field field pteval loc =
      let open AArch64PteVal in
      let open WPTE in
      match field with
      | One AF | Zero AF | HA -> { pteval with af = 1-pteval.af; }
      | One DB | Zero DB | HD -> { pteval with db = 1-pteval.db; }
      | One DBM | Zero DBM -> { pteval with dbm = 1-pteval.dbm; }
      | One VALID | Zero VALID -> { pteval with valid = 1-pteval.valid; }
      | OA -> { pteval with oa=OutputAddress.PHY (loc ()); }

    (* toggle or flip the value of pte field *)
    let toggle_pte flag_set pteval loc =
      WPTESet.fold (fun f p ->
        toggle_pte_field f p loc
      ) flag_set pteval

    (* Decide the initial pte value for location `loc`
       and align up with the atom_pte_list *)
    let init_pte loc pte_atom_list =
      let open WPTE in
      let default_pte_loc = default_pte loc in
      let pte_atom_list = List.filter_map
        ( fun (atom, _mach_size) -> match atom with
          | Pte(pte_atom) -> Some(pte_atom)
          | _ -> None
        ) pte_atom_list in
      (* A dummy function that return the default physical address `*` *)
      let loc_fun () = "*" in
      (* TODO: Check if the `field` in `pteval` is of `value`.
         Upon mismatching  `value`, the initial value needs to change. *)
      let precise_set_field field (af,db,dbm,valid,pteval) =
        let open AArch64PteVal in
        (* Helper function to check if the `field` in `pteval` is of `value`.
          Upon mismatching  `value`, the initial value needs to change. *)
        let flip_field field value (af,db,dbm,valid,pteval) =
          match field with
          | AF -> begin match value = pteval.af,af with
            (* Either carry the previous `af` or set `af` to Some false,
               i.e. no need to flip the initial value of `af` *)
            | true,_ -> (Some (Option.value ~default:false af),db,dbm,valid,pteval)
            (* Flip the initial `af` *)
            | false,None -> (Some true,db,dbm,valid,{pteval with af = value})
            (* Value collide, invalid cycle/anotation specification *)
            | false,Some _ -> Warn.user_error "Fail to set AF."
          end
          | DB -> begin match value = pteval.db,db with
            | true,_ -> (af,Some (Option.value ~default:false db),dbm,valid,pteval)
            | false,None -> (af,Some true,dbm,valid,{pteval with db = value})
            | false,Some _ -> Warn.user_error "Fail to set DB."
          end
          | DBM -> begin match value = pteval.dbm, dbm with
            | true,_ -> (af,db,Some (Option.value ~default:false dbm),valid,pteval)
            | false,None -> (af,db,Some true,valid,{pteval with dbm = value})
            | false,Some _ -> Warn.user_error "Fail to set DBM."
          end
          | VALID -> begin match value = pteval.valid,valid with
            | true,_ -> (af,db,dbm,Some (Option.value ~default:false valid),pteval)
            | false,None -> (af,db,dbm,Some true,{pteval with valid = value})
            | false,Some _ -> Warn.user_error "Fail to set VALID."
          end in
        let acc = (af,db,dbm,valid,toggle_pte_field field pteval loc_fun) in
        match field with
          | OA -> acc
          | One pte_field -> flip_field pte_field 1 acc
          | Zero pte_field -> flip_field pte_field 0 acc
          | HA ->
            let expected_af = not (default_pte_loc.af = 0) in
            let init_af = Option.value ~default:expected_af af in
            let new_pteval = {pteval with af = 1} in
            if init_af then (Some expected_af,db,dbm,valid,new_pteval)
            else Warn.user_error "Fail to set AF in TTHM=HA."
          | HD ->
            let expected_db = not (default_pte_loc.db = 0) in
            let init_db = Option.value ~default:expected_db db in
            let expected_dbm = not (default_pte_loc.dbm = 1) in
            let init_dbm = Option.value ~default:expected_dbm dbm in
            let new_pteval = {pteval with db = 1} in
            begin match init_db,init_dbm with
              | true,true -> (af, Some expected_db,Some expected_dbm,valid,new_pteval)
              | _ -> Warn.user_error "Fail to set DB and DBM in TTHM=HD."
            end
      in
      (* The entire process decides if we want to flip the initial value of fields.
         Field `valid,af,db,dbm` in accumulator `acc` track if the default
         value is (not) needed to be flipped.
         - None, all good,
         - Some true, must flip
         - Some false must not flip
         Conflict initial values cause, i.e. Some true and Some false, warning.
         The final `pteval` should be throw away as of no meaning. *)
      let (af,db,dbm,valid,_) =
        List.fold_left ( fun acc atom_pte ->
          (* Toggle values for further process *)
          match atom_pte with
          | Set(field_set)|SetRel(field_set) -> WPTESet.fold precise_set_field field_set acc
          | ReadHAAcq | ReadHAAcqPc -> precise_set_field HA acc
          | _ -> acc
        ) (None,None,None,None,default_pte_loc) pte_atom_list in
      (* Create a new WPTESet to adjust the inital value.
         Collapse None to false as it means no need to change default value *)
      let adjust_value =
        let value_false = Option.value ~default:false in
        WPTESet.empty
        |> (if value_false af then WPTESet.add (One AF) else Fun.id)
        |> (if value_false db then WPTESet.add (One DB) else Fun.id)
        |> (if value_false dbm then WPTESet.add (One DBM) else Fun.id)
        |> (if value_false valid then WPTESet.add (One VALID) else Fun.id) in
      toggle_pte adjust_value default_pte_loc loc_fun

    let do_setpteval flags pte loc =
      let open WPTE in
      match flags with
        | Set f|SetRel f when WPTESet.mem HA f || WPTESet.mem HD f ->
          Warn.user_error "Atom `HD` or `HA` is not a pteval write"
        | Set f|SetRel f -> toggle_pte f pte loc
        | Read|ReadAcq|ReadAcqPc ->
          Warn.user_error "Atom `Read|ReadAcq|ReadAcqPc` is not a pteval write"
        | ReadHAAcq | ReadHAAcqPc ->
          Warn.user_error "Atom `HA` is not a pteval write"

    let set_pteval a p =
      match a with
      | Pte f,None -> do_setpteval f p
      | _ -> Warn.user_error "Atom is not a pteval write"

    let can_fault dir pte_val =
      let open AArch64PteVal in
      pte_val.valid = 0 || pte_val.af = 0 || (dir = Code.W && pte_val.db = 0)

    (* check if an pte annotation `pte` will affect a pte `field` *)
    let affect_pte_field field pte =
      let open WPTE in
      match pte with
      | Read | ReadAcq | ReadAcqPc -> false
      | ReadHAAcq | ReadHAAcqPc -> field = AF
      | Set pte_fields | SetRel pte_fields ->
        WPTESet.mem (One field) pte_fields
        || WPTESet.mem (Zero field) pte_fields
        (* special case for `HD` and `HA` *)
        || (field = AF && WPTESet.mem HA pte_fields)
        || (field = DB && WPTESet.mem HD pte_fields)

    let need_check_fault atom =
      let open WPTE in
      match atom with
      | Some (Pte pte, None)
        when (affect_pte_field AF pte || affect_pte_field VALID pte) -> Irr
      | Some (Pte pte, None)
        when affect_pte_field DB pte -> Dir W
      | _ -> NoDir

    let implicitly_set_pteval dir machine_feature p =
      let open WPTE in
      let open AArch64PteVal in
      if StringSet.mem (WPTE.pp HA) machine_feature && p.af = 0 then
          Some (Irr,{p with af = 1})
      else if StringSet.mem (WPTE.pp HD) machine_feature
        && dir = Code.W && p.db = 0  && p.dbm = 1 then
          Some (Dir W,{p with db = 1})
      else None

    let refers_virtual p = OutputAddress.refers_virtual p.AArch64PteVal.oa
  end)

  let from_pte p = PteValue p
  let to_pte = function
    | PteValue p -> p
    | _ -> Warn.user_error "Cannot convert to pte"
end

(* Mixed size *)
module Mixed =
  MachMixed.Make
    (struct
      let naturalsize = Some C.naturalsize
      let fullmixed = C.fullmixed
    end)(Value)

let default_atom = Atomic PP,None
let instr_atom = Some (Instr,None)

let applies_atom (a,_) d =
  let open WPTE in
  match a,d with
  | Neon SIMD.NeAcqPc,W
  | Neon SIMD.NeRel,R -> false
  | Acq _,R
  | AcqPc _,R
  | Rel _,W
  | Pte (Read|ReadAcq|ReadAcqPc),R
  | Instr, R
  | (Plain _|Atomic _|Tag|CapaTag|CapaSeal|Neon _|Pair _),(R|W)
    -> true
  (* special case for TTHM HA for read *)
  | Pte (Set p),R when WPTESet.mem HA p -> true
  | Pte (ReadHAAcq|ReadHAAcqPc),R -> true
  | Pte (Set _|SetRel _),W -> true
  | _ -> false

let is_ifetch a = match a with
| Some (Instr,_) -> true
| _ -> false

let is_tthm fields =
  let open WPTE in
    WPTESet.mem HD fields || WPTESet.mem HA fields

   let pp_plain = "P"
(* Annotation A is taken by load aquire *)
   let pp_as_a = None

   let pp_atom_rw = function
     | PP -> ""
     | PL -> "L"
     | AP -> "A"
     | AL -> "AL"

   let pp_opt = function
     | None -> ""
     | Some Capability -> "c"

   let pp_pair_opt = function
     | Pa -> ""
     | PaN -> "N"
     | PaI -> "I"

   and pp_pair_idx = function
     | UnspecLoc -> ""

   let pp_atom_acc = function
     | Atomic rw -> sprintf "X%s" (pp_atom_rw rw)
     | Rel o -> sprintf "L%s" (pp_opt o)
     | Acq o -> sprintf "A%s" (pp_opt o)
     | AcqPc o -> sprintf "Q%s" (pp_opt o)
     | Plain o -> sprintf "P%s" (pp_opt o)
     | Tag -> "T"
     | CapaTag -> "Ct"
     | CapaSeal -> "Cs"
     | Pte p -> sprintf "Pte%s" (pp_atom_pte p)
     | Neon n -> SIMD.pp n
     | Pair (opt,idx)
       -> sprintf "Pa%s%s" (pp_pair_opt opt) (pp_pair_idx idx)
     | Instr -> "I"

   let pp_atom (a,m) = match a with
   | Plain o ->
      let prefix = match o with
      | None -> ""
      | Some Capability -> "Pc" in
       begin
         match m with
         | None -> prefix
         | Some m ->
            if String.length prefix > 0
            then sprintf "%s.%s" prefix (Mixed.pp_mixed m)
            else Mixed.pp_mixed m
       end
   | _ ->
     let pp_acc = pp_atom_acc a in
     match m with
     | None -> pp_acc
     | Some m -> sprintf "%s.%s" pp_acc  (Mixed.pp_mixed m)

   let compare_atom = compare

   let equal_atom a1 a2 = a1 = a2

   include
     MachMixed.Util
       (struct
         type at = atom_acc
         let plain = plain
       end)

   let fold_mixed f r =
     if do_mixed then
       Mixed.fold_mixed
         (fun m r -> f (Plain None,Some m) r)
         r
     else
       r

   let fold_pte f r =
     if do_kvm then
       let open WPTE in
       let fold_singleton_wpte f r =
         List.fold_left (fun acc pte -> f (WPTESet.singleton pte) acc) r WPTE.all in
       let fold_pte_set fs r = r |> f (SetRel fs) |> f (Set fs) in
       r |> fold_singleton_wpte fold_pte_set |> f Read |> f ReadAcq |> f ReadAcqPc
         |> f ReadHAAcq |> f ReadHAAcqPc
     else r

   let fold_atom_rw f r = f PP (f PL (f AP (f AL r)))

   let fold_tag =
     if do_tag then fun f r -> f Tag r
     else fun _f r -> r

   let fold_morello =
     if do_morello then fun f r -> f CapaSeal (f CapaTag r)
     else fun _f r -> r

   let fold_neon =
     if do_neon then
       fun f -> SIMD.fold_neon (fun n -> f (Neon n))
     else
       fun _ r -> r

   let fold_sve =
     if do_sve then
       fun f -> SIMD.fold_sve (fun n -> f (Neon n))
     else
       fun _ r -> r

   let fold_sme =
     if do_sme then
       fun f -> SIMD.fold_sme (fun n -> f (Neon n))
     else
       fun _ r -> r

      let fold_pair f r =
        if do_mixed then r
        else
          let f opt idx r =
            f (Pair (opt,idx)) r in
          r |>
          f Pa UnspecLoc |>
          f PaN UnspecLoc |>
          f PaI UnspecLoc

      let fold_acc_opt o f r =
        let r = f (Acq o) r in
        let r = f (AcqPc o) r in
        let r = f (Rel o) r in
        r

   let fold_self f r = if do_self then f Instr r else r

   let fold_acc mixed f r =
     let r = if mixed then r else fold_pte (fun p r -> f (Pte p) r) r in
     let r = fold_morello f r in
     let r = fold_tag f r in
     let r = fold_neon f r in
     let r = fold_sve f r in
     let r = fold_sme f r in
     let r = fold_pair f r in
     let r = fold_acc_opt None f r in
     let r = fold_self f r in
     let r =
       if do_morello then
         let r = f (Plain (Some Capability)) r in
         let r = fold_acc_opt (Some Capability) f r in
         r
       else r in
     let r = fold_atom_rw (fun rw -> f (Atomic rw)) r in
     r

   let fold_non_mixed f r = fold_acc false (fun acc r -> f (acc,None) r) r

   let fold_atom f r =
     let r = fold_non_mixed f r in
     if do_mixed then
       fold_acc true
         (fun acc r -> Mixed.fold_mixed (fun m r -> f (acc,Some m) r) r)
         (Mixed.fold_mixed
            (fun m r -> f (Plain None,Some m) r)
            r)
     else r

   let worth_final (a,_) = match a with
     | Atomic _ -> true
     | Acq _|AcqPc _|Rel _|Plain _|Tag|Instr
     | CapaTag|CapaSeal
     | Pte _|Neon _
     | Pair _
       -> false



   let varatom_dir _d f r = f None r

   let merge_atoms a1 a2 =
   let open WPTE in
   match a1,a2 with
(* Plain and Instr do not merge *)
   | ((Plain _,_),(Instr,_))
   | ((Instr,_),(Plain _,_)) ->
       None
(* Eat Plain *)
   | ((Plain None,None),a)
   | (a,(Plain None,None)) ->
       Some a
(* Add size to ordinary annotations *)
   | ((Plain None,(Some _ as sz)),
      ((Acq None|AcqPc None|Rel None|Atomic _ as a),None))
   | (((Acq None|AcqPc None|Rel None|Atomic _ as a),None),
      (Plain None,(Some _ as sz)))
     -> Some (a,sz)
(* No sizes for Pte and tags *)
   | (((Pte _|Tag),_),(_,Some _))
   | ((_,Some _),((Pte _|Tag),_)) ->
       None
(* Merge Pte *)
   | ((Pte (Read|ReadAcq),None),((Pte ReadAcq|Acq None),None))
   | (((Acq None|Pte ReadAcq),None),(Pte (Read|ReadAcq),None))
       -> Some (Pte ReadAcq,None)
   | ((Pte (Read|ReadAcqPc),None),((Pte ReadAcqPc|AcqPc None),None))
   | (((Pte ReadAcqPc|AcqPc None),None),(Pte (Read|ReadAcqPc),None))
       -> Some (Pte ReadAcqPc,None)
   (* A few special cases for TTHM HA on read *)
   | ((Pte (Set p),None),((Pte ReadHAAcq|Acq None),None))
   | (((Acq None|Pte ReadHAAcq),None),(Pte (Set p),None))
     when p = WPTESet.singleton HA
       -> Some (Pte ReadHAAcq,None)
   | ((Pte ReadHAAcq,None),((Pte ReadHAAcq|Acq None),None))
   | ((Acq None,None),(Pte ReadHAAcq,None))
       -> Some (Pte ReadHAAcq,None)
   | ((Pte (Set p),None),((Pte ReadHAAcqPc|AcqPc None),None))
   | (((Pte ReadHAAcqPc|AcqPc None),None),(Pte (Set p),None))
     when p = WPTESet.singleton HA
       -> Some (Pte ReadHAAcqPc,None)
   | ((Pte ReadHAAcqPc,None),((Pte ReadHAAcqPc|AcqPc None),None))
   | ((AcqPc None,None),(Pte ReadHAAcqPc,None))
       -> Some (Pte ReadHAAcqPc,None)
   (* END special cases for TTHM HA on read *)
   | ((Pte (Set set|SetRel set),None),(Rel None,None))
   | ((Rel None,None),(Pte (Set set|SetRel set),None))
       -> Some (Pte (SetRel set),None)
   | (Pte (Set set1),None),(Pte (Set set2),None)
     -> let set = WPTESet.union set1 set2 in
        if contain_valid_pte_fields set
          || contain_valid_tthm_fields set
        then Some (Pte (Set set),None)
        else None
   | ((Pte (Set set1),None),(Pte (SetRel set2),None))
   | ((Pte (SetRel set1),None),(Pte (Set set2),None))
   | ((Pte (SetRel set1),None),(Pte (SetRel set2),None))
     -> let set = WPTESet.union set1 set2 in
        if contain_valid_pte_fields set
          || contain_valid_tthm_fields set
        then Some (Pte (SetRel set),None)
        else None
(* Add size when (ordinary) annotation equal *)
   | ((Acq None as a,None),(Acq None,(Some _ as sz)))
   | ((Acq None as a,(Some _ as sz)),(Acq None,None))
   | ((AcqPc None as a,None),(AcqPc None,(Some _ as sz)))
   | ((AcqPc None as a,(Some _ as sz)),(AcqPc None,None))
   | ((Rel None as a,None),(Rel None,(Some _ as sz)))
   | ((Rel None as a,(Some _ as sz)),(Rel None,None))
   | ((Atomic PP as a,None),(Atomic PP,(Some _ as sz)))
   | ((Atomic PP as a,(Some _ as sz)),(Atomic PP,None))
   | ((Atomic AP as a,None),(Atomic AP,(Some _ as sz)))
   | ((Atomic AP as a,(Some _ as sz)),(Atomic AP,None))
   | ((Atomic PL as a,None),(Atomic PL,(Some _ as sz)))
   | ((Atomic PL as a,(Some _ as sz)),(Atomic PL,None))
   | ((Atomic AL as a,None),(Atomic AL,(Some _ as sz)))
   | ((Atomic AL as a,(Some _ as sz)),(Atomic AL,None))
     -> Some (a,sz)
(* Remove plain when size equal *)
   | ((Plain None,sz1),(a,sz2))
   | ((a,sz1),(Plain None,sz2)) when sz1=sz2 -> Some (a,sz1)
   | _,_ ->
       if equal_atom a1 a2 then Some a1 else None

   let overlap_atoms a1 a2 = match a1,a2 with
     | ((_,None),(_,_))|((_,_),(_,None)) -> true
     | ((_,Some sz1),(_,Some sz2)) ->
         MachMixed.overlap  sz1 sz2

   let neon_as_integers =
     let open SIMD in
     function
     | NeP | NeAcqPc | NeRel -> 1
     | NePa | NePaN -> 2
     | SmV | SmH
     | SvV | Sv1  | Ne1 -> 4
     | Sv2i | Ne2 | Ne2i -> 8
     | Sv3i | Ne3 | Ne3i -> 12
     | Sv4i | Ne4 | Ne4i -> 16

   let atom_to_bank = function
   | Tag,None -> Code.Tag
   (* TTHM feature only apply to ordinary R/W *)
   | Pte (Set p|SetRel p),None when is_tthm p -> Code.Ord
   | Pte (ReadHAAcq|ReadHAAcqPc),None -> Code.Ord
   | Pte _,None -> Code.Pte
   | CapaTag,None -> Code.CapaTag
   | CapaSeal,None -> Code.CapaSeal
   | Neon n,None -> Code.VecReg n
   | Pair (_,UnspecLoc),_ -> Code.Pair
   | Instr,_ -> Code.Instr
   | (Tag|CapaTag|CapaSeal|Pte _|Neon _),Some _ -> assert false
   | (Plain _|Acq _|AcqPc _|Rel _|Atomic _),_
     -> Code.Ord


(**************)
(* Mixed size *)
(**************)

   let tr_value ao v = match ao with
   | None| Some (_,None) -> v
   | Some (_,Some (sz,_)) -> Mixed.tr_value sz v

   module ValsMixed =
     MachMixed.Vals
       (struct
         let naturalsize () = C.naturalsize
         let endian = endian
       end)(Value)

let overwrite_value v ao w = match ao with
| None
| Some
    ((Atomic _|Acq _|AcqPc _|Rel _|Plain _|
    Tag|CapaTag|CapaSeal|Pte _|Neon _|Pair _|Instr),None)
  -> w (* total overwrite *)
| Some ((Atomic _|Acq _|AcqPc _|Rel _|Plain _|Neon _|Instr),Some (sz,o)) ->
   ValsMixed.overwrite_value v sz o w
| Some ((Tag|CapaTag|CapaSeal|Pte _|Pair _),Some _) ->
    assert false

 let extract_value v ao = match ao with
  | None
  | Some
      ((Atomic _|Acq _|AcqPc _|Rel _|Plain _
        |Tag|CapaTag|CapaSeal|Pte _|Neon _|Pair _|Instr),None) -> v
  | Some ((Atomic _|Acq _|AcqPc _|Rel _|Plain _|Tag|CapaTag|CapaSeal|Neon _),Some (sz,o)) ->
     ValsMixed.extract_value v sz o
  | Some ((Pte _|Pair _|Instr),Some _) -> assert false

(* Wide accesses *)

   let as_integers a =
     Misc.seq_opt
       (function
        | Neon n,_ -> (match neon_as_integers n with
                       | 1 -> None
                       | n -> Some n)
        | Pair _,_ -> Some 2
        | _ -> None)
       a

   let is_pair a =
     match a with
     | Some (Pair _,_) -> true
     | Some _|None -> false

  let get_machine_feature atom =
    let open WPTE in
    match atom with
    | Some(Pte(Set pte|SetRel pte), _) ->
      WPTESet.fold (fun f acc ->
        match f with
        | HA -> StringSet.add (WPTE.pp HA) acc
        | HD -> StringSet.add (WPTE.pp HD) acc
        | _ -> acc
      ) pte StringSet.empty
    | Some(Pte(ReadHAAcq|ReadHAAcqPc), _) ->
      StringSet.singleton (WPTE.pp HA)
    | _ -> StringSet.empty

(* End of atoms *)

(**********)
(* Fences *)
(**********)

type strength = Strong | Weak
let fold_strength f r = f Strong (f Weak r)
let fold_dirloc f r = f Next (f Prev r)
type fence = | Barrier of barrier | CacheSync of strength * bool
             | ShootdownSync of mBReqDomain * TLBI.op
             | ShootdownNoSync of TLBI.op
             | CMO of syncType * dirloc

let is_isync = function
  | Barrier ISB -> true
  | _ -> false

let compare_fence = compare

let default = Barrier (DMB (SY,FULL))
let strong = default

let add_dot f x = match f x with
| "" -> ""
| s -> "." ^ s

let pp_fence f = match f with
| Barrier f -> do_pp_barrier "." f
| CacheSync (s,isb) ->
   sprintf "CacheSync%s%s"
     (match s with Strong -> "Strong" | Weak -> "")
     (if isb then "Isb" else "")
| ShootdownSync (d,op) ->
   sprintf "TLBI-sync%s%s"
     (add_dot TLBI.short_pp_op op)
     (add_dot pp_domain d)
| ShootdownNoSync (op) ->
   sprintf "TLBI%s"
     (add_dot TLBI.short_pp_op op)
| CMO (t,loc) ->
  sprintf "%s%s"
    (match t with DC_CVAU -> "DC.CVAU" | IC_IVAU -> "IC.IVAU")
    (match loc with Prev -> "p"| Next -> "n")

let fold_cumul_fences f k =
   do_fold_dmb_dsb (fun b k -> f (Barrier b) k) k

let fold_shootdown f acc =
  if not do_kvm then acc
  else
    let fold_domain =
      if C.moreedges then fold_domain
      else fun f k -> f ISH k
    and fold_op =
      if C.moreedges then TLBI.full_fold_op
      else TLBI.fold_op in
    acc
    |> fold_op ( fun op -> f (ShootdownNoSync(op)) )
    |> fold_op ( fun op ->
      fold_domain ( fun domain -> f (ShootdownSync(domain,op)) ) )

let fold_cachesync =
  if do_self then
    fun f ->
      Misc.fold_bool
        (fun b k -> fold_strength (fun s k -> f (CacheSync (s,b)) k) k)
  else fun _ k -> k


let fold_cmo f k =
  if do_self then
    fold_dirloc (fun d k -> f (CMO (DC_CVAU,d)) (f (CMO (IC_IVAU,d)) k)) k
  else k


let fold_all_fences f k =
  let k = fold_shootdown f k in
  let k = fold_cachesync f k in
  let k = fold_cmo f k in
  fold_barrier (fun b k -> f (Barrier b) k) k


let fold_some_fences f k =
  let f = fun b k -> f (Barrier b) k in
  let k = f ISB k  in
  let k = f (DMB (SY,FULL)) k in
  let k = f (DMB (SY,ST)) k in
  let k = f (DMB (SY,LD)) k in
  k

let orders f d1 d2 = match f,d1,d2 with
| Barrier ISB,_,_ -> false
| Barrier (DSB (_,FULL)|DMB (_,FULL)),_,_ -> true
| Barrier (DSB (_,ST)|DMB (_,ST)),W,W -> true
| Barrier (DSB (_,ST)|DMB (_,ST)),_,_ -> false
| Barrier (DSB (_,LD)|DMB (_,LD)),Code.R,(W|Code.R) -> true
| Barrier (DSB (_,LD)|DMB (_,LD)),_,_ -> false
| CacheSync _,_,_ -> true
| ShootdownSync _,_,_ -> false
| ShootdownNoSync _,_,_ -> false
| CMO _,_,_ -> true

let var_fence f r = f default r

(********)
(* Deps *)
(********)

module D = Dep

type csel = OkCsel|NoCsel

type dp = D.dp * csel

let fold_dpr f r =
  D.fold_dpr
    (fun d r -> f (d,NoCsel) (f (d,OkCsel) r))
    r
let fold_dpw f r =
  D.fold_dpw
    (fun d r -> f (d,NoCsel) (f (d,OkCsel) r))
    r

let pp_ddp =
  let open D in
  function
  | ADDR -> "Addr"
  | DATA -> "Data"
  | CTRL -> "Ctrl"
  | CTRLISYNC -> "CtrlIsb"

let pp_dp (d,c) = match c with
  | NoCsel ->  pp_ddp d
  | OkCsel -> pp_ddp d^"Csel"

let lift_dd = Misc.app_opt (fun d -> d,NoCsel)
let ddr_default = lift_dd D.ddr_default
let ddw_default = lift_dd D.ddw_default
let ctrlr_default = lift_dd  D.ctrlr_default
let ctrlw_default = lift_dd  D.ctrlw_default

let lift_pred p (d,_) = p d
let is_ctrlr dc = lift_pred D.is_ctrlr dc
let is_addr dc = lift_pred D.is_addr dc

let fst_dp (d,c) = match c with
  | NoCsel -> List.map (fun d -> (d,NoCsel)) (D.fst_dp d)
  | OkCsel -> []

let sequence_dp (d1,c1) (d2,c2) = match c1 with
  | NoCsel -> List.map (fun d -> d,c2) (D.sequence_dp d1 d2)
  | OkCsel -> []

let expand_dp_dir (dir,_) = D.expand_dp_dir dir

(* Read-Modify-Write *)
module RMW = struct
type rmw =  LrSc | LdOp of atomic_op | StOp of atomic_op | Swp | Cas | AllAmo

type nonrec atom = atom

let pp_aop op =  Misc.capitalize (Misc.lowercase (pp_aop op))

let pp_rmw compat = function
  | LrSc -> if compat then "Rmw" else "LxSx"
  | Swp -> "Amo.Swp"
  | Cas -> "Amo.Cas"
  | LdOp op -> sprintf "Amo.Ld%s" (pp_aop op)
  | StOp op -> sprintf "Amo.St%s" (pp_aop op)
  | AllAmo -> sprintf "Amo"

let is_one_instruction = function
  | LrSc -> false
  | LdOp _ | StOp _ | Swp | Cas | AllAmo -> true

let fold_aop f r =
  let r = f A_ADD r in
  let r = f A_EOR r in
  let r = f A_SET r in
  let r = f A_CLR r in
  r

let fold_rmw wildcard f r =
  let r = f LrSc r in
  let r = f Swp r in
  let r = f Cas r in
  let r = fold_aop (fun op r -> f (LdOp op) r) r in
  let r = fold_aop (fun op r -> f (StOp op) r) r in
  let r = if wildcard then f AllAmo r else r in
  r

let all_concrete_rmw =
  fold_rmw false ( fun rmw acc ->
    if rmw <> AllAmo && rmw <> LrSc then rmw :: acc else acc
  ) []
let expand_rmw rmw = match rmw with
  | LrSc | Swp | Cas | LdOp _ | StOp _ -> [rmw]
  | AllAmo -> all_concrete_rmw

let fold_rmw_compat f r = f LrSc r

(* Check legal anotation for AMO instructions and LxSx pairs *)

let ok_rw ar aw =
  match ar,aw with
  | (Some ((Acq _|Plain _),_)|None),(Some ((Rel _|Plain _),_)|None)
    -> true
  | _ -> false

let ok_w  ar aw =
  match ar,aw with
  | (Some (Plain _,_)|None),(Some ((Rel _|Plain _),_)|None)
    -> true
  | _ -> false

let same_mixed (a1:atom option) (a2:atom option) =
  let a1 = get_access_atom a1
  and a2 = get_access_atom a2 in
  Misc.opt_eq MachMixed.equal a1 a2

let applies_atom_rmw rmw ar aw = match rmw with
  | LrSc ->
     ok_rw ar aw && (do_cu || same_mixed ar aw)
  | Swp|Cas|LdOp _| AllAmo ->
     ok_rw ar aw && same_mixed ar aw
  | StOp _ ->
     ok_w ar aw && same_mixed ar aw

let show_rmw_reg = function
| StOp _ -> false
| LdOp _|Cas|Swp|LrSc -> true
| AllAmo -> assert false

let compute_rmw r ~old ~operand =
    match r with
    | LdOp op | StOp op ->
      begin match op with
        | A_ADD -> old + operand
        | A_SMAX -> if old > operand then old else operand
        | A_UMAX ->
           let o = Int64.of_int old and c = Int64.of_int operand in
           if Int64.unsigned_compare o c >  0 then old else operand
        | A_SMIN -> if old < operand then old else operand
        | A_UMIN ->
           let o = Int64.of_int old and c = Int64.of_int operand in
           if Int64.unsigned_compare o c <  0 then old else operand
        | A_EOR -> old lxor operand
        | A_SET -> old lor operand
        | A_CLR -> old land (lnot operand)
    end
    | LrSc | Swp | Cas  -> operand
    | AllAmo -> assert false
end

include
    ArchExtra_gen.Make
    (struct
      type arch_reg = reg

      let is_symbolic = function
        | Symbolic_reg _ -> true
        | _ -> false

      let pp_reg = pp_reg
      let pp_i = pp_i
      let free_registers = allowed_for_symb

      type special = reg
      type special2 = reg
      type special3 = int * reg
      let specials = vregs
      let specials2 = pregs
      let specials3 = zaslices
      type arch_atom = atom
      module Value = Value
    end)

end
