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
let do_memtag = C.variant Variant_gen.MemTag
let do_store_only = C.variant Variant_gen.StoreOnly
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

type neon_opt = SIMD.atom

type pair_idx = UnspecLoc

type atom_acc =
  | Plain of capa_opt | Acq of capa_opt | AcqPc of capa_opt | Rel of capa_opt
  | Atomic of atom_rw | Tag | CapaTag | CapaSeal | Pte of atom_pte | Neon of neon_opt
  | Pair of [ld_pair_opt | st_pair_opt] * pair_idx | Instr

let  plain = Plain None

type atom = atom_acc * MachMixed.t option

type rmw = LrSc | LdOp of atomic_op | StOp of atomic_op | Swp | Cas | AllAmo
  (* `SafeAmo` unfolds to
     `[Swp; Cas; LdOp A_ADD; StOp A_ADD]`, that is,
     edges `[Amo.Swp; Amo.Cas; Amo.LdAdd; Amo.StAdd]` and
     the corresponding instructions `[SWP; CAS; LDADD; STADD]`.
     These edges, `Amo.R`, can be safely used to generate cycles.
     That is, for any `Amo.R`,
     (1) given a value `a` (of a location), e.g. 0,
     (2) given the current operand `b`, e.g. 1, where `b` is picked
     by `diy` internally from an incremental counter starting from 1,
     then the result value of `a Amo.R b` should be
     distinct from the initial value `a`. Hence we can distinguish
     whether `Amo.R` takes effect by reading the value of `a Amo.R b`,
     making it safe to use when generating tests.
     A counterexample is
     `diyone7 -arch AArch64 "PodWR Amo.LdClr Rfe PodRW Coe"`
     ```
     P0               | P1          ;
     ...              | LDR W0,[X2] ;
     MOV W4,#1        | ...         ;
     LDCLR W4,W3,[X2] |             ;
     exists (... /\ 0:X3=0 /\ 1:X0=0)
     ```
     Here the initial value is observed by `0:X3 = 0`, the instruction
     calculates `0 LDCLR 1`, and the result is observed by `1:X1 = 0`.
     The problem here is `1:X1 = 0:X3`, thus the test cannot
     distinguish whether `LDCLR` or `Amo.LdClr` takes effect. *)
  | SafeAmo

module StructuredAtom = struct
  type access_order =
    (* Plain access order, as in `P` or the order part of `h0`. *)
    | OrderPlain
    (* Acquire load order, as in `A`. *)
    | OrderAcquire
    (* Acquire-PC load order, as in `Q`. *)
    | OrderAcquirePc
    (* Release store order, as in `L`. *)
    | OrderRelease
    (* Atomic/RMW order, as in `X`, `XA`, `XL`, or `XAL`. *)
    | OrderAtomic of atom_rw

  type access_type =
    (* Ordinary integer/general-purpose data access. *)
    | OrdinaryAccess
    (* Mixed-size slice of an ordinary access, as in `b0`, `h0`, or `w0`. *)
    | AccessSize of MachMixed.t
    (* Morello capability data access, as in `Pc`, `Ac`, `Qc`, or `Lc`. *)
    | CapaAccess
    (* Morello capability-tag access, as in `Ct`. *)
    | CapaTagAccess
    (* Morello capability-seal access, as in `Cs`. *)
    | CapaSealAccess
    (* MemTag allocation-tag access, as in `T`. *)
    | TagAccess
    (* VMSA PTE access, as in `Pte`, `PteA`, `PteV1`, or `PteHA`. *)
    | PteAccess of atom_pte
    (* SIMD/Neon/SVE/SME access, as in `NeP` or `Ne1`. *)
    | NeonAccess of neon_opt
    (* Pair access, as in `Pa`, `PaN`, `PaIQ`, `PaA`, `PaIL`, or `PaL`. *)
    | PairAccess of [ld_pair_opt | st_pair_opt] * pair_idx
    (* Instruction-fetch access used by the ifetch variant, printed as `I`. *)
    | InstrAccess

  type t =
    { access_type : access_type;
      access_order : access_order; }

  let make access_type access_order = { access_type; access_order; }

  let plain = make OrdinaryAccess OrderPlain
  let default = make OrdinaryAccess (OrderAtomic PP)
  let instr = make InstrAccess OrderPlain

  let is_valid a =
    match a.access_type,a.access_order with
    | (OrdinaryAccess|AccessSize _),(OrderPlain|OrderAcquire|OrderAcquirePc
                                    |OrderRelease|OrderAtomic _) ->
        true
    | CapaAccess,(OrderPlain|OrderAcquire|OrderAcquirePc|OrderRelease) ->
        true
    | PteAccess Read,(OrderPlain|OrderAcquire|OrderAcquirePc) ->
        true
    | PteAccess (Set _),(OrderPlain|OrderRelease) ->
        true
    | PteAccess (Set p),(OrderAcquire|OrderAcquirePc)
      when p = WPTESet.singleton WPTE.HA ->
        true
    | (CapaTagAccess|CapaSealAccess|TagAccess|NeonAccess _
      |PairAccess _|InstrAccess),OrderPlain ->
        true
    | _,_ ->
        false

  let order_to_legacy = function
    | OrderPlain -> Plain None
    | OrderAcquire -> Acq None
    | OrderAcquirePc -> AcqPc None
    | OrderRelease -> Rel None
    | OrderAtomic rw -> Atomic rw

  let to_legacy a =
    if not (is_valid a) then assert false
    else
      match a.access_type,a.access_order with
      | OrdinaryAccess,access_order ->
          order_to_legacy access_order,None
      | AccessSize m,access_order ->
          order_to_legacy access_order,Some m
      | CapaAccess,OrderPlain -> Plain (Some Capability),None
      | CapaAccess,OrderAcquire -> Acq (Some Capability),None
      | CapaAccess,OrderAcquirePc -> AcqPc (Some Capability),None
      | CapaAccess,OrderRelease -> Rel (Some Capability),None
      | CapaAccess,OrderAtomic _ -> assert false
      | CapaTagAccess,OrderPlain -> CapaTag,None
      | CapaSealAccess,OrderPlain -> CapaSeal,None
      | TagAccess,OrderPlain -> Tag,None
      | PteAccess Read,OrderPlain -> Pte Read,None
      | PteAccess Read,OrderAcquire -> Pte ReadAcq,None
      | PteAccess Read,OrderAcquirePc -> Pte ReadAcqPc,None
      | PteAccess (Set p),OrderPlain -> Pte (Set p),None
      | PteAccess (Set p),OrderRelease -> Pte (SetRel p),None
      | PteAccess (Set p),OrderAcquire
        when p = WPTESet.singleton WPTE.HA -> Pte ReadHAAcq,None
      | PteAccess (Set p),OrderAcquirePc
        when p = WPTESet.singleton WPTE.HA -> Pte ReadHAAcqPc,None
      | PteAccess (ReadAcq|ReadAcqPc|SetRel _|ReadHAAcq|ReadHAAcqPc),_ ->
          assert false
      | NeonAccess n,OrderPlain -> Neon n,None
      | PairAccess (opt,idx),OrderPlain -> Pair (opt,idx),None
      | InstrAccess,OrderPlain -> Instr,None
      | (CapaTagAccess|CapaSealAccess|TagAccess|PteAccess _
        |NeonAccess _|PairAccess _|InstrAccess),
        (OrderAcquire|OrderAcquirePc|OrderRelease|OrderAtomic _) ->
          assert false

  let of_legacy =
    let ordinary_or_size = function
      | None -> OrdinaryAccess
      | Some m -> AccessSize m in
    function
    | Plain None,None -> plain
    | Plain None,Some m -> make (AccessSize m) OrderPlain
    | Acq None,m -> make (ordinary_or_size m) OrderAcquire
    | AcqPc None,m -> make (ordinary_or_size m) OrderAcquirePc
    | Rel None,m -> make (ordinary_or_size m) OrderRelease
    | Atomic rw,m -> make (ordinary_or_size m) (OrderAtomic rw)
    | Plain (Some Capability),None -> make CapaAccess OrderPlain
    | Acq (Some Capability),None -> make CapaAccess OrderAcquire
    | AcqPc (Some Capability),None -> make CapaAccess OrderAcquirePc
    | Rel (Some Capability),None -> make CapaAccess OrderRelease
    | CapaTag,None -> make CapaTagAccess OrderPlain
    | CapaSeal,None -> make CapaSealAccess OrderPlain
    | Tag,None -> make TagAccess OrderPlain
    | Pte Read,None -> make (PteAccess Read) OrderPlain
    | Pte ReadAcq,None -> make (PteAccess Read) OrderAcquire
    | Pte ReadAcqPc,None -> make (PteAccess Read) OrderAcquirePc
    | Pte (Set p),None -> make (PteAccess (Set p)) OrderPlain
    | Pte (SetRel p),None -> make (PteAccess (Set p)) OrderRelease
    | Pte ReadHAAcq,None ->
        make (PteAccess (Set (WPTESet.singleton WPTE.HA))) OrderAcquire
    | Pte ReadHAAcqPc,None ->
        make (PteAccess (Set (WPTESet.singleton WPTE.HA))) OrderAcquirePc
    | Neon n,None -> make (NeonAccess n) OrderPlain
    | Pair (opt,idx),None -> make (PairAccess (opt,idx)) OrderPlain
    | Instr,None -> instr
    | (Plain (Some Capability)|Acq (Some Capability)|AcqPc (Some Capability)
      |Rel (Some Capability)|Tag|CapaTag|CapaSeal|Pte _|Neon _|Pair _|Instr),Some _ ->
        assert false

  let compare_atom_rw rw1 rw2 =
    let rank = function
      | PP -> 0
      | PL -> 1
      | AP -> 2
      | AL -> 3 in
    Int.compare (rank rw1) (rank rw2)

  let compare_mach_size sz1 sz2 =
    let open MachSize in
    let rank = function
      | Byte -> 0
      | Short -> 1
      | Word -> 2
      | Quad -> 3
      | S128 -> 4 in
    Int.compare (rank sz1) (rank sz2)

  let compare_mixed (sz1,o1) (sz2,o2) =
    match compare_mach_size sz1 sz2 with
    | 0 -> Int.compare o1 o2
    | c -> c

  let compare_atom_pte p1 p2 =
    let rank = function
      | Read -> 0
      | ReadAcq -> 1
      | ReadAcqPc -> 2
      | Set _ -> 3
      | SetRel _ -> 4
      | ReadHAAcq -> 5
      | ReadHAAcqPc -> 6 in
    match Int.compare (rank p1) (rank p2) with
    | 0 -> begin
        match p1,p2 with
        | Set s1,Set s2
        | SetRel s1,SetRel s2 -> WPTESet.compare s1 s2
        | (Read,Read)
        | (ReadAcq,ReadAcq)
        | (ReadAcqPc,ReadAcqPc)
        | (ReadHAAcq,ReadHAAcq)
        | (ReadHAAcqPc,ReadHAAcqPc) -> 0
        | _,_ -> assert false
      end
    | c -> c

  let compare_neon n1 n2 =
    let open SIMD in
    let rank = function
      | SmV -> 0
      | SmH -> 1
      | SvV -> 2
      | Sv1 -> 3
      | Sv2i -> 4
      | Sv3i -> 5
      | Sv4i -> 6
      | NeP -> 7
      | NeAcqPc -> 8
      | NeRel -> 9
      | Ne1 -> 10
      | Ne2 -> 11
      | Ne3 -> 12
      | Ne4 -> 13
      | Ne2i -> 14
      | Ne3i -> 15
      | Ne4i -> 16
      | NePa -> 17
      | NePaN -> 18 in
    Int.compare (rank n1) (rank n2)

  let compare_pair_opt p1 p2 =
    let rank = function
      | `Pa -> 0
      | `PaN -> 1
      | `PaIQ -> 2
      | `PaIL -> 3
      | `PaA -> 4
      | `PaL -> 5 in
    Int.compare (rank p1) (rank p2)

  let compare_pair_idx idx1 idx2 =
    match idx1,idx2 with
    | UnspecLoc,UnspecLoc -> 0

  let compare_access_order o1 o2 =
    let rank = function
      | OrderPlain -> 0
      | OrderAcquire -> 1
      | OrderAcquirePc -> 2
      | OrderRelease -> 3
      | OrderAtomic _ -> 4 in
    match Int.compare (rank o1) (rank o2) with
    | 0 -> begin
        match o1,o2 with
        | OrderAtomic rw1,OrderAtomic rw2 -> compare_atom_rw rw1 rw2
        | (OrderPlain,OrderPlain)
        | (OrderAcquire,OrderAcquire)
        | (OrderAcquirePc,OrderAcquirePc)
        | (OrderRelease,OrderRelease) -> 0
        | _,_ -> assert false
      end
    | c -> c

  let compare_access_type t1 t2 =
    let rank = function
      | OrdinaryAccess -> 0
      | AccessSize _ -> 1
      | CapaAccess -> 2
      | CapaTagAccess -> 3
      | CapaSealAccess -> 4
      | TagAccess -> 5
      | PteAccess _ -> 6
      | NeonAccess _ -> 7
      | PairAccess _ -> 8
      | InstrAccess -> 9 in
    match Int.compare (rank t1) (rank t2) with
    | 0 -> begin
        match t1,t2 with
        | AccessSize m1,AccessSize m2 -> compare_mixed m1 m2
        | PteAccess p1,PteAccess p2 -> compare_atom_pte p1 p2
        | NeonAccess n1,NeonAccess n2 -> compare_neon n1 n2
        | PairAccess (p1,idx1),PairAccess (p2,idx2) -> begin
            match compare_pair_opt p1 p2 with
            | 0 -> compare_pair_idx idx1 idx2
            | c -> c
          end
        | (OrdinaryAccess,OrdinaryAccess)
        | (CapaAccess,CapaAccess)
        | (CapaTagAccess,CapaTagAccess)
        | (CapaSealAccess,CapaSealAccess)
        | (TagAccess,TagAccess)
        | (InstrAccess,InstrAccess) -> 0
        | _,_ -> assert false
      end
    | c -> c

  let compare a1 a2 =
    match compare_access_type a1.access_type a2.access_type with
    | 0 -> compare_access_order a1.access_order a2.access_order
    | c -> c

  let equal a1 a2 = compare a1 a2 = 0
  let pp_mixed (sz,o) =
    sprintf "%s%i" (MachSize.pp_short sz) o

  let pp_atom_rw = function
    | PP -> ""
    | PL -> "L"
    | AP -> "A"
    | AL -> "AL"

  let pp_order = function
    | OrderPlain -> "P"
    | OrderAcquire -> "A"
    | OrderAcquirePc -> "Q"
    | OrderRelease -> "L"
    | OrderAtomic rw -> sprintf "X%s" (pp_atom_rw rw)

  let pp_pair_opt = function
    | `Pa -> ""
    | `PaN -> "N"
    | `PaIQ -> "IQ"
    | `PaIL -> "IL"
    | `PaA -> "A"
    | `PaL -> "L"

  let pp_pair_idx = function
    | UnspecLoc -> ""

  let pp = function
    | { access_type = OrdinaryAccess; access_order = OrderPlain; } -> "P"
    | { access_type = OrdinaryAccess; access_order; } -> pp_order access_order
    | { access_type = AccessSize m; access_order = OrderPlain; } -> pp_mixed m
    | { access_type = AccessSize m; access_order; } ->
        sprintf "%s.%s" (pp_order access_order) (pp_mixed m)
    | { access_type = CapaAccess; access_order = OrderPlain; } -> "Pc"
    | { access_type = CapaAccess; access_order = OrderAcquire; } -> "Ac"
    | { access_type = CapaAccess; access_order = OrderAcquirePc; } -> "Qc"
    | { access_type = CapaAccess; access_order = OrderRelease; } -> "Lc"
    | { access_type = CapaAccess; access_order = OrderAtomic _; } -> assert false
    | { access_type = CapaTagAccess; access_order = OrderPlain; } -> "Ct"
    | { access_type = CapaSealAccess; access_order = OrderPlain; } -> "Cs"
    | { access_type = TagAccess; access_order = OrderPlain; } -> "T"
    | { access_type = PteAccess Read; access_order = OrderPlain; } -> "Pte"
    | { access_type = PteAccess Read; access_order = OrderAcquire; } -> "PteA"
    | { access_type = PteAccess Read; access_order = OrderAcquirePc; } -> "PteQ"
    | { access_type = PteAccess (Set p); access_order = OrderPlain; } ->
        sprintf "Pte%s" (pp_w_pte p)
    | { access_type = PteAccess (Set p); access_order = OrderRelease; } ->
        sprintf "Pte%sL" (pp_w_pte p)
    | { access_type = PteAccess (Set p); access_order = OrderAcquire; } ->
        assert (p = WPTESet.singleton WPTE.HA) ; "PteHAA"
    | { access_type = PteAccess (Set p); access_order = OrderAcquirePc; } ->
        assert (p = WPTESet.singleton WPTE.HA) ; "PteHAQ"
    | { access_type = NeonAccess n; access_order = OrderPlain; } -> SIMD.pp n
    | { access_type = PairAccess (opt,idx); access_order = OrderPlain; } ->
        sprintf "Pa%s%s" (pp_pair_opt opt) (pp_pair_idx idx)
    | { access_type = InstrAccess; access_order = OrderPlain; } -> "I"
    | { access_type =
          PteAccess (ReadAcq|ReadAcqPc|SetRel _|ReadHAAcq|ReadHAAcqPc); _ } ->
        assert false
    | { access_type =
          (CapaTagAccess|CapaSealAccess|TagAccess|PteAccess _
          |NeonAccess _|PairAccess _|InstrAccess);
        access_order = (OrderAcquire|OrderAcquirePc|OrderRelease|OrderAtomic _); } ->
        assert false

  let get_access_atom = function
    | None -> None
    | Some { access_type = AccessSize m; _ } -> Some m
    | Some _ -> None

  let set_access_atom atom m =
    match atom with
    | None -> Some (make (AccessSize m) OrderPlain)
    | Some ({ access_type = (OrdinaryAccess|AccessSize _); _ } as atom) ->
        Some { atom with access_type = AccessSize m }
    | Some atom -> Some atom

  let overlap a1 a2 =
    match a1.access_type,a2.access_type with
    | AccessSize sz1,AccessSize sz2 -> MachMixed.overlap sz1 sz2
    | _,_ -> true

  let is_ifetch = function
    | Some { access_type = InstrAccess; _ } -> true
    | _ -> false

  let is_pair = function
    | Some { access_type = PairAccess _; _ } -> true
    | _ -> false

  let as_integers atom =
    let neon_as_integers =
      let open SIMD in
      function
      | NeP | NeAcqPc | NeRel -> 1
      | NePa | NePaN -> 2
      | SmV | SmH
      | SvV | Sv1 | Ne1 -> 4
      | Sv2i | Ne2 | Ne2i -> 8
      | Sv3i | Ne3 | Ne3i -> 12
      | Sv4i | Ne4 | Ne4i -> 16 in
    match atom with
    | Some { access_type = NeonAccess n; _ } ->
        begin match neon_as_integers n with
        | 1 -> None
        | n -> Some n
        end
    | Some { access_type = PairAccess _; _ } -> Some 2
    | Some _|None -> None

  let worth_final = function
    | { access_order = OrderAtomic _; _ } -> true
    | _ -> false

  let get_machine_feature = function
    | Some { access_type = PteAccess (Set pte); _ } ->
        let open WPTE in
        WPTESet.fold
          (fun field features -> match field with
            | HA|HD -> StringSet.add (WPTE.pp field) features
            | _ -> features)
          pte StringSet.empty
    | Some _|None -> StringSet.empty

  let applies a d =
    let open WPTE in
    match a.access_type,a.access_order,d with
    | NeonAccess SIMD.NeAcqPc,_,W
    | NeonAccess SIMD.NeRel,_,R -> false
    | (OrdinaryAccess|AccessSize _|CapaAccess),(OrderAcquire|OrderAcquirePc),R -> true
    | (OrdinaryAccess|AccessSize _|CapaAccess),OrderRelease,W -> true
    | PteAccess Read,(OrderPlain|OrderAcquire|OrderAcquirePc),R -> true
    | PteAccess (Set p),OrderPlain,R when WPTESet.mem HA p -> true
    | PteAccess (Set p),(OrderAcquire|OrderAcquirePc),R
      when p = WPTESet.singleton HA -> true
    | PteAccess (Set _),(OrderPlain|OrderRelease),W -> true
    | InstrAccess,OrderPlain,R -> true
    | (OrdinaryAccess|AccessSize _|CapaAccess),OrderPlain,(R|W) -> true
    | (OrdinaryAccess|AccessSize _),OrderAtomic _,(R|W) -> true
    | (TagAccess|CapaTagAccess|CapaSealAccess|NeonAccess _),OrderPlain,(R|W) ->
        true
    | PairAccess ((`Pa|`PaN|`PaIQ|`PaA),_),OrderPlain,R -> true
    | PairAccess ((`Pa|`PaN|`PaIL|`PaL),_),OrderPlain,W -> true
    | _ -> false

  let applies_rmw rmw ar aw =
    let ok_rw ar aw = match ar,aw with
      | (None|Some {
          access_type = (OrdinaryAccess|AccessSize _|CapaAccess);
          access_order = (OrderPlain|OrderAcquire); }),
        (None|Some {
          access_type = (OrdinaryAccess|AccessSize _|CapaAccess);
          access_order = (OrderPlain|OrderRelease); }) -> true
      | _,_ -> false in
    let ok_w ar aw = match ar,aw with
      | (None|Some {
          access_type = (OrdinaryAccess|AccessSize _|CapaAccess);
          access_order = OrderPlain; }),
        (None|Some {
          access_type = (OrdinaryAccess|AccessSize _|CapaAccess);
          access_order = (OrderPlain|OrderRelease); }) -> true
      | _,_ -> false in
    let same_mixed =
      Misc.opt_eq MachMixed.equal
        (get_access_atom ar) (get_access_atom aw) in
    match rmw with
    | LrSc -> ok_rw ar aw && (do_cu || same_mixed)
    | Swp|Cas|LdOp _|AllAmo|SafeAmo -> ok_rw ar aw && same_mixed
    | StOp _ -> ok_w ar aw && same_mixed

  let is_tthm fields =
    let open WPTE in
    WPTESet.mem HD fields || WPTESet.mem HA fields

  let to_bank = function
    | { access_type = TagAccess; access_order = OrderPlain; } -> Code.Tag
    | { access_type = PteAccess (Set p); access_order =
          (OrderPlain|OrderAcquire|OrderAcquirePc|OrderRelease); }
      when is_tthm p -> Code.Ord
    | { access_type = PteAccess (Read|Set _); _ } -> Code.Pte
    | { access_type = CapaTagAccess; access_order = OrderPlain; } -> Code.CapaTag
    | { access_type = CapaSealAccess; access_order = OrderPlain; } -> Code.CapaSeal
    | { access_type = NeonAccess n; access_order = OrderPlain; } -> Code.VecReg n
    | { access_type = PairAccess (_,UnspecLoc); access_order = OrderPlain; } -> Code.Pair
    | { access_type = InstrAccess; access_order = OrderPlain; } -> Code.Instr
    | { access_type = (OrdinaryAccess|AccessSize _|CapaAccess); _ } -> Code.Ord
    | _ -> assert false

  let merge a1 a2 =
    let open WPTE in
    let merge_order o1 o2 = match o1,o2 with
    | OrderPlain,o
    | o,OrderPlain -> Some o
    | o1,o2 when o1 = o2 -> Some o1
    | _,_ -> None in
    match a1,a2 with
    | { access_type = OrdinaryAccess; access_order = OrderPlain; },
      { access_type = InstrAccess; _ }
    | { access_type = InstrAccess; _ },
      { access_type = OrdinaryAccess; access_order = OrderPlain; } ->
        None
    | { access_type = OrdinaryAccess; access_order = OrderPlain; }, a
    | a, { access_type = OrdinaryAccess; access_order = OrderPlain; } ->
        Some a
    | { access_type = AccessSize m; access_order = OrderPlain; },
      { access_type = OrdinaryAccess; access_order; }
    | { access_type = OrdinaryAccess; access_order; },
      { access_type = AccessSize m; access_order = OrderPlain; } ->
        Some (make (AccessSize m) access_order)
    | { access_type = AccessSize m; access_order = o1; },
      { access_type = OrdinaryAccess; access_order = o2; }
      when o1 = o2 ->
        Some (make (AccessSize m) o1)
    | { access_type = OrdinaryAccess; access_order = o1; },
      { access_type = AccessSize m; access_order = o2; }
      when o1 = o2 ->
        Some (make (AccessSize m) o1)
    | { access_type = AccessSize m1; access_order = OrderPlain; },
      { access_type = AccessSize m2; access_order; }
      when m1 = m2 ->
        Some (make (AccessSize m1) access_order)
    | { access_type = AccessSize m1; access_order; },
      { access_type = AccessSize m2; access_order = OrderPlain; }
      when m1 = m2 ->
        Some (make (AccessSize m1) access_order)
    | { access_type = PteAccess p; access_order = OrderPlain; },
      { access_type = OrdinaryAccess; access_order; }
    | { access_type = OrdinaryAccess; access_order; },
      { access_type = PteAccess p; access_order = OrderPlain; } ->
        let atom = make (PteAccess p) access_order in
        if is_valid atom then Some atom else None
    | { access_type = PteAccess (Set set1); access_order = order1; },
      { access_type = PteAccess (Set set2); access_order = order2; } ->
        let set = WPTESet.union set1 set2 in
        begin match merge_order order1 order2 with
        | Some access_order
          when contain_valid_pte_fields set || contain_valid_tthm_fields set ->
            let atom = make (PteAccess (Set set)) access_order in
            if is_valid atom then Some atom else None
        | Some _|None -> None
        end
    | _,_ ->
        if equal a1 a2 then Some a1 else None

  let fold_atom_rw f r = f PP (f PL (f AP (f AL r)))

  let fold_access_order f r =
    let r = f OrderPlain r in
    let r = f OrderAcquire r in
    let r = f OrderAcquirePc r in
    let r = f OrderRelease r in
    fold_atom_rw (fun rw -> f (OrderAtomic rw)) r

  let fold_pte_access f r =
    let open WPTE in
    let fold_set set r =
      f (PteAccess (Set set)) r in
    let r =
      List.fold_left
        (fun r pte -> fold_set (WPTESet.singleton pte) r)
        r WPTE.all in
    r
    |> f (PteAccess Read)

  let fold_neon_access fold f r =
    fold (fun n -> f (NeonAccess n)) r

  let fold_pair_access f r =
    let add opt = f (PairAccess (opt,UnspecLoc)) in
    r |> add `Pa |> add `PaN |> add `PaIQ |> add `PaIL |> add `PaA |> add `PaL

  let fold_mixed f r =
    let open MachSize in
    let get_off =
      (if C.fullmixed then get_off else get_off_reduced) C.naturalsize in
    let fold_size sz r =
      List.fold_right (fun o r -> f (sz,o) r) (get_off sz) r in
    r |> fold_size Byte |> fold_size Short |> fold_size Word
      |> fold_size Quad |> fold_size S128

  let fold_access_type f r =
    let r = f OrdinaryAccess r in
    let r = if do_mixed then fold_mixed (fun m -> f (AccessSize m)) r else r in
    let r = if do_kvm then fold_pte_access f r else r in
    let r = if do_neon then fold_neon_access SIMD.fold_neon f r else r in
    let r = if do_sve then fold_neon_access SIMD.fold_sve f r else r in
    let r = if do_sme then fold_neon_access SIMD.fold_sme f r else r in
    let r = if do_mixed then r else fold_pair_access f r in
    let r = if do_memtag then f TagAccess r else r in
    let r = if do_self then f InstrAccess r else r in
    if do_morello then
      r
      |> f CapaAccess
      |> f CapaTagAccess
      |> f CapaSealAccess
    else r

  let fold f r =
    fold_access_type
      (fun access_type ->
        fold_access_order
          (fun access_order r ->
            let atom = make access_type access_order in
            let is_component = match access_type,access_order with
            | PteAccess _,(OrderAcquire|OrderAcquirePc|OrderRelease|OrderAtomic _) ->
                false
            | _,_ -> true in
            if is_component && is_valid atom && (applies atom R || applies atom W)
            then f atom r
            else r))
      r

end

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

let default_atom = StructuredAtom.to_legacy StructuredAtom.default
let instr_atom = Some (StructuredAtom.to_legacy StructuredAtom.instr)

let applies_atom atom d =
  StructuredAtom.applies (StructuredAtom.of_legacy atom) d

let is_ifetch atom =
  let atom = match atom with
  | None -> None
  | Some atom -> Some (StructuredAtom.of_legacy atom) in
  StructuredAtom.is_ifetch atom

   let pp_plain = StructuredAtom.pp StructuredAtom.plain
   let pair_opt_to_ld : [ld_pair_opt | st_pair_opt] -> ld_pair_opt = function
     | `Pa -> `Pa | `PaN -> `PaN | `PaIQ -> `PaIQ | `PaA -> `PaA
     | `PaIL | `PaL -> assert false

   let pair_opt_to_st : [ld_pair_opt | st_pair_opt] -> st_pair_opt = function
     | `Pa -> `Pa | `PaN -> `PaN | `PaIL -> `PaIL | `PaL -> `PaL
     | `PaIQ | `PaA -> assert false

   let pp_atom_acc atom =
     StructuredAtom.pp (StructuredAtom.of_legacy (atom,None))

   let pp_atom atom =
     let atom = StructuredAtom.of_legacy atom in
     if StructuredAtom.equal atom StructuredAtom.plain then ""
     else StructuredAtom.pp atom

   let compare_atom a1 a2 =
     StructuredAtom.compare
       (StructuredAtom.of_legacy a1) (StructuredAtom.of_legacy a2)

   let equal_atom a1 a2 =
     StructuredAtom.equal
       (StructuredAtom.of_legacy a1) (StructuredAtom.of_legacy a2)

   let get_access_atom = function
   | None -> None
   | Some atom ->
       StructuredAtom.get_access_atom (Some (StructuredAtom.of_legacy atom))

   let set_access_atom atom m =
     let atom = match atom with
     | None -> None
     | Some atom -> Some (StructuredAtom.of_legacy atom) in
     match StructuredAtom.set_access_atom atom m with
     | None -> None
     | Some atom -> Some (StructuredAtom.to_legacy atom)

   let fold_atom f r =
     StructuredAtom.fold
       (fun atom r -> f (StructuredAtom.to_legacy atom) r)
       r

   let worth_final atom =
     StructuredAtom.worth_final (StructuredAtom.of_legacy atom)



   let varatom_dir _d f r = f None r

   let merge_atoms a1 a2 =
     match
       StructuredAtom.merge
         (StructuredAtom.of_legacy a1) (StructuredAtom.of_legacy a2)
     with
     | Some atom -> Some (StructuredAtom.to_legacy atom)
     | None -> None

   let overlap_atoms a1 a2 =
     StructuredAtom.overlap
       (StructuredAtom.of_legacy a1) (StructuredAtom.of_legacy a2)

   let atom_to_bank atom =
     StructuredAtom.to_bank (StructuredAtom.of_legacy atom)


(**************)
(* Mixed size *)
(**************)

   let tr_value ao v = match get_access_atom ao with
   | None -> v
   | Some (sz,_) -> Mixed.tr_value sz v

   module ValsMixed =
     MachMixed.Vals
       (struct
         let naturalsize () = C.naturalsize
         let endian = endian
       end)(Value)

let overwrite_value v ao w = match get_access_atom ao with
| None -> w (* total overwrite *)
| Some (sz,o) -> ValsMixed.overwrite_value v sz o w

 let extract_value v ao = match get_access_atom ao with
 | None -> v
 | Some (sz,o) -> ValsMixed.extract_value v sz o

(* Wide accesses *)

   let as_integers atom =
     let atom = match atom with
     | None -> None
     | Some atom -> Some (StructuredAtom.of_legacy atom) in
     StructuredAtom.as_integers atom

   let is_pair atom =
     let atom = match atom with
     | None -> None
     | Some atom -> Some (StructuredAtom.of_legacy atom) in
     StructuredAtom.is_pair atom

  let get_machine_feature atom =
    let atom = match atom with
    | None -> None
    | Some atom -> Some (StructuredAtom.of_legacy atom) in
    StructuredAtom.get_machine_feature atom

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
| Barrier GCSB,_,_
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

let equal_csel c1 c2 = match c1,c2 with
  | OkCsel,OkCsel
  | NoCsel,NoCsel -> true
  | (OkCsel|NoCsel),_ -> false

let equal_dp (d1,c1) (d2,c2) = D.equal_dp d1 d2 && equal_csel c1 c2

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
type nonrec rmw = rmw =
  | LrSc
  | LdOp of atomic_op
  | StOp of atomic_op
  | Swp
  | Cas
  | AllAmo
  | SafeAmo

type nonrec atom = atom

let pp_aop op =  Misc.capitalize (Misc.lowercase (pp_aop op))

let pp_rmw compat = function
  | LrSc -> if compat then "Rmw" else "LxSx"
  | Swp -> "Amo.Swp"
  | Cas -> "Amo.Cas"
  | LdOp op -> sprintf "Amo.Ld%s" (pp_aop op)
  | StOp op -> sprintf "Amo.St%s" (pp_aop op)
  | AllAmo -> sprintf "Amo"
  | SafeAmo -> sprintf "Amo.Safe"

let equal_rmw rmw1 rmw2 = match rmw1,rmw2 with
  | LrSc,LrSc
  | Swp,Swp
  | Cas,Cas
  | AllAmo,AllAmo
  | SafeAmo,SafeAmo -> true
  | LdOp op1,LdOp op2
  | StOp op1,StOp op2 -> atomic_op_equal op1 op2
  | (LrSc|LdOp _|StOp _|Swp|Cas|AllAmo|SafeAmo),_ -> false

let is_one_instruction = function
  | LrSc -> false
  | LdOp _ | StOp _ | Swp | Cas | AllAmo | SafeAmo -> true

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
  let r = if wildcard then f AllAmo r |> f SafeAmo else r in
  r

let all_concrete_rmw =
  fold_rmw false ( fun rmw acc ->
    if rmw <> AllAmo && rmw <> LrSc then rmw :: acc else acc
  ) []
let expand_rmw rmw = match rmw with
  | LrSc | Swp | Cas | LdOp _ | StOp _ -> [rmw]
  | AllAmo -> all_concrete_rmw
  | SafeAmo -> [Swp; Cas; LdOp A_ADD; StOp A_ADD]

let fold_rmw_compat f r = f LrSc r

(* Check legal anotation for AMO instructions and LxSx pairs *)

let applies_atom_rmw rmw ar aw =
  let to_structured = function
    | None -> None
    | Some atom -> Some (StructuredAtom.of_legacy atom) in
  let ar = to_structured ar and aw = to_structured aw in
  StructuredAtom.applies_rmw rmw ar aw

let show_rmw_reg = function
| StOp _ -> false
| LdOp _|Cas|Swp|LrSc -> true
| AllAmo | SafeAmo -> assert false

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
    | AllAmo | SafeAmo -> assert false

(* Rule out `rmw_list` that contains the same type of atomic operation for a location. *)
let is_valid_rmw rmw_list =
  let atomic_st_list = List.filter_map ( function
    | StOp op -> Some ( op )
    | _ -> None ) rmw_list in
  List.length atomic_st_list = List.length (Util.List.uniq ~eq:atomic_op_equal atomic_st_list)
end

let free_registers =
  if do_sme then
    (* Reserve SME's slice index register *)
    List.filter ( fun r -> r != Ireg R12) allowed_for_symb
  else
    allowed_for_symb

include
    ArchExtra_gen.Make
    (struct
      type arch_reg = reg

      let is_symbolic = function
        | Symbolic_reg _ -> true
        | _ -> false

      let pp_reg = pp_reg
      let free_registers = free_registers

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
