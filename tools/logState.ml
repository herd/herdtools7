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

(**********************************)
(* Hash-consed lists for bindings *)
(**********************************)

(* Hashed list of bindings *)
module HashedEnv = HashedList.Make (struct type elt = HashedBinding.key end)

(* Hashed list of faults *)
module HashedFaults = HashedList.Make(struct type elt = HashedFault.key end)

(* Hashed pair of triples *)
module HashedState = struct

  module S = struct

    type t = {e:HashedEnv.t; f:HashedFaults.t; a:HashedFaults.t; }

    let equal
      {e=e1; f=f1; a=a1;}
      {e=e2; f=f2; a=a2;} =
      e1 == e2 && f1 == f2 && a1=a2

    let hash {e; f; a;} =
      let eh = HashedEnv.as_hash e
      and fh = HashedFaults.as_hash f
      and ah = HashedFaults.as_hash a in
      abs (Misc.mix  (0x4F1BBCDC+eh) (0x4F1BBCDC+fh) (0x4F1BBCDC+ah))

  end

  module M = Hashcons.Make(S)
  type node = M.node

  let t = M.create 101

  let as_hashed e f a =
    M.hashcons t {S.e=e; S.f=f; S.a=a;}
  let as_t h = h.Hashcons.node

end

type st_concrete = HashedState.node

type parsed_st =
  {
    p_noccs : Int64.t ;
    p_st : st_concrete ;
  }


type parsed_sts = {
    p_nouts : Int64.t ;
    p_sts : parsed_st list ;
  }

type topology = HashedString.t * Int64.t
type parsed_topologies = topology list

type sts = parsed_sts (* + sorted *)

let rec do_equal_states xs ys = match xs,ys with
  | [],[] -> true
  | (_::_,[])|([],_::_) -> false
  | x::xs,y::ys ->
     x.p_st == y.p_st && (* Ignore counts, outcomes are hash-consed *)
     do_equal_states xs ys

let equal_states sts1 sts2 = do_equal_states sts1.p_sts sts2.p_sts

type topologies = parsed_topologies (* + sorted *)

type kind = Allow | Require | Forbid | NoKind | ErrorKind | Undefined

let is_reliable k = match k with
| Allow|Require|Forbid -> true
| _ -> false

type validation = Undef | Ok | No | DontKnow | Run

let tr_validate kref k v = match kref with
| (Allow|Forbid) ->
    begin match k,v with
    | (Allow,Ok)
    | (Forbid,No)
    | (Require,No)
      -> Some Allow
    | (Allow,No)
    | (Forbid,Ok)
    | (Require,Ok)
      -> Some Forbid
    | _ -> None
    end
| _ -> None



type test =
 { tname : string ;      (* name of the test, aka key *)
   states : sts ;        (* final states observed *)
   condition : LogConstr.cond option ;
   (* Plain condition, enables reconstruction of following fields.
      Elle est pas belle la vie? *)
   kind : kind ;       (* Style of test Require/Allow etc. *)
   loop : bool ;
   validation : validation ; (* condition validation status *)
   witnesses : Int64.t * Int64.t ; (* witnesses pos/neg *)
   hash : string option ;  (* Hash of init state + code *)
   time : float option ;
   topologies : topologies ;
 }

let equal_test t1 t2 =
  Misc.string_eq t1.tname t2.tname
  && Misc.opt_eq Misc.string_eq t1.hash t2.hash
  && equal_states t1.states t2.states

type t =
  { name : string  ;   (* Name of the log file *)
    is_litmus : bool ; (* Litmus? (if_false == memevents log) *)
    tests : test array ; }


exception StateMismatch of string


(* Simplified States *)

type simple_sts = st_concrete list

type simple_test =
  { s_tname : string ;
    s_states : simple_sts ;
    s_hash : string ; }

type simple_t =  { s_name : string ; s_tests : simple_test list; }


let is_local loc = String.contains loc ':'
let is_array_item loc = String.contains loc '['
let is_array v =  String.contains v '{'

let add_brk loc v = not (is_local loc || is_array_item loc || is_array v)

let pretty_binding loc v =
  if add_brk loc v then Printf.sprintf "[%s]=%s" loc v
  else Printf.sprintf "%s=%s" loc v


module Make(O:sig val verbose : int end) = struct
open Printf
open OutMode

module W = Warn.Make(O)
(* Bindings are ordered by their locations, identical locations
   in an outcome is an error *)

let compare_binding p1 p2 =
  match HashedBinding.compare_loc p1 p2 with
| 0 -> assert false
| r -> r

let as_st_concrete bds fs abs =
  let bds = List.sort compare_binding bds
  and fs = List.sort HashedFault.compare fs
  and abs = List.sort HashedFault.compare abs in
  let bds = List.fold_right HashedEnv.cons bds HashedEnv.nil
  and fs =  List.fold_right HashedFaults.cons fs HashedFaults.nil
  and abs =  List.fold_right HashedFaults.cons abs HashedFaults.nil in
  HashedState.as_hashed bds fs abs

let faults_as_strings =
  HashedFaults.map (fun f -> Fault_tools.pp (HashedFault.as_t f))

let st_as_string st =
  let open HashedState in
  let {S.e=e; f; a;} = HashedState.as_t st in
  let pp_env = HashedEnv.map HashedBinding.as_t e
  and pp_faults = faults_as_strings f
  and pp_absent = faults_as_strings a in
  pp_env,pp_faults,pp_absent

let is_empty_simple st = match st.s_states with
| [] -> true
| _::_ -> false

let get_nouts st = st.p_nouts
let get_bindings st = List.map (fun st -> st_as_string st.p_st) st.p_sts

let empty_sts = { p_nouts = Int64.zero ; p_sts = []; }

let pretty_bd loc v = pretty_binding loc v ^ ";"

let pretty_state pref mode with_noccs st =
  let buff = Buffer.create 10 in
  Buffer.add_string buff pref ;
  Buffer.add_char buff '[' ;
  let e,f,a = st_as_string st.p_st in
  let pp_e = List.map (fun (loc,v) -> pretty_bd loc v) e
  and pp_f = f
  and pp_a = List.map (sprintf "~%s") a in
  let pp = String.concat " " (pp_e @ pp_f @ pp_a ) in
  Buffer.add_string buff pp ;
  Buffer.add_char buff ']' ;
  if with_noccs then begin
    match mode with
    | Txt ->
        Buffer.add_string buff (sprintf "<%s>" (Int64.to_string st.p_noccs))
    | LaTeX|HeVeA|HeVeANew ->
        Buffer.add_string buff (sprintf " (%s)" (Int64.to_string st.p_noccs))
  end ;
  Buffer.contents buff

(* Redump log *)

let dump_bd chan loc v =
  if add_brk loc v then fprintf chan " [%s]=%s;" loc v
  else fprintf chan " %s=%s;" loc v

let dump_state chan is_litmus st =
  if is_litmus then fprintf chan  "%-8s:>" (Int64.to_string st.p_noccs) ;
  let {HashedState.S.e=e; f; a;} = HashedState.as_t st.p_st in
  HashedEnv.iter
    (fun p ->
      let loc,v = HashedBinding.as_t p in
      dump_bd chan loc v)
    e ;
  let dump_faults prf =
    HashedFaults.iter
      (fun p ->
        let f = HashedFault.as_t p in
        fprintf chan " %s%s;" prf (Fault_tools.pp f)) in
  dump_faults "" f ;
  dump_faults "~" a ;
  output_char chan '\n'

let dump_states_cond chan is_litmus t =
  if is_litmus then
    fprintf chan "Histogram (%i states)\n"  (List.length t.p_sts)
  else
    fprintf chan "States %i\n"  (List.length t.p_sts) ;
  List.iter (dump_state chan is_litmus) t.p_sts

let dump_states chan t = dump_states_cond chan true t

let no_states sts = match sts.p_sts with
| [] -> true
| _::_ -> false

let no_states_or_no_obs sts = match sts.p_sts with
| [] -> true
| [st] ->
    let { HashedState.S.e=e; f; a; } = HashedState.as_t st.p_st in
    HashedEnv.nilp e && HashedFaults.nilp f && HashedFaults.nilp a
| _::_::_ -> false

let card sts = List.length  sts.p_sts

let millions x = Int64.to_float x /. 1000000.0

let pretty_states pref mode with_noccs st =
  let r = List.map (pretty_state pref mode with_noccs) st.p_sts in
  if with_noccs then
    sprintf "nstates=%i, nouts=%.2fM"
      (List.length st.p_sts) (millions st.p_nouts)::r
  else r

let some_topologies = function
  | [] -> false
  | _::_ -> true

let dump_topologies chan topos =
(* higher count first *)
  let topos =
    List.sort
      (fun (_,n1) (_,n2) ->
        let c = Int64.compare n1 n2 in
        if c < 0 then 1
        else if c > 0 then -1
        else 0) topos in
  List.iter
    (fun (t,n) ->
      if Int64.compare n Int64.zero <> 0 then
        fprintf chan "Topology %-6s:> %s\n"
          (Int64.to_string n) (HashedString.as_t t))
    topos

let pp_kind = function
  | Allow -> "Allow"
  | Require -> "Require"
  | Forbid -> "Forbid"
  | NoKind -> "???"
  | ErrorKind -> "----"
  | Undefined -> "Undefined"

let parse_kind = function
  | "Allow"|"Allowed" -> Some Allow
  | "Require" | "Required" -> Some Require
  | "Forbid"|"Forbidden" -> Some Forbid
  | ""|"Unknown"|"???"|"---" -> Some NoKind
  | "Undefined" -> Some Undefined
  | _ -> None

let pp_validation = function
  | Ok -> "Ok"
  | No -> "No"
  | DontKnow -> "??"
  | Run -> "Run"
  | Undef -> "Undef"


let extract_loc h =
  let loc,_ = HashedBinding.as_t h in
  loc

let mismatch s = raise (StateMismatch (extract_loc s))

let rec compare_env st1 st2 =
  let open Hashcons in
  let open HashedEnv in
  match st1.node,st2.node with
  | Nil,Nil -> 0
  | (Nil,Cons (p,_))
  | (Cons (p,_),Nil) -> mismatch p
  | Cons (p1,st1),Cons (p2,st2) ->
      match HashedBinding.compare_loc p1 p2 with
      | 0 ->
          begin match HashedBinding.compare_v p1 p2 with
          | 0 -> compare_env st1 st2
          | r -> r
          end
      | r ->
          mismatch (if r > 0 then p2 else p1)

let rec compare_faults st1 st2 =
  let open Hashcons in
  let open HashedFaults in
  match st1.node,st2.node with
  | Nil,Nil -> 0
  | (Nil,Cons _) -> -1
  | (Cons _,Nil) -> 1
  | Cons (p1,st1),Cons (p2,st2) ->
      match HashedFault.compare p1 p2 with
      | 0 -> compare_faults st1 st2
      | r -> r


(* First argument is true when states are from the same log *)
let compare_state same st1 st2 =
  let open HashedState in
  let {S.e=e1; f=f1; a=a1;} = as_t st1.p_st
  and {S.e=e2; f=f2; a=a2;} = as_t st2.p_st in
  match compare_env e1 e2 with
  | 0 ->
      begin match compare_faults f1 f2 with
      | 0 -> assert (not same || compare_faults a1 a2 = 0) ; 0
      | r -> r
      end
  | r -> r
(* Betweenn two equal states, select the one with explicit absent faults *)
let select_absent st1 st2 =
  let open HashedState in
  let open HashedFaults in
  match
    (as_t st1.p_st).S.a.Hashcons.node,
    (as_t st2.p_st).S.a.Hashcons.node
  with
  | (Cons _,_) -> st1
  | (_,Cons _) -> st2
  | (Nil,Nil)-> st1

let state_has_fault_type st =
  let open HashedState in
  let open HashedFaults in
  let {S.e=_; f=f1; a=_;} = as_t st.p_st in
  let rec fault_type st = match st.Hashcons.node with
    | Nil -> false
    | Cons (p, st) -> HashedFault.has_fault_type p || fault_type st in
  fault_type f1

(* Select state with the most explicit information.
 * This works because explicit fault types have been
 * introduced after explicit fault absence. Thus
 * the presence of fault type implies that explicit
 * absent faults are also here (if some fault is
 * absent, of course).
 *)
let select_newer st1 st2 =
  if st1 == st2 || state_has_fault_type st1 then st1
  else if state_has_fault_type st2 then st2
  else
    (* No state has fault types, select one with explicit absent faults *)
    select_absent st1 st2

let rec do_diff_states sts1 sts2 sts2_retry do_retry = match sts1,sts2 with
| [],_ -> []
| _,[] -> if do_retry then do_diff_states sts1 sts2_retry [] false else sts1
| st1::sts1,st2::sts2 ->
   (* if st1 and st2 have a fault for the same instruction and the
    * same location (e.g. fault(P0:L0,x)), where st1 has a fault type
    * (e.g. fault(P0:L0,x,TagCheck)) and st2 doesn't
    * (e.g. fault(P0:L0,x)), we consider the two faults
    * equal. However, it's possible that sts1 might have more states
    * with the same fault of different type (e.g.,
    * fault(P0:L0,x,MMU:Translation)). These other states will be
    * compared against st2 as well. *)
    let r = compare_state false st1 st2 in
    if r < 0 then
      st1::do_diff_states sts1 (st2::sts2) sts2_retry do_retry
    else if r > 0 then
      do_diff_states (st1::sts1) sts2 sts2_retry do_retry
    else
      let sts2_retry, do_retry =
        match state_has_fault_type st1, state_has_fault_type st2 with
        | true, false -> st2::sts2_retry, true
        | _, _ -> sts2_retry, do_retry in
      do_diff_states sts1 sts2 sts2_retry do_retry

let comp_nouts sts =
  List.fold_left (fun k st -> Int64.add k st.p_noccs) Int64.zero sts

let diff_states sts1 sts2 =
  let sts = do_diff_states sts1.p_sts sts2.p_sts [] true in
  let n_outs = comp_nouts sts in
  {
   p_nouts = n_outs ;
   p_sts = sts  ;
  }

let rec do_union_states sts1 sts2 =  match sts1,sts2 with
| ([],sts)|(sts,[]) -> sts
| st1::sts1,st2::sts2 ->
    let r = compare_state false st1 st2 in
    if r < 0 then
      st1::do_union_states sts1 (st2::sts2)
    else if r > 0 then
      st2::do_union_states (st1::sts1) sts2
    else begin
      let st = select_newer st1 st2 in
      let st =
        { st with p_noccs = Int64.add st1.p_noccs  st2.p_noccs ; } in
      st::do_union_states sts1 sts2
    end

let union_states sts1 sts2 =
  {
   p_nouts = Int64.add sts1.p_nouts sts2.p_nouts ;
   p_sts = do_union_states sts1.p_sts sts2.p_sts ;
  }


module LC =
  LogConstr.Make
    (struct
      type v = ToolsConstant.v

      type state = st_concrete

      let rec bds_assoc bds loc = match bds.Hashcons.node with
      | HashedEnv.Nil -> Warn.fatal "No value for location %s" loc
      | HashedEnv.Cons (p,r) ->
          if Misc.string_eq loc (HashedBinding.get_loc p) then
            HashedBinding.get_v p
          else
            bds_assoc r loc

(*
 * Comparaison is string-based, except for integers, which are parsed.
 * That way, radix is abstracted away.
 *)
      module SL = StateLexer.Make(struct let debug = false end)

      let is_pteval v =
        let lb = Lexing.from_string v in
        try
          ignore (StateParser.pteval SL.token lb);
          true
        with
        |LexMisc.Error _|Parsing.Parse_error ->
            false

      let rec value_eq vcond v =
        let open Constant in
        match vcond with
        | Concrete _ ->
            begin try
              let i = Int64.of_string v in
              ToolsConstant.eq vcond (Constant.Concrete i)
              with Failure _ ->
                (* Some data (e.g. page tambe entries)
                   can be compared  with zero *)
                if is_pteval v && ToolsConstant.is_zero vcond then false
                else
                  Warn.fatal
                    "Comparing constant %s against integer %s"
                    v
                    (ToolsConstant.pp_v vcond)

            end
        | ConcreteVector vconds ->
            let vs = LexSplit.split_array v in
            value_eqs vconds vs
        | _ ->
            let vcond = ToolsConstant.pp false vcond in
            Misc.string_eq vcond v

      and value_eqs vconds vs = match vconds,vs with
      | [],[] -> true
      | ([],_)|(_,[])
 (* Could be a failure, as arrays are of fixed length *)
        -> false
      | vcond::vconds,v::vs ->
          value_eq vcond v && value_eqs vconds vs

      let state_mem st loc v =
        let open HashedState in
        let {S.e=bds; _;} = as_t st in
        let v_bound_pp =
          bds_assoc bds (ConstrGen.dump_rloc MiscParser.dump_location  loc) in
        value_eq v v_bound_pp

      let state_eqloc st loc1 loc2 =
        let open HashedState in
        let {S.e=bds; _;} = HashedState.as_t st in
        let v1 = bds_assoc bds (MiscParser.dump_location loc1)
        and v2 = bds_assoc bds (MiscParser.dump_location loc2) in
        Misc.string_eq v1 v2

      let state_fault st f =
        let open HashedFaults in
        let {HashedState.S.f=fs; _;} = HashedState.as_t st in
        let (p0,lbl0),v0,ftype0 = f in
        let eq_label = match lbl0 with
        | None -> fun _ -> true
        | Some lbl0 ->
            Misc.app_opt_def true (Misc.string_eq lbl0) in
        let eq_loc = match v0 with
          | None -> fun _ -> true
          | Some v0 ->
             (function
              | Some sym ->
                 Misc.string_eq  (ToolsConstant.pp_v v0) sym
              | None -> true) in
        let rec find fs = match fs.Hashcons.node with
          | Nil -> false
          | Cons (f,fs) ->
              let ((p,lbl),sym,ftype) = HashedFault.as_t f in
              let eq_ft = Fault_tools.equal_ft ftype0 ftype in
              Misc.int_eq p0 p && eq_label lbl && eq_ft && eq_loc sym ||
              find fs in
        find fs

    end)

let revalidate c sts = match c with
| None -> DontKnow
| Some c ->
  if
    LC.validate c (List.map (fun st -> st.p_st) sts.p_sts)
  then Ok
  else No

let to_exists c = ConstrGen.ExistsState (ConstrGen.prop_of c)

let witness_again c sts = match c with
| None -> Int64.zero,Int64.zero
| Some c ->
    let c = to_exists c in
    LC.witness c (List.map (fun {p_st=st; p_noccs=c} -> st,c) sts.p_sts)

let filter inv tbl t =
  let open ConstrGen in
  let xs = t.tests in
  let sz_xs = Array.length xs in
  let tout = ExtArray.create () in
  let out z = ExtArray.add tout z in
  let rec loop i_xs =
    if i_xs < sz_xs then begin
      let x = xs.(i_xs) in
      let tname = x.tname in
      let c =
        try TblRename.find_value tbl tname
        with Not_found -> match x.condition with
        | Some c -> c
        | None -> LogConstr.foralltrue in
      let p = match c with
      | ExistsState p|ForallStates p -> p
      | NotExistsState p -> ConstrGen.Not p in
      let sts =
        List.filter
          (fun st ->
            let b = LC.check_prop p st.p_st in
            if inv then not b else b)
          x.states.p_sts in
      let nouts = comp_nouts sts in
      let sts =
        { p_nouts = nouts;
          p_sts = sts; } in
      let z =
        { x with states = sts ;
          witnesses = (nouts,Int64.zero);
          condition = None; } in
      out z ;
      loop (i_xs+1)
    end in
  loop 0 ;
  ExtArray.to_array tout

let count_outcomes t =
  let xs = t.tests in
  let sz_xs = Array.length xs in
  let rec loop k i_xs =
    if i_xs < sz_xs then begin
      let x = xs.(i_xs) in
      let k = k + (List.length x.states.p_sts) in
      loop k (i_xs+1)
    end else k in
  loop 0 0

(* Sum of logs *)

(**********************)
(* Union of log files *)
(**********************)

(* As union_test is used in two places, error report is by exception *)
type union_error =
  | NoHashLeft
  | NoHashRight
  | DiffHash
  | State of string

exception Error of union_error

let error e = raise (Error e)

let union_kind tname k1 k2 = match k1,k2 with
| (NoKind,k)|(k,NoKind) -> k
| (ErrorKind,_)|(_,ErrorKind) -> ErrorKind
| _ ->
    W.warn "Test %s, kind error: %s<>%s"
      tname (pp_kind k1) (pp_kind k2) ;
    ErrorKind

let save_cond c1 c2 = match c1,c2 with
| Some x1,Some x2 ->
  if ConstrGen.prop_of x1 = ConstrGen.prop_of x2 then
    c1
  else
    None
| Some _ as x,None
| None,(Some _ as x) -> x
| None,None -> None

let union_cond tname c1 c2 =
  if c1 <> c2 then begin
    let c = save_cond c1 c2 in
    begin match c with
    | None ->
        W.warn "Test %s, changing condition" tname  ;
        c
    | Some _ -> c
    end
  end else
    c1

(*
let p_hash = function
  | None -> "-"
  | Some h -> "+" ^ h
*)

let strict = true

let union_hash h1 h2 = match h1,h2 with
| None,h ->
    if strict then error NoHashLeft ;
    h
|(h,None) ->
    if strict then error NoHashRight ;
    h
| (Some "NOHASH",h)
| (h,Some "NOHASH")
  -> h
| Some s1,Some s2 ->
    if not (Misc.string_eq s1 s2) then error DiffHash ;
    h1

let union_time t1 t2 = match t1,t2 with
  | (None,_)|(_,None) -> None
  | Some t1,Some t2 -> Some (t1 +. t2)

let gt0 n = Int64.compare n Int64.zero > 0

let rec union_topos xs ys = match xs,ys with
| (zs,[])|([],zs) -> zs
| x::rx,y::ry ->
    let (tx,nx) = x and (ty,ny) = y in
    let c = HashedString.compare tx ty in
    if c < 0 then x::union_topos rx ys
    else if c > 0 then y::union_topos xs ry
    else (tx,Int64.add nx ny)::union_topos rx ry


let union_test_gen t1 t2 =
  assert (t1.tname = t2.tname) ;
  let k =
    if (t1.kind <> t2.kind) then begin
      union_kind t1.tname t1.kind t2.kind
    end else
      t1.kind in
  let cond = union_cond t1.tname t1.condition t2.condition in
  let hash = union_hash t1.hash t2.hash in
  let (p1,n1) = t1.witnesses and (p2,n2) = t2.witnesses in
  let p = Int64.add p1 p2 and n = Int64.add n1 n2 in
  let v = match k with
  | Allow -> if gt0 p then Ok else No
  | Forbid | Require -> if gt0 n then No else Ok
  | _ -> DontKnow in

  if O.verbose > 1 then
    eprintf "k=%s, p=%s, n=%s, v=%s\n%!"
      (pp_kind k) (Int64.to_string p) (Int64.to_string n) (pp_validation v) ;

  let sts =
    try union_states t1.states t2.states
    with StateMismatch loc -> error (State loc) in

  let topos = union_topos t1.topologies t2.topologies in

  let time = union_time  t1.time t2.time in
  {
   tname = t1.tname ;
   kind = k ;
   condition = cond ;
   states = sts ;
   validation = v ;
   witnesses = (p,n) ;
   loop = t1.loop || t2.loop ;
   hash = hash ;
   time = time ;
   topologies = topos ;
 }

(* pp of a list of file names *)
let rec pp_files = function
  | [] -> ""
  | [f] -> Filename.basename f
  | f::fs -> Filename.basename f ^ "," ^ pp_files fs

let union_test f1 f2 t1 t2 =
  try union_test_gen t1 t2 with
  | Error e ->
      begin match e with
      | NoHashLeft ->
          Warn.fatal "Hash missing for test %s in file(s) %s"
            t1.tname (pp_files f1)
      | NoHashRight ->
          Warn.fatal "Hash missing for test %s in file(s) %s"
            t1.tname (pp_files f2)
      | DiffHash ->
          Warn.fatal "Hash mismatch for test %s in file(s) %s<>%s"
            t1.tname (pp_files f1) (pp_files f2)
      | State loc ->
          Warn.fatal
            "Incompatible outcomes for location %s in test %s (files %s<>%s)"
            loc t1.tname (pp_files f1) (pp_files f2)
      end

let union_logs ns ms xs ys =
  let sz_xs = Array.length xs
  and sz_ys = Array.length ys in
  let tout = ExtArray.create () in
  let out x = ExtArray.add tout x in
  let rec loop i_xs i_ys =
    if i_xs >= sz_xs then ExtArray.blit tout ys i_ys (sz_ys-i_ys)
    else if i_ys >= sz_ys then  ExtArray.blit tout xs i_xs (sz_xs-i_xs)
    else
      let x = xs.(i_xs) and y = ys.(i_ys) in
      let c = String.compare x.tname y.tname in
      if c < 0 then begin
        out x ; loop (i_xs+1) i_ys
      end else if c > 0 then begin
        out y ; loop i_xs (i_ys+1)
      end else begin
        out (union_test ns ms x y) ;
        loop (i_xs+1) (i_ys+1)
      end in
  loop 0 0 ;
  ExtArray.to_array tout

let rec do_unions = function
| ([]|[_]) as r -> r
| (ns,xs)::(ms,ys)::rem ->
    let rs = ns@ms in (rs,union_logs ns ms xs ys)::do_unions rem

let rec unions a = match a with
  | [] -> [| |]
  | [_,xs] -> xs
  | xss -> unions (do_unions xss)


let unions0 ts =
  List.map
    (fun t ->
      if not t.is_litmus then
        Warn.warn_always "File %s is not a litmus log" t.name ;
      let tsts = Array.copy t.tests in
      Array.sort
        (fun t1 t2 -> String.compare t1.tname t2.tname)
        tsts ;
      [t.name],tsts)
    ts


let union_logs all = unions (unions0 all)

(* Difference of two logs *)

let do_diff_test t1 t2 =
  assert (t1.tname = t2.tname) ;
  let k = NoKind in
  let cond = None in
  let hash = union_hash t1.hash t2.hash in
  let v = DontKnow in

  let sts =
    try diff_states t1.states t2.states
    with StateMismatch loc -> error (State loc) in

  let topos = [] in
  {
   tname = t1.tname ;
   kind = k ;
   condition = cond ;
   states = sts ;
   validation = v ;
   witnesses = (Int64.zero,Int64.zero) ;
   loop = t1.loop || t2.loop ;
   hash = hash ;
   time = None ;
   topologies = topos ;
 }


let diff_test f1 f2 t1 t2 =
  try do_diff_test t1 t2 with
  | Error e ->
      begin match e with
      | NoHashLeft ->
          Warn.fatal "Hash missing for test %s in file(s) %s"
            t1.tname f1
      | NoHashRight ->
          Warn.fatal "Hash missing for test %s in file(s) %s"
            t1.tname f2
      | DiffHash ->
          Warn.fatal "Hash mismatch for test %s in file(s) %s/%s"
            t1.tname f1 f2
      | State loc ->
          Warn.fatal
            "Incompatible outcomes for location %s in test %s (files %s<>%s)"
            loc t1.tname f1 f2
      end

let diff_tests nx ny emptyok xs ys =
  let sz_xs = Array.length xs
  and sz_ys = Array.length ys in
  let tout = ExtArray.create () in
  let out x = ExtArray.add tout x in
  let rec loop i_xs i_ys =
    if i_xs >= sz_xs then ExtArray.blit tout ys i_ys (sz_ys-i_ys)
    else if i_ys >= sz_ys then  ()
    else
      let x = xs.(i_xs) and y = ys.(i_ys) in
      let c = String.compare x.tname y.tname in
      if c < 0 then begin
        out x ; loop (i_xs+1) i_ys
      end else if c > 0 then begin
        loop i_xs (i_ys+1)
      end else begin
        let z = diff_test nx ny x y in
        if emptyok || z.states.p_sts <> [] then out z ;
        loop (i_xs+1) (i_ys+1)
      end in
  loop 0 0 ;
  ExtArray.to_array tout

let diff_logs emptyok t1 t2 =
  diff_tests t1.name t2.name emptyok t1.tests t2.tests

(* Intersection of two logs *)

let rec do_inter_states sts1 sts2 =  match sts1,sts2 with
| ([],_)|(_,[]) -> []
| st1::sts1,st2::sts2 ->
    let r = compare_state false st1 st2 in
    if r < 0 then
      do_inter_states sts1 (st2::sts2)
    else if r > 0 then
      do_inter_states (st1::sts1) sts2
    else begin
      let st = st1 in (* Consider second log as some filter *)
      st::do_inter_states sts1 sts2
    end

let inter_states sts1 sts2 =
  let sts =do_inter_states sts1.p_sts sts2.p_sts in
  {
   p_nouts = comp_nouts sts ;
   p_sts = sts ;
  }

let do_inter_test t1 t2 =
  assert (t1.tname = t2.tname) ;
  let k = NoKind in
  let cond = None in
  let hash = union_hash t1.hash t2.hash in
  let v = DontKnow in

  let sts =
    try inter_states t1.states t2.states
    with StateMismatch loc -> error (State loc) in

  {
   tname = t1.tname ;
   kind = k ;
   condition = cond ;
   states = sts ;
   validation = v ;
   witnesses = (Int64.zero,Int64.zero) ;
   loop = t1.loop || t2.loop ;
   hash = hash ;
   time = None ;
   topologies = [] ;
 }


let inter_test f1 f2 t1 t2 =
  try do_inter_test t1 t2 with
  | Error e ->
      begin match e with
      | NoHashLeft ->
          Warn.fatal "Hash missing for test %s in file(s) %s"
            t1.tname f1
      | NoHashRight ->
          Warn.fatal "Hash missing for test %s in file(s) %s"
            t1.tname f2
      | DiffHash ->
          Warn.fatal "Hash mismatch for test %s in file(s) %s/%s"
            t1.tname f1 f2
      | State loc ->
          Warn.fatal
            "Incompatible outcomes for location %s in test %s (files %s<>%s)"
            loc t1.tname f1 f2
      end

let inter_tests nx ny emptyok xs ys =
  let sz_xs = Array.length xs
  and sz_ys = Array.length ys in
  let tout = ExtArray.create () in
  let out x = ExtArray.add tout x in
  let rec loop i_xs i_ys =
    if i_xs >= sz_xs then ()
    else if i_ys >= sz_ys then  ()
    else
      let x = xs.(i_xs) and y = ys.(i_ys) in
      let c = String.compare x.tname y.tname in
      if c < 0 then begin
        loop (i_xs+1) i_ys
      end else if c > 0 then begin
        loop i_xs (i_ys+1)
      end else begin
        let z = inter_test nx ny x y in
        begin match z.states.p_sts with
        | [] -> if emptyok then out z
        | _ -> out z
        end ;
        loop (i_xs+1) (i_ys+1)
      end in
  loop 0 0 ;
  ExtArray.to_array tout

let inter_logs emptyok t1 t2 = inter_tests t1.name t2.name emptyok t1.tests t2.tests


(* Apply union_test on adjacent tests with identical names in arrays *)

let union_adjs union ts =
  let sz = Array.length ts in
  if sz > 0 then begin
    let out = ExtArray.create () in
    let rec loop p i =
      if i >= sz then ExtArray.add out p
      else begin
        let q = ts.(i) in
        if String.compare p.tname q.tname  != 0 then begin
          ExtArray.add out p ;
          loop q (i+1)
        end else
          loop (union p q) (i+1)
      end in
    loop ts.(0) 1 ;
    ExtArray.to_array out
  end else ts

(* Renormalise, after name change *)
let reuniq ts = union_adjs  (union_test [] []) ts

let renormalize tests =
  Array.sort
    (fun {tname=n1;_} {tname=n2;_} -> String.compare n1 n2)
    tests ;
  reuniq tests


let rename f t =

  let old_hashes =
    let htbl = Hashtbl.create 17 in
    Array.iter
      (fun t -> match t.hash with
      | None -> ()
      | Some h -> Hashtbl.add htbl t.tname h)
      t.tests ;
  htbl in

  let tests =
    Array.map
      (fun t ->
        let n = f t.tname in
        if n = t.tname then t
        else match t.hash with
        | None -> { t with tname = n; }
        | Some h ->
            begin try
              if h <> Hashtbl.find old_hashes n then
                Warn.fatal
                  "Cannot rename %s into %s, hash mismatch"
                  t.tname n
            with Not_found -> () end ;
          { t with tname = n;})
      t.tests in
  let tests = renormalize tests in
  { t with tests = tests; }

let array_filter p ts =
  let out = ExtArray.create () in
  let sz = Array.length ts in
  let rec loop i =
    if i < sz then begin
      let x = ts.(i) in
      if p x then ExtArray.add out x ;
      loop (i+1)
    end in
  loop 0 ;
  ExtArray.to_array out

let exclude e t =
  let tests =
    array_filter
      (fun t -> not (Str.string_match e t.tname 0))
      t.tests in
  { t with tests = tests; }

(*************)
(* Normalize *)
(*************)

(* No duplicates in normalized lists of states *)

let rec  normalize_sts_uniq cmp sts = match sts with
  | []|[_] -> sts
  | st1::(st2::_ as sts) ->
     if cmp st1 st2=0 then
       normalize_sts_uniq cmp sts
     else
       st1::normalize_sts_uniq cmp sts

let normalize_sts_gen cmp sts = normalize_sts_uniq cmp (List.sort cmp sts)

let normalize_sts = normalize_sts_gen (compare_state true)

let normalize_states sts =
  let p_sts = normalize_sts sts in
  let n_outs = comp_nouts p_sts in
  { p_nouts = n_outs ;  p_sts = p_sts ; }

let uniq _is_litmus name =
  let union_test t1 t2 =
    if (t1.kind <> t2.kind) then
      Warn.fatal
        "Different kinds for test %s in file %s"
        t1.tname name ;
    let k = t1.kind in

    if (t1.condition <> t2.condition) then
      Warn.fatal
        "Different conditions for test %s in file %s"
        t1.tname name ;

    let hash = match t1.hash,t2.hash with
    | (None,h)|(h,None) -> h
    | Some _h1,Some _h2 ->
        if t1.hash <> t2.hash then
          Warn.fatal
            "Different hash codes for test %s in file %s"
            t1.tname name ;
        t1.hash in

    let cond = t1.condition in
    let (p1,n1) = t1.witnesses
    and (p2,n2) = t2.witnesses in
    let p = Int64.add p1 p2 and n = Int64.add n1 n2 in
    let v = match k with
    | Allow ->
        if gt0 p then Ok else No
    | Forbid | Require -> if gt0 n then No else Ok
    | NoKind|Undefined -> DontKnow
    | ErrorKind -> assert false in
    { tname = t1.tname ; condition=cond; kind=k ;
      states = union_states t1.states t2.states ;
      validation=v ;
      witnesses=(p,n) ;
      loop = t1.loop || t2.loop ;
      hash = hash ;
      time = None ;
      topologies = union_topos t1.topologies t2.topologies; } in

  fun ts -> union_adjs union_test ts

let union_same_log merge ts =
  Array.sort (fun t1 t2 -> String.compare t1.tname t2.tname) ts ;
  union_adjs merge ts

let union_litmus name t1 t2 =
  try union_test_gen t1 t2 with
  | Error e ->
      begin match e with
      | NoHashLeft|NoHashRight ->
          Warn.fatal "Hash missing for test %s in file %s" t1.tname name
      | DiffHash ->
          Warn.fatal "Hash mismatch for test %s in file %s" t1.tname name
      | State loc ->
          Warn.fatal
            "Incompatible outcomes for location %s in test %s (file %s)"
            loc t1.tname name
      end

let union_equals name t1 t2 =
  if equal_test t1 t2 then t1
  else begin
    W.warn "Different results for test %s in file %s" t1.tname name ;
    union_litmus name t1 t2
  end

let normalize_topos topos =
  List.sort (fun (n1,_) (n2,_) -> HashedString.compare n1 n2) topos

let normalize name is_litmus ts =
  let ts = Array.of_list ts in
  let ts =
    Array.map
      (fun (n,k,(sts,ok,wits,cond,loop,hash,topos,time)) ->
        { tname = n ; kind=k ;
          states = normalize_states sts ; validation=ok ;
          witnesses = wits ;
          condition = cond ;
          loop = loop ;
          hash = hash ;
          time = time ;
          topologies = normalize_topos topos ;
        })
      ts in
(* This first step on unsorted tests catches old litmus logs
   with missing hashes *)
  let ts = uniq is_litmus name ts in
(* Now union tests in logs *)
  let ts =
    union_same_log
      (if is_litmus then
        union_litmus name
      else
        union_equals name)
      ts in
  { name = name ;
    is_litmus = is_litmus ;
    tests = ts ; }


let compare_simple_st st1 st2 = compare st1.Hashcons.tag st2.Hashcons.tag

let norm_states = normalize_sts_gen compare_simple_st

let union_same_log_simple merge =
  let rec merge2 xs ys = match xs,ys with
  | ([],r)|(r,[]) -> r
  | x::rx,y::ry ->
      let c = String.compare x.s_tname y.s_tname in
      if c < 0 then x::merge2 rx ys
      else if c > 0 then y::merge2 xs ry
      else merge x y::merge2 rx ry  in

  let rec do_unions = function
  | []|[_] as r -> r
  | x::y::rem -> merge2 x y::do_unions rem in

  let rec loop = function
    | [] -> []
    | [r] -> r
    | xss -> loop (do_unions xss) in

  fun ts ->
    let xss = List.fold_left (fun k t -> [t]::k) [] ts in
    loop xss


let rec union_states_simple sts1 sts2 =  match sts1,sts2 with
| ([],sts)|(sts,[]) -> sts
| st1::sts1,st2::sts2 ->
    let r = compare_simple_st st1 st2 in
    if r < 0 then
      st1::union_states_simple sts1 (st2::sts2)
    else if r > 0 then
      st2::union_states_simple (st1::sts1) sts2
    else begin
      st1::union_states_simple sts1 sts2
    end

let do_union_litmus_simple t1 t2 =
  assert (t1.s_tname = t2.s_tname) ;
  let sts =
    try union_states_simple t1.s_states t2.s_states
    with  StateMismatch loc -> error (State loc) in
  if t1.s_hash <> t2.s_hash then error DiffHash ;
  { t1 with s_states = sts ; }

(*
let union_equal_simple _name t1 t2 = assert (t1 = t2) ;  t1
*)

let union_litmus_simple name t1 t2 =
  try do_union_litmus_simple t1 t2
  with Error e ->
    begin match e with
    | NoHashLeft|NoHashRight ->
        assert false (* Same log, hence impossible *)
    | DiffHash ->
        Warn.fatal "Hash mismatch for test %s in file %s" t1.s_tname name
    | State loc ->
        Warn.fatal
          "Incompatible outcomes for location %s in test %s (file %s)"
          loc t1.s_tname name
      end

let normalize_simple name _is_litmus ts =
  let ts =
    List.map
      (fun (n,sts,hash) ->
        let hash = match hash with
        | Some h -> h
        | None -> Warn.fatal "No hash for test %s\n" name in
        { s_tname = n ;
          s_states = norm_states sts ;
          s_hash = hash ; })
      ts in
  let ts =
    union_same_log_simple
(*      (if is_litmus then union_litmus_simple name else union_equal_simple name) *)
      (union_litmus_simple name)
      ts in
  { s_name = name ; s_tests = ts ; }


let simple_same out1 out2 t1 t2 k =
  let rec do_rec ts1 ts2 k = match ts1,ts2 with
  | [],[] -> k
  | [],t2::r2 -> do_rec [] r2 (out2 t2.s_tname k)
  | t1::r1,[] -> do_rec r1 [] (out1 t1.s_tname k)
  | t1::r1,t2::r2 ->
      let c = String.compare t1.s_tname t2.s_tname in
      if c < 0 then do_rec r1 ts2 (out1 t1.s_tname k)
      else if c > 0 then do_rec ts1 r2 (out2 t2.s_tname k)
      else if String.compare t1.s_hash t2.s_hash != 0 then
        Warn.fatal "Hashes for test %s differ\n" t1.s_tname
      else do_rec r1 r2 k in
  do_rec t1.s_tests t2.s_tests k

let simple_diff_gen diff out t1 t2 k =
 let rec do_diff ts1 ts2 k = match ts1,ts2 with
  | ([],_)|(_,[]) -> k
  | t1::r1,t2::r2 ->
      let c = String.compare t1.s_tname t2.s_tname in
      if c < 0 then do_diff r1 ts2 k
      else if c > 0 then do_diff ts1 r2 k
      else if String.compare t1.s_hash t2.s_hash != 0 then
        Warn.fatal "Hashes for test %s differ\n" t1.s_tname
      else
        do_diff r1 r2
          (if diff t1.s_states t2.s_states then begin
            out t1.s_tname k
          end else k) in
  do_diff t1.s_tests t2.s_tests k

(* Answers true if X/Y not empty *)
let rec diff_not_empty  xs ys = match xs,ys with
| [],_ -> false
| _,[] -> true
| x::rx,y::ry ->
    let tx = x.Hashcons.tag and ty = y.Hashcons.tag in
    if tx < ty then true
    else if tx > ty then diff_not_empty xs ry
    else diff_not_empty rx ry

let simple_diff_not_empty out t1 t2 k =
  simple_diff_gen diff_not_empty out t1 t2 k


let rec diff_simple_states xs ys = match xs,ys with
| [],[] -> false
| x::xs,y::ys -> x != y || diff_simple_states xs ys
| _,_ -> true

let simple_diff out t1 t2 k =
  simple_diff_gen diff_simple_states out t1 t2 k
end
