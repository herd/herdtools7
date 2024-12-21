(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Interpreter for a user-specified model *)

open Printf

module Extract = TxtLoc.Extract ()

module type Config = sig
  val m : AST.t
  val bell : bool (* executing bell file *)
  val bell_fname : string option (* name of bell file if present *)
(* Restricted Model.Config *)
  val showsome : bool
  val debug : bool
  val debug_files : bool
  val profile: bool
  val verbose : int
  val skipchecks : StringSet.t
  val strictskip : bool
  val cycles : StringSet.t
  val compat : bool
(* Show control *)
  val doshow : StringSet.t
  val showraw : StringSet.t
  val symetric : StringSet.t
(* find files *)
  val libfind : string -> string
(* check variant *)
  val variant : string -> bool
end

(* Simplified Sem module.
   In effect, the interpreter needs a restricted subset of Sem functionalities:
   set of events, relation on events and that is about all.
   A few utilities are passed as the next "U" argument to functor. *)
module type SimplifiedSem = sig
  module E : sig
    type event
    val event_compare : event -> event -> int
    val pp_eiid : event -> string
    val pp_instance : event -> string
    val is_store : event -> bool
    val is_pt : event -> bool

    module EventSet : MySet.S
    with type elt = event

    module EventRel : InnerRel.S
    with type elt0 = event
    and module Elts = EventSet

    module EventMap : MyMap.S
    with type key = event
  end

  type test
  type concrete

  type event = E.event
  type event_set = E.EventSet.t
  type event_rel = E.EventRel.t
  type rel_pp = (string * event_rel) list
  type set_pp = event_set StringMap.t
end

module Make
    (O:Config)
    (S:SimplifiedSem)
    (U: sig
      val partition_events : S.event_set -> S.event_set list
      val loc2events : string -> S.event_set -> S.event_set
      val check_through : bool -> bool
      val pp_failure : S.test -> S.concrete -> string -> S.rel_pp -> unit
      val pp : S.test -> S.concrete -> string -> S.rel_pp -> unit
      val fromto :
          S.event_rel -> (* po *)
            S.event_set (* labelled fence(s) *) ->
              S.event_rel (* localised fence relation *)
      val same_value : S.event -> S.event -> bool
      val same_oa : S.event -> S.event -> bool
      val writable2 : S.event -> S.event -> bool
    end)
    :
    sig

(* Some constants passed to the interpreter, made open for the
   convenience of building them from outside *)
      type ks =
          { id : S.event_rel Lazy.t; unv : S.event_rel Lazy.t;
            evts : S.event_set;  conc : S.concrete; po:S.event_rel;}


(* Initial environment, they differ from internal env, so
   as not to expose the polymorphic argument of the later *)
      type init_env
      val init_env_empty : init_env
      val add_rels : init_env -> S.event_rel Lazy.t Misc.Simple.bds -> init_env
      val add_sets : init_env -> S.event_set Lazy.t Misc.Simple.bds -> init_env
      val get_set : init_env -> string -> S.event_set Lazy.t option

(* Subset of interpreter state used by the caller *)
      type st_out = {
          out_show : S.rel_pp Lazy.t ;
          out_sets : S.set_pp Lazy.t ;
          out_skipped : StringSet.t ;
          out_flags : Flag.Set.t ;
          out_bell_info :  BellModel.info ;
        }


(* Interpreter *)

      val interpret :
          S.test -> ('a -> 'a) ->
            ks ->
              init_env ->
                S.rel_pp Lazy.t ->
                  (st_out -> 'a -> 'a) -> 'a -> 'a
    end
    =
  struct

    let _dbg = false

    let () =
      if _dbg then match O.bell_fname with
      | None ->  eprintf "Interpret has no bell file\n"
      | Some fname -> eprintf "Interpret bell file is %s\n" fname

    let next_id =
      let id = ref 0 in
      fun () -> let r = !id in id := r+1 ; r


(****************************)
(* Convenient abbreviations *)
(****************************)

    module E = S.E
    module W = Warn.Make(O)
(* Add relations amongst event classes, notice that a class is an event set,
   regardless of class disjointness *)
    module ClassRel = InnerRel.Make(E.EventSet)

(* Check utilities *)
    open AST

    let skip_this_check name = match name with
    | Some name -> StringSet.mem name O.skipchecks
    | None -> false

    let cycle_this_check name = match name with
    | Some name -> StringSet.mem name O.cycles
    | None -> false

    let check_through test_type ok = match test_type with
    | Check ->  U.check_through ok
    | UndefinedUnless|Flagged|Assert -> ok


(*  Model interpret *)
    let (_,_,mprog) = O.m

(* Debug printing *)

    let _debug_proc chan p = fprintf chan "%i" p
    let debug_event chan e = fprintf chan "%s" (E.pp_eiid e)
    let debug_set chan s =
      output_char chan '{' ;
      E.EventSet.pp chan "," debug_event s ;
      output_char chan '}'

    let debug_rel chan r =
      E.EventRel.pp chan ","
        (fun chan (e1,e2) -> fprintf chan "%a -> %a"
            debug_event e1 debug_event e2)
        r

    let debug_class_rel chan r =
      ClassRel.pp chan ","
        (fun chan (e1,e2) -> fprintf chan "%a -> %a"
            debug_set e1 debug_set e2)
        r

    type ks =
        { id : S.event_rel Lazy.t; unv : S.event_rel Lazy.t;
          evts : S.event_set; conc : S.concrete; po:S.event_rel; }

(* Internal typing *)
    type typ =
      | TEmpty | TEvent | TEvents | TPair | TRel | TClassRel
      | TTag of string |TClo | TProc | TSet of typ
      | TTuple of typ list

    let rec eq_type t1 t2 = match t1,t2 with
    | (TEmpty,(TSet _ as t))
    | ((TSet _ as t),TEmpty) -> Some t
    | (TEvents,TEvents)
    | (TEmpty,TEvents)
    | (TEvents,TEmpty) -> Some TEvents
    | (TRel,TRel)
    | (TEmpty,TRel)
    | (TRel,TEmpty) -> Some TRel
    | (TClassRel,TClassRel)
    | (TEmpty,TClassRel)
    | (TClassRel,TEmpty) -> Some TClassRel
    | TEvent,TEvent -> Some TEvent
    | TPair,TPair -> Some TPair
    | TTag s1,TTag s2 when s1 = s2 -> Some t1
    | TSet t1,TSet t2 ->
        begin match eq_type t1 t2 with
        | None -> None
        | Some t -> Some (TSet t)
        end
    | TTuple ts1,TTuple ts2 ->
        Misc.app_opt (fun ts -> TTuple ts) (eq_types ts1 ts2)
    | TClo,TClo -> Some TClo
    | TProc,TProc -> Some TProc
    | _,_ -> None


    and eq_types ts1 ts2 = match ts1,ts2 with
    | [],[] -> Some []
    | ([],_::_)|(_::_,[]) -> None
    | t1::ts1,t2::ts2 ->
        begin
          let ts = eq_types ts1 ts2
          and t = eq_type t1 t2 in
          match t,ts with
          | Some t,Some ts -> Some (t::ts)
          | _,_ -> None
        end

    let type_equal t1 t2 = match eq_type t1 t2 with
    | None -> false
    | Some _ -> true

    exception CompError of string
    exception PrimError of string


    let rec pp_typ = function
      | TEmpty -> "{}"
      | TEvent -> "event"
      | TEvents -> "events"
      | TPair -> "pair"
      | TRel -> "rel"
      | TClassRel -> "classrel"
      | TTag ty -> ty
      | TClo -> "closure"
      | TProc -> "procedure"
      | TSet elt -> sprintf "%s set" (pp_typ elt)
      | TTuple ts ->
          sprintf "(%s)" (String.concat " * " (List.map pp_typ ts))

    module rec V : sig

      type v =
        | Empty | Unv
        | Pair of (S.event * S.event)
        | Rel of S.event_rel
        | ClassRel of ClassRel.t
        | Event of S.event
        | Set of S.event_set
        | Clo of  closure
        | Prim of string * int * (v -> v)
        | Proc of  procedure
        | Tag of string * string     (* type  X name *)
        | ValSet of typ *  ValSet.t   (* elt type X set *)
        | Tuple of  v list

      and env =
          { vals  : v Lazy.t StringMap.t;
            enums : string list StringMap.t;
            tags  : string StringMap.t; }
      and closure =
          { clo_args : AST.pat ;
            mutable clo_env : env ;
            clo_body : AST.exp;
            clo_name : string * int; } (* unique id (hack) *)

      and procedure = {
          proc_args : AST.pat ;
          mutable proc_env : env;
          proc_body : AST.ins list; }

      val type_val : v -> typ

    end = struct

      type v =
        | Empty | Unv
        | Pair of (S.event * S.event)
        | Rel of S.event_rel
        | ClassRel of ClassRel.t
        | Event of S.event
        | Set of S.event_set
        | Clo of  closure
        | Prim of string * int * (v -> v)
        | Proc of  procedure
        | Tag of string * string     (* type  X name *)
        | ValSet of typ *  ValSet.t   (* elt type X set *)
        | Tuple of v list

      and env =
          { vals  : v  Lazy.t StringMap.t;
            enums : string list StringMap.t;
            tags  : string StringMap.t; }
      and  closure =
          { clo_args : AST.pat ;
            mutable clo_env : env ;
            clo_body : AST.exp;
            clo_name : string * int; } (* unique id (hack) *)

      and  procedure = {
          proc_args : AST.pat ;
          mutable proc_env : env;
          proc_body : AST.ins list; }

      let rec type_val = function
        | V.Empty -> TEmpty
        | Unv -> assert false (* Discarded before *)
        | Pair _ -> TPair
        | Rel _ -> TRel
        | ClassRel _ -> TClassRel
        | Event _ -> TEvent
        | Set _ -> TEvents
        | Clo _|Prim _ -> TClo
        | Proc _ -> TProc
        | Tag (t,_) -> TTag t
        | ValSet (t,_) -> TSet t
        | Tuple vs -> TTuple (List.map type_val vs)

    end
    and ValOrder : Set.OrderedType with type t = V.v = struct
      (* Note: cannot use Full in sets.. *)
      type t = V.v
      open V

      let error fmt = ksprintf (fun msg -> raise (CompError msg)) fmt


      let rec compare v1 v2 = match v1,v2 with
      | V.Empty,V.Empty -> 0
(* Expand all legitimate empty's *)
      | V.Empty,ValSet (_,s) -> ValSet.compare ValSet.empty s
      | ValSet (_,s),V.Empty -> ValSet.compare s ValSet.empty
      | V.Empty,Rel r -> E.EventRel.compare E.EventRel.empty r
      | Rel r,V.Empty -> E.EventRel.compare r E.EventRel.empty
      | V.Empty,Set s -> E.EventSet.compare E.EventSet.empty s
      | Set s,V.Empty -> E.EventSet.compare s E.EventSet.empty
      | V.Empty,ClassRel r -> ClassRel.compare ClassRel.empty r
      | ClassRel r,V.Empty -> ClassRel.compare r ClassRel.empty
            (* Legitimate cmp *)
      | Tag (_,s1), Tag (_,s2) -> String.compare s1 s2
      | Event e1,Event e2 -> E.event_compare e1 e2
      | ValSet (_,s1),ValSet (_,s2) -> ValSet.compare s1 s2
      | Rel r1,Rel r2 -> E.EventRel.compare r1 r2
      | ClassRel r1,ClassRel r2 -> ClassRel.compare r1 r2
      | Set s1,Set s2 -> E.EventSet.compare s1 s2
      | (Clo {clo_name = (_,i1);_},Clo {clo_name=(_,i2);_})
      | (Prim (_,i1,_),Prim (_,i2,_)) -> Misc.int_compare i1 i2
      | Clo _,Prim _ -> 1
      | Prim _,Clo _ -> -1
      | Tuple vs,Tuple ws ->  compares vs ws
(* Errors *)
      | (Unv,_)|(_,Unv) -> error "Universe in compare"
      | _,_ ->
          let t1 = V.type_val v1
          and t2 = V.type_val v2 in
          if type_equal t1 t2 then
            error "Sets of %s are illegal" (pp_typ t1)
          else
            error
              "Heterogeneous set elements: types %s and %s "
              (pp_typ t1) (pp_typ t2)

      and compares vs ws = match vs,ws with
      | [],[] -> 0
      | [],_::_ -> -1
      | _::_,[] -> 1
      | v::vs,w::ws ->
          begin match compare v w with
          | 0 -> compares vs ws
          | r -> r
          end

    end and ValSet : (MySet.S with type elt = V.v) = MySet.Make(ValOrder)

    type fix = CheckFailed of V.env | CheckOk of V.env

    let error silent loc fmt =
      ksprintf
        (fun msg ->
          if O.debug || not silent then eprintf "%a: %s\n" TxtLoc.pp loc msg ;
          raise Misc.Exit) (* Silent failure *)
        fmt

    let error_not_silent loc fmt = error false loc fmt

    let warn loc fmt =
      ksprintf
        (fun msg ->
          Warn.warn_always "%a: %s" TxtLoc.pp loc msg)
        fmt

    open V

(* pretty *)
    let pp_type_val v = pp_typ (type_val v)

    let rec pp_val = function
      | Unv -> "<universe>"
      | V.Empty -> "{}"
      | Tag (_,s) -> sprintf "'%s" s
      | ValSet (_,s) ->
          sprintf "{%s}" (ValSet.pp_str "," pp_val s)
      | Clo {clo_name=(n,x);_} ->
          sprintf "%s_%i" n x
      | V.Tuple vs ->
          sprintf "(%s)" (String.concat "," (List.map pp_val vs))
      | v -> sprintf "<%s>" (pp_type_val v)

    let rec debug_val_set chan s =
      output_char chan '{' ;
      ValSet.pp chan "," debug_val s ;
      output_char chan '}'

    and debug_val chan = function
      | ValSet (_,s) -> debug_val_set chan s
      | Set es ->  debug_set chan es
      | Rel r  -> debug_rel chan r
      | ClassRel r -> debug_class_rel chan r
      | v -> fprintf chan "%s" (pp_val v)

(* lift a tag to a singleton set *)
    let tag2set v = match v with
    | V.Tag (t,_) -> ValSet (TTag t,ValSet.singleton v)
    | _ -> v

(* extract lists from tuples *)
    let pat_as_vars = function
      | Pvar x -> [x]
      | Ptuple xs -> xs

    let v_as_vs = function
      | V.Tuple vs -> vs
      | Pair (ev1,ev2) -> [Event ev1;Event ev2;]
      | v -> [v]

    let pat2empty = function
      | Pvar _ -> V.Empty
      | Ptuple xs -> V.Tuple (List.map (fun _ -> V.Empty) xs)

    let bdvar = function
      | None -> StringSet.empty
      | Some x -> StringSet.singleton x

    let bdvars (_,pat,_) = match pat with
    | Pvar x -> bdvar x
    | Ptuple xs -> StringSet.unions (List.map bdvar xs)


(* Add values to env *)
    let do_add_val k v env =
      { env with vals = StringMap.add k v env.vals; }

    let add_val k v env = match k with
    | None -> env
    | Some k -> do_add_val k v env

    let add_pat_val silent loc pat v env = match pat with
    | Pvar k -> add_val k v env
    | Ptuple ks ->
        let rec add_rec extract env = function
          | [] -> env
          | k::ks ->
              let vk =
                lazy
                  begin match extract (Lazy.force v) with
                  | v::_ -> v
                  | [] -> error silent loc "%s" "binding mismatch"
                  end in
              let env = add_val k vk env
              and extract v = match extract v with
              | _::vs -> vs
              | [] -> error silent loc "%s" "binding mismatch" in
              add_rec extract env ks in
        add_rec v_as_vs env ks

    let env_empty =
      {vals=StringMap.empty;
       enums=StringMap.empty;
       tags=StringMap.empty; }

(* Initial env, a restriction of env *)

    type init_env =
        E.EventSet.t Lazy.t Misc.Simple.bds *
          E.EventRel.t Lazy.t Misc.Simple.bds

    let init_env_empty = [],[]

    let add_rels (sets,rels) bds = (sets,bds@rels)

    and add_sets (sets,rels) bds = (bds@sets,rels)

    let get_set (sets,_) key =
      let rec f_rec = function
        | [] -> None
        | (k,v)::sets ->
            if Misc.string_eq key k then Some v
            else f_rec sets in
      f_rec sets

(* Go on *)
    let add_vals_once mk =
      List.fold_right
        (fun (k,v) m ->
          if StringMap.mem k m then
            Warn.warn_always
              "redefining key '%s' in cat interpreter initial environment"
              k ;
          StringMap.add k (mk v) m)

    let env_from_ienv (sets,rels) =
      let vals =
        add_vals_once
          (fun v -> lazy (Set (Lazy.force v))) sets StringMap.empty in
      let vals =
        add_vals_once
          (fun v -> lazy (Rel (Lazy.force v))) rels vals in
      { env_empty with vals; }

(* Primitive added internally to actual env *)
    let add_prims env bds =
      let vals = env.vals in
      let vals =
        List.fold_left
          (fun vals (k,f) ->
            StringMap.add k (lazy (Prim (k,next_id (),f)))
              vals)
          vals bds in
      { env with vals; }

    type loc = (TxtLoc.t * string option) list

    module Shown = struct
      type t = Rel of S.event_rel | Set of S.event_set

      let apply_rel f (sr:t) = match sr with
        | Rel r -> Rel (f r)
        | Set _ -> sr
    end

(* Internal status of interpreter *)
    type inter_st = {
        included : StringSet.t ;
        loc : loc ;
      }

    let inter_st_empty = { included = StringSet.empty; loc = []; }

(* Complete status *)
    type st = {
        env : V.env ;
        show : Shown.t StringMap.t Lazy.t ;
        skipped : StringSet.t ;
        flags : Flag.Set.t ;
        ks : ks ;
        bell_info : BellModel.info ;
        st : inter_st ;
      }

(* Interpretation result *)
    type st_out = {
        out_show : S.event_rel Misc.Simple.bds Lazy.t ;
        out_sets : S.event_set StringMap.t Lazy.t ;
        out_skipped : StringSet.t ;
        out_flags : Flag.Set.t ;
        out_bell_info :  BellModel.info ;
      }

(* Remove transitive edges, except if instructed not to *)
    let rt_loc lbl =
      if
        O.verbose <= 1 &&
        not (StringSet.mem lbl O.symetric) &&
        not (StringSet.mem lbl O.showraw)
      then E.EventRel.remove_transitive_edges else (fun x -> x)

    let show_to_vbpp st =
      StringMap.fold
        (fun tag v k -> match v with
        | Shown.Rel v -> (tag,v)::k
        | Shown.Set _ -> k)
        (Lazy.force st.show) []

    let show_to_sets st =
      StringMap.fold
        (fun tag v k -> match v with
        | Shown.Rel _ -> k
        | Shown.Set v -> StringMap.add tag v k)
        (Lazy.force st.show) StringMap.empty

    let st2out st =
      {out_show = lazy (show_to_vbpp st) ;
       out_sets = lazy (show_to_sets st) ;
       out_skipped = st.skipped ;
       out_flags = st.flags ;
       out_bell_info = st.bell_info ; }


    let push_loc st loc =
      let ist = st.st in
      let loc = loc :: ist.loc in
      let ist = { ist with loc; } in
      { st with st=ist; }

    let pop_loc st =
      let ist = st.st in
      match ist.loc with
      | [] -> assert false
      | _::loc ->
          let ist = { ist with loc; } in
          { st with st=ist; }

    let show_loc (loc,name) =
      eprintf "%a: calling procedure%s\n" TxtLoc.pp loc
        (match name with
        | None -> ""
        | Some n -> " as " ^ n)

    let show_call_stack st = List.iter show_loc st

    let protect_call st f x =
      try f x
      with Misc.Exit ->
        let st = st.st in
        List.iter
          (fun loc -> if O.debug then show_loc loc)
          st.loc ;
        raise Misc.Exit

(* Type of eval env *)
    module EV = struct
      type env =
          { env : V.env ; silent : bool; ks : ks; }
    end


    let from_st st = { EV.env=st.env; silent=false; ks=st.ks; }

    let set_op env loc t op s1 s2 =
      try V.ValSet (t,op s1 s2)
      with CompError msg -> error env.EV.silent loc "%s" msg

    let tags_universe {enums=env; _} t =
      let tags =
        try StringMap.find t env
        with Not_found -> assert false in
      let tags = ValSet.of_list (List.map (fun s -> V.Tag (t,s)) tags) in
      tags

    let find_env {vals=env; _} k =
      Lazy.force begin
        try StringMap.find k env
        with
        | Not_found -> Warn.user_error "unbound var: %s" k
      end

    let find_env_loc loc env k =
      try  find_env env.EV.env k
      with Misc.UserError msg -> error env.EV.silent loc "%s" msg

(* find without forcing lazy's *)
    let just_find_env fail loc env k =
      try StringMap.find k env.EV.env.vals
      with Not_found ->
        if fail then error env.EV.silent loc "unbound var: %s" k
        else raise Not_found

    let as_rel ks = function
      | Rel r -> r
      | Empty -> E.EventRel.empty
      | Unv -> Lazy.force ks.unv
      | v ->
          eprintf "this is not a relation: '%s'" (pp_val v) ;
          assert false

    let as_classrel = function
      | ClassRel r -> r
      | Empty -> ClassRel.empty
      | Unv -> Warn.fatal "No universal class relation"
      | v ->
          eprintf "this is not a relation: '%s'" (pp_val v) ;
          assert false

    let as_set ks = function
      | Set s -> s
      | Empty -> E.EventSet.empty
      | Unv -> ks.evts
      | _ -> assert false

    let as_valset = function
      | ValSet (_,v) -> v
      | _ -> assert false

    let as_tag = function
      | V.Tag (_,tag) -> tag
      | _ -> assert false

    let as_tags tags =
      let ss = ValSet.fold (fun v k -> as_tag v::k) tags [] in
      StringSet.of_list ss

    exception Stabilised of typ

    let stabilised ks env =
      let rec stabilised vs ws = match vs,ws with
      | [],[] -> true
      | v::vs,w::ws -> begin match v,w with
        | (_,V.Empty)|(Unv,_) -> stabilised vs ws
(* Relation *)
        | (V.Empty,Rel w) -> E.EventRel.is_empty w && stabilised vs ws
        | (Rel v,Unv) ->
            E.EventRel.subset (Lazy.force ks.unv) v && stabilised vs ws
        | Rel v,Rel w ->
            E.EventRel.subset w v && stabilised vs ws
(* Class relation *)
        | (V.Empty,ClassRel w) -> ClassRel.is_empty w && stabilised vs ws
        | ClassRel v,ClassRel w ->
            ClassRel.subset w v && stabilised vs ws
(* Event Set *)
        | (V.Empty,Set w) -> E.EventSet.is_empty w && stabilised vs ws
        | (Set v,Unv) ->
            E.EventSet.subset ks.evts v && stabilised vs ws
        | Set v,Set w ->
            E.EventSet.subset w v && stabilised vs ws
(* Value Set *)
        | (V.Empty,ValSet (_,w)) -> ValSet.is_empty w && stabilised vs ws
        | (ValSet (TTag t,v),Unv) ->
            ValSet.subset (tags_universe env t) v && stabilised vs ws
        | ValSet (_,v),ValSet (_,w) ->
            ValSet.subset w v && stabilised vs ws
        | _,_ ->
            eprintf "Problem %s vs. %s\n" (pp_val v) (pp_val w) ;
            raise (Stabilised (type_val w))
      end
      | _,_ -> assert false in
      stabilised


(* Syntactic function *)
    let is_fun = function
      | Fun _ -> true
      | _ -> false


(* Get an expression location *)

    let get_loc = function
      | Konst (loc,_)
      | AST.Tag (loc,_)
      | Var (loc,_)
      | ExplicitSet (loc,_)
      | Op1 (loc,_,_)
      | Op (loc,_,_)
      | Bind (loc,_,_)
      | BindRec (loc,_,_)
      | App (loc,_,_)
      | Fun (loc,_,_,_,_)
      | Match (loc,_,_,_)
      | MatchSet (loc,_,_,_)
      | Try (loc,_,_)
      | If (loc,_,_,_)
        -> loc


    let empty_rel = Rel E.EventRel.empty

    let error_typ silent loc t0 t1  =
      error silent loc"type %s expected, %s found" (pp_typ t0) (pp_typ t1)

    let error_rel silent loc v = error_typ silent loc TRel (type_val v)

    let error_events silent loc v = error_typ silent loc TEvents (type_val v)


(* Tests are polymorphic, acting on relations, class relations and sets *)
    let test2pred env t e v = match t,v with
    | Acyclic,Rel r -> E.EventRel.is_acyclic r
    | Irreflexive,Rel r -> E.EventRel.is_irreflexive r
    | Acyclic,ClassRel r -> ClassRel.is_acyclic r
    | Irreflexive,ClassRel r -> ClassRel.is_irreflexive r
    | TestEmpty,Rel r -> E.EventRel.is_empty r
    | TestEmpty,ClassRel r -> ClassRel.is_empty r
    | TestEmpty,Set s -> E.EventSet.is_empty s
    | (Acyclic|Irreflexive),Set _ ->
        error env.EV.silent (get_loc e) "relation expected"
    | _,_ -> assert false (* Called on Rel or Set *)

    let test2pred  env t e v = match  t with
    | Yes t -> test2pred env t e v
    | No t -> not (test2pred env t e v)


(********************************)
(* Helpers for n-ary operations *)
(********************************)

    let type_list silent = function
      | [] -> assert false
      | (_,v)::vs ->
          let rec type_rec t0 = function
            | [] -> t0,[]
            | (loc,v)::vs ->
                let t1 = type_val v in
(* eprintf "Value: %s, Type %s\n" (pp_val v) (pp_typ t1) ; *)
                match eq_type t0 t1 with
                | Some t0 ->
                    let t0,vs = type_rec t0 vs in
                    t0,v::vs
                | None ->
                    error silent loc
                      "type %s expected, %s found" (pp_typ t0) (pp_typ t1) in
          let t0,vs = type_rec (type_val v) vs in
          t0,v::vs


(* Check explicit set arguments *)
    let set_args silent =
      let rec s_rec = function
        | [] -> []
        | (loc,Unv)::_ ->
            error silent loc "universe in explicit set"
        | x::xs -> x::s_rec xs in
      s_rec

(* Union is polymorphic *)
    let union_args =
      let rec u_rec = function
        | [] -> []
        | (_,V.Empty)::xs -> u_rec xs
        | (_,Unv)::_ -> raise Exit
        | (loc,v)::xs ->
            (loc,tag2set v)::u_rec xs in
      u_rec

(* Definition of primitives *)

    module type PrimArg = sig
      type base
      module Rel : InnerRel.S with type elt0=base
      val debug_set : out_channel -> Rel.Elts.t -> unit
      val debug_rel : out_channel -> Rel.t -> unit
      val mk_val : Rel.t -> V.v
      val typ : typ
    end

    module EventArg = struct
      type base = E.event
      module Rel = E.EventRel
      let debug_set = debug_set
      let debug_rel = debug_rel
      let mk_val r = Rel r
      let typ = TRel
    end

    module ClassArg = struct
      type base = E.EventSet.t
      module Rel = ClassRel
      let debug_set chan ss =
        fprintf chan "{%a}"
          (fun chan ss -> ClassRel.Elts.pp chan "," debug_set ss)
          ss
      let debug_rel = debug_class_rel
      let mk_val r = ClassRel r
      let typ = TClassRel
    end

(* Debug ClassRel's as instance relations
   module InstanceArg = struct
   type base = E.EventSet.t
   module Rel = ClassRel
   let debug_set chan ss =
   fprintf chan "{%a}"
   (fun chan ss -> ClassRel.Elts.pp chan "," debug_set ss)
   ss
   let debug_instance chan es =
   try fprintf chan "%s"  (E.pp_instance (E.EventSet.choose es))
   with Not_found -> assert false

   let debug_instance_rel chan r =
   ClassRel.pp chan ","
   (fun chan (e1,e2) -> fprintf chan "{%a -> %a}"
   debug_instance e1 debug_instance e2)
   r

   let debug_rel = debug_instance_rel
   let mk_val r = ClassRel r
   let typ = TClassRel
   end
 *)
    let arg_mismatch () = raise (PrimError "argument mismatch")

    let partition arg = match arg with
    | Set evts ->
        let r = U.partition_events evts in
        let vs = List.map (fun es -> Set es) r in
        ValSet (TEvents,ValSet.of_list vs)
    | _ -> arg_mismatch ()

    and classes arg =  match arg with
    | Rel r ->
        let r = E.EventRel.classes r in
        let vs = List.map (fun es -> Set es) r in
        ValSet (TEvents,ValSet.of_list vs)
    | _ -> arg_mismatch ()

(* Lift a relation from events to event classes *)
    and lift arg = match arg with
    | V.Tuple [ValSet (TEvents,cls);Rel r] ->
        let m =
          ValSet.fold
            (fun v m ->  match v with
            | Set evts ->
                E.EventSet.fold
                  (fun e m -> E.EventMap.add e evts m)
                  evts m
            | _ -> assert false)
            cls E.EventMap.empty in
        let clsr =
          E.EventRel.fold
            (fun (e1,e2) k ->
              try
                let cl1 = E.EventMap.find e1 m
                and cl2 = E.EventMap.find e2 m in
                ClassRel.add (cl1,cl2) k
              with Not_found -> k)
            r ClassRel.empty in
        V.ClassRel clsr
    | _ -> arg_mismatch ()

(* Delift a relation from event classes to events *)
    and delift arg = match arg with
    | ClassRel clsr ->
        let r =
          ClassRel.fold
            (fun (cl1,cl2) k -> E.EventRel.cartesian cl1 cl2::k)
            clsr [] in
        Rel (E.EventRel.unions r)
    | _ -> arg_mismatch ()

(* Restrict delift by intersection (fulldeflift(clsr) & loc) *)
    and deliftinter arg = match arg with
    | V.Tuple[Rel m;ClassRel clsr] ->
        let make_rel_from_classpair (cl1,cl2) =
          E.EventRel.filter
            (fun (e1,e2) ->
              E.EventSet.mem e1 cl1 &&
              E.EventSet.mem e2 cl2) m in
        let r =
          ClassRel.fold
            (fun clp k -> make_rel_from_classpair clp::k)
            clsr [] in
        Rel (E.EventRel.unions r)
    | _ -> arg_mismatch ()

    and linearisations =
      let module Make =
        functor (In : PrimArg) -> struct
          let mem = In.Rel.Elts.mem
          let zyva es r =
            if O.debug && O.verbose > 1 then begin
              eprintf "Linearisations:\n" ;
              eprintf "  %a\n" In.debug_set es ;
              eprintf "  {%a}\n"
                In.debug_rel
                (In.Rel.filter
                   (fun (e1,e2) ->
                     mem e1 es && mem e2 es)
                   r)
            end ;
            let nodes = es in
            if O.debug && O.verbose > 1 then begin
              let n = In.Rel.all_topos_kont_rel  nodes r (fun _ -> 0) (fun _ k -> k+1) 0 in
              eprintf "number of orders: %i\n" n
            end ;
            let rs =
              In.Rel.all_topos_kont_rel nodes r
                (fun o ->
                  let o =
                    In.Rel.filter
                      (fun (e1,e2) ->
                        mem e1 es && mem e2 es)
                      o in
                  if O.debug then begin
                    eprintf
                      "Linearisation failed {%a}\n%!" In.debug_rel o ;
                    ValSet.singleton (In.mk_val o)
                  end else
                    ValSet.empty)
                (fun o os ->
                  if O.debug && O.verbose > 1 then
                    eprintf "  -> {%a}\n%!" In.debug_rel o ;
                  ValSet.add (In.mk_val o) os)
                ValSet.empty in
            ValSet (In.typ,rs)
        end in
      fun ks arg -> match arg with
      | V.Tuple [Set es;Rel r;] ->
          let module L = Make(EventArg) in
          L.zyva es r
      | V.Tuple [ValSet (TEvents,es);ClassRel r;] ->
          let module L = Make(ClassArg) in
          let es =
            let sets = ValSet.fold (fun v k -> as_set ks v::k) es [] in
            ClassRel.Elts.of_list sets in
          L.zyva es r
      | _ -> arg_mismatch ()

    and bisimulation =
      fun arg -> match arg with
      | V.Tuple [Rel t; Rel e; ] ->
          Rel (E.EventRel.bisimulation t e)
      | V.Tuple [ClassRel t; ClassRel e; ] ->
          ClassRel (ClassRel.bisimulation t e)
      | _ -> arg_mismatch ()

    and tag2scope env arg = match arg with
    | V.Tag (_,tag) ->
        begin try
          let v = Lazy.force (StringMap.find tag env.vals) in
          match v with
          | V.Empty|V.Unv|V.Rel _ ->  v
          | _ ->
              raise
                (PrimError
                   (sprintf
                      "value %s is not a relation, found %s"
                      tag  (pp_type_val v)))

        with Not_found ->
          raise
            (PrimError (sprintf "cannot find scope instance %s (the litmus test might be missing a scope tree declaration)" tag))
        end
    | _ -> arg_mismatch ()

    and tag2events env arg = match arg with
    | V.Tag (_,tag) ->
        let x = BellName.tag2instrs_var tag in
        begin try
          let v = Lazy.force (StringMap.find x env.vals) in
          match v with
          | V.Empty|V.Unv|V.Set _ ->  v
          | _ ->
              raise
                (PrimError
                   (sprintf
                      "value %s is not a set of events, found %s"
                      x  (pp_type_val v)))

        with Not_found ->
          raise
            (PrimError (sprintf "cannot find event set %s" x))
        end
    | _ -> arg_mismatch ()

    and fromto ks arg = match arg with
    | V.Set es -> V.Rel (U.fromto ks.po es)
    | _ -> arg_mismatch ()

    and tag2fenced env arg = match arg with
    | V.Tag (_,tag) ->
        let not_a_set_of_events x v =
          raise
            (PrimError
               (sprintf
                  "value %s is not a set of events, found %s"
                  x  (pp_type_val v))) in
        let find_rel id = match Lazy.force (StringMap.find id env.vals) with
        | V.Rel rel -> rel
        | _ -> assert false
        in
        let po = find_rel "po" in
        let fromto = find_rel "fromto" in
        let x = BellName.tag2instrs_var tag in
        begin try
          let v = Lazy.force (StringMap.find x env.vals) in
          match v with
          | V.Empty -> V.Rel E.EventRel.empty
          | V.Unv -> assert false (* jade: we assert false when all the events in the execution bear the tag tag *)
          | V.Set bevts ->
              let filter (x, y) =
                E.EventSet.exists
                  (fun b -> E.EventRel.mem (x,b) po && E.EventRel.mem (b,y) po)
                  bevts in
              V.Rel (E.EventRel.filter filter fromto)
          | _ -> not_a_set_of_events x v

        with Not_found ->
          raise
            (PrimError (sprintf "cannot find event set %s" x))
        end
    | _ -> arg_mismatch ()

    and loc2events ks arg = match arg with
    | V.Tag (_,s) ->
        let evts = ks.evts in
        let r = U.loc2events s evts in
        Set r
    | _ -> arg_mismatch ()

    and domain arg = match arg with
    | V.Empty ->  V.Empty
    | V.Unv -> V.Unv
    | V.Rel r ->  V.Set (E.EventRel.domain r)
    | _ -> arg_mismatch ()

    and range arg = match arg with
    | V.Empty ->  V.Empty
    | V.Unv -> V.Unv
    | V.Rel r ->  V.Set (E.EventRel.codomain r)
    | _ -> arg_mismatch ()

    and fail arg =
      let pp = pp_val arg in
      let msg = sprintf "fail on %s" pp in
      raise (PrimError msg)

    and different_values arg = match arg with
    | V.Rel r ->
        let r = E.EventRel.filter (fun (e1,e2) -> not (U.same_value e1 e2)) r in
        V.Rel r
    | _ -> arg_mismatch ()

    and same_oaRel arg = match arg with
    | V.Rel r ->
        let r = E.EventRel.filter (fun (e1,e2) -> (U.same_oa e1 e2)) r in
        V.Rel r
    | _ -> arg_mismatch ()

    and check_two pred arg = match arg with
      | V.Tuple [V.Set ws; V.Rel prec; ] ->
          let m = E.EventRel.M.to_map prec in
          let ws =
            E.EventSet.filter
              (fun w ->
                E.is_store w && E.is_pt w &&
                begin
                  match E.EventSet.as_singleton (E.EventRel.M.succs w m) with
                  | Some p -> pred w p
 (* w does not qualify when zero of two or more prec-related events *)
                  | None -> false
                end)
              ws in
          V.Set ws
      | _ -> arg_mismatch ()

    let oa_changes = check_two (fun w p -> not (U.same_oa w p))
    and at_least_one_writable = check_two U.writable2

    (*
     * The relation po is to interpreted as a transitive relation,
     * this primitive is much more efficient when po+ is huge.
     *)
    let inter_transitive arg = match arg with
      |  V.Tuple [V.Rel po; V.Rel loc; ] ->
          let module ME = E.EventRel.M in
          let m = ME.to_map  po in
          let r =
            E.EventRel.filter
              (fun  p -> ME.exists_path p m)
              loc in
          V.Rel r
      | _ -> arg_mismatch ()

    let add_primitives ks m =
      add_prims m
        [
          "at-least-one-writable",at_least_one_writable;
          "oa-changes",oa_changes;
          "same-oa",same_oaRel;
          "different-values",different_values;
         "fromto",fromto ks;
         "classes-loc",partition;
         "classes",classes;
         "lift",lift;
         "delift",delift;
         "deliftinter",deliftinter;
         "linearisations",linearisations ks;
         "bisimulation",bisimulation;
         "tag2scope",tag2scope m;
         "tag2level",tag2scope m;
         "tag2events",tag2events m;
         "tag2fenced",tag2fenced m;
         "loc2events",loc2events ks;
         "domain",domain;
         "range",range;
         "fail",fail;
         "inter_transitive", inter_transitive;
       ]


(***************)
(* Interpreter *)
(***************)
    type rel =
      | Rid of (E.event -> bool)
      | Revent of E.EventRel.t
      | Rclass of ClassRel.t

    let end_profile ~t0 ~loc : unit =
      let t1 = Sys.time () in
      if t1 -. t0 > 1. (* We log only executions that took more than 1 second *)
      then
        Printf.eprintf "cat interpreter took %fs to evaluate %s.\n%!" (t1 -. t0)
          (Extract.extract loc)

    let profile_exp =
      if O.profile then (fun f e ->
        let t0 = Sys.time () in
        let res = f e in
        end_profile ~t0 ~loc:(ASTUtils.exp2loc e);
        res)
      else ( @@ )

    let eval_variant loc v =
      try O.variant v
      with Not_found ->
        Warn.warn_always
          "%a: non-existent variant \"%s\", assumed unset"
          TxtLoc.pp loc v ;
        false

    let rec eval_variant_cond loc = function
      | Variant v -> eval_variant loc v
      | OpNot v -> not (eval_variant_cond loc v)
      | OpAnd (v1,v2) -> (eval_variant_cond loc v1) && (eval_variant_cond loc v2)
      | OpOr (v1,v2) -> (eval_variant_cond loc v1) || (eval_variant_cond loc v2)

(* For all success call kont, accumulating results *)
    let interpret test kfail =

      let rec eval_loc env e = get_loc e,eval env e

      and check_id env e = match e with
      | Op1 (_,ToId,e) -> Some (eval_events_mem env e)
      | _ -> None

      and eval env = profile_exp @@ function
        | Konst (_,AST.Empty SET) -> V.Empty (* Polymorphic empty *)
        | Konst (_,AST.Empty RLN) -> empty_rel
        | Konst (_,Universe _) -> Unv
        | AST.Tag (loc,s) ->
            begin try
              V.Tag (StringMap.find s env.EV.env.tags,s)
            with Not_found ->
              error env.EV.silent loc "tag '%s is undefined" s
            end
        | Var (loc,k) ->
            find_env_loc loc env k
        | Fun (loc,xs,body,name,fvs) ->
            Clo (eval_fun false env loc xs body name fvs)
(* Unary operators *)
        | Op1 (_,Plus,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Unv
            | Rel r -> Rel (E.EventRel.transitive_closure r)
            | v -> error_rel env.EV.silent (get_loc e) v
            end
        | Op1 (_,Star,e) ->
            begin match eval env e with
            | V.Empty -> Rel (Lazy.force env.EV.ks.id)
            | Unv -> Unv
            | Rel r ->
                Rel
                  (E.EventRel.union
                     (E.EventRel.transitive_closure r)
                     (Lazy.force env.EV.ks.id))
            | v -> error_rel env.EV.silent (get_loc e) v
            end
        | Op1 (_,Opt,e) ->
            begin match eval env e with
            | V.Empty -> Rel (Lazy.force env.EV.ks.id)
            | Unv -> Unv
            | Rel r ->
                Rel (E.EventRel.union r (Lazy.force env.EV.ks.id))
            | v -> error_rel env.EV.silent (get_loc e) v
            end
        | Op1 (_,Comp,e) -> (* Back to polymorphism *)
            begin match eval env e with
            | V.Empty -> Unv
            | Unv -> V.Empty
            | Set s ->
                Set (E.EventSet.diff env.EV.ks.evts s)
            | Rel r ->
                Rel (E.EventRel.diff (Lazy.force env.EV.ks.unv) r)
            | ValSet (TTag ts as t,s) ->
                ValSet (t,ValSet.diff (tags_universe env.EV.env ts) s)
            | v ->
                error env.EV.silent (get_loc e)
                  "set or relation expected, %s found"
                  (pp_typ (type_val v))
            end
        | Op1 (_,Inv,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Unv
            | Rel r -> Rel (E.EventRel.inverse r)
            | ClassRel r -> ClassRel (ClassRel.inverse r)
            | Pair (v1,v2) -> Pair (v2,v1)
            | v -> error_rel env.EV.silent (get_loc e) v
            end
        | Op1 (_,ToId,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Rel (Lazy.force env.EV.ks.id)
            | Set s -> Rel (E.EventRel.set_to_rln s)
            | v -> error_events env.EV.silent (get_loc e) v
            end
(* One xplicit N-ary operator *)
        | ExplicitSet (loc,es) ->
            let vs = List.map (eval_loc env) es in
            let vs = set_args env.EV.silent vs in
            begin match vs with
            | [] -> V.Empty
            | _ ->
                let t,vs = type_list env.EV.silent vs in
                try match t with
                | TEvent ->
                    let vs =
                      List.rev_map
                        (function Event e -> e | _ -> assert false)
                        vs in
                    Set (E.EventSet.of_list vs)
                | TPair ->
                    let vs =
                      List.rev_map
                        (function Pair p -> p | _ -> assert false)
                        vs in
                    Rel (E.EventRel.of_list vs)
                | _ -> ValSet (t,ValSet.of_list vs)
                with CompError msg ->
                  error env.EV.silent loc "%s" msg
            end
(* Tuple is an actual N-ary operator *)
        | Op (_loc,AST.Tuple,es) ->
            V.Tuple (List.map (eval env) es)
(* N-ary operators, those associative binary operators are optimized *)
        | Op (loc,Union,es) ->
            let vs = List.map (eval_loc env) es in
            begin try
              let vs = union_args vs in
              match vs with
              | [] -> V.Empty
              | _ ->
                  let t,vs = type_list env.EV.silent vs in
                  match t with
                  | TClassRel ->
                      ClassRel(ClassRel.unions (List.map as_classrel vs))
                  | TRel ->
                      Rel (E.EventRel.unions (List.map (as_rel env.EV.ks) vs))
                  | TEvents ->
                      Set (E.EventSet.unions  (List.map (as_set env.EV.ks) vs))
                  | TSet telt ->
                      ValSet (telt,ValSet.unions (List.map as_valset vs))
                  | ty ->
                      error env.EV.silent loc
                        "cannot perform union on type '%s'" (pp_typ ty)
            with Exit -> Unv end
        | Op (loc,Seq,es) ->
            begin try
              let vs = List.map  (eval_rels env) es in
              let rec do_seq = function
                | [] -> assert false
                | [v] -> v
                | v::vs ->
                    begin match v,do_seq vs with
                    | Rid f,Rid fs -> Rid (fun e -> f e && fs e)
                    | Rid f,Revent vs ->
                        Revent (E.EventRel.filter (fun (e1,_) -> f e1) vs)
                    | Revent v,Rid fs  ->
                        Revent (E.EventRel.filter (fun (_,e2) -> fs e2) v)
                    | Revent v,Revent vs ->
                        Revent (E.EventRel.sequence v vs)
                    | Rclass v,Rclass vs ->
                        Rclass (ClassRel.sequence v vs)
                    | _,_ ->
                        error env.EV.silent loc
                          "mixing relations in sequence"
                    end in
              match do_seq vs with
              | Rid f -> Rel (E.EventRel.set_to_rln (E.EventSet.filter f env.EV.ks.evts))
              | Revent r -> Rel r
              | Rclass r -> ClassRel r
            with Exit -> V.Empty end
(*
  begin try
  let f1,rs,f2 = eval_seq_args env es in
  let r =
  List.fold_right
  E.EventRel.sequence
  rs (Lazy.force env.EV.ks.id) in
  let r = match f1,f2 with
  | None,None -> r
  | Some f1,None ->
  E.EventRel.filter (fun (e1,_) -> f1 e1) r
  | None,Some f2 ->
  E.EventRel.filter (fun (_,e2) -> f2 e2) r
  | Some f1,Some f2 ->
  E.EventRel.filter (fun (e1,e2) -> f1 e1 && f2 e2) r in
  Rel r
  with Exit -> empty_rel
  end
 *)
(* Binary operators *)
        | Op (_loc1,Inter,[e1;Op (_loc2,Cartesian,[e2;e3])])
        | Op (_loc1,Inter,[Op (_loc2,Cartesian,[e2;e3]);e1]) ->
            let r = eval_rel env e1
            and f1 = eval_events_mem env e2
            and f2 = eval_events_mem env e3 in
            let r =
              E.EventRel.filter
                (fun (e1,e2) -> f1 e1 && f2 e2)
                r in
            Rel r
        | Op (loc,Inter,[e1;Op1 (_,Comp,e2)])
        | Op (loc,Inter,[Op1 (_,Comp,e2);e1;])
          ->
           eval_diff env loc e1 e2
        | Op (loc,Inter,[e1;e2;]) -> (* Binary notation kept in parser *)
            let loc1,v1 = eval_loc env e1
            and loc2,v2 = eval_loc env e2 in
            begin match tag2set v1,tag2set v2 with
            | (V.Tag _,_)|(_,V.Tag _) -> assert false
            | Rel r1,Rel r2 -> Rel (E.EventRel.inter r1 r2)
            | ClassRel r1,ClassRel r2 -> ClassRel (ClassRel.inter r1 r2)
            | Set s1,Set s2 -> Set (E.EventSet.inter s1 s2)
            | ValSet (t,s1),ValSet (_,s2) ->
                set_op env loc t ValSet.inter s1 s2
            | (Unv,r)|(r,Unv) -> r
            | (V.Empty,_)|(_,V.Empty) -> V.Empty
            | (Event _|Pair _|Clo _|Prim _|Proc _|V.Tuple _),_ ->
                error env.EV.silent loc1
                  "intersection on %s" (pp_typ (type_val v1))
            | _,(Event _|Pair _|Clo _|Prim _|Proc _|V.Tuple _) ->
                error env.EV.silent loc2
                  "intersection on %s" (pp_typ (type_val v2))
            | ((Rel _|ClassRel _),(Set _|ValSet _))
            | ((Set _|ValSet _),(Rel _|ClassRel _)) ->
                error env.EV.silent
                  loc "mixing sets and relations in intersection"
            | (ValSet _,Set _)
            | (Set _,ValSet _) ->
                error env.EV.silent
                  loc "mixing event sets and sets in intersection"
            | (ClassRel _,Rel _)
            | (Rel _,ClassRel _) ->
                error env.EV.silent
                  loc "mixing event relation and class relation in intersection"

            end
        | Op (loc,Diff,[e1;e2;]) ->
           eval_diff env loc e1 e2
        | Op (_,Cartesian,[e1;e2;]) ->
            let s1 = eval_events env e1
            and s2 = eval_events env e2 in
            Rel (E.EventRel.cartesian s1 s2)
        | Op (loc,Add,[e1;e2;]) ->
            let v1 = eval env e1
            and v2 = eval env e2 in
            begin match v1,v2 with
            | V.Unv,_ -> error env.EV.silent loc "universe in set ++"
            | _,V.Unv -> V.Unv
            | Event e,V.Empty -> Set (E.EventSet.singleton e)
            | Pair p,V.Empty -> Rel (E.EventRel.singleton p)
            | _,V.Empty -> V.ValSet (type_val v1,ValSet.singleton v1)
            | V.Empty,V.ValSet (TSet e2 as t2,s2) ->
                let v1 = ValSet (e2,ValSet.empty) in
                set_op env loc t2 ValSet.add v1 s2
            | _,V.ValSet (_,s2) ->
                set_op env loc (type_val v1) ValSet.add v1 s2
            | Pair p,Rel r ->
                Rel (E.EventRel.add p r)
            | Tuple [Event ev1;Event ev2;],Rel r ->
                Rel (E.EventRel.add (ev1,ev2) r)
            | _,Rel _ ->
                error env.EV.silent (get_loc e1)
                  "this expression of type '%s' should be a pair"
                  (pp_typ (type_val v2))
            | Event e,Set es ->
                Set (E.EventSet.add e es)
            | _,Set _ ->
                error env.EV.silent (get_loc e1)
                  "this expression of type '%s' should be an event"
                  (pp_typ (type_val v1))
            | _,
                (Event _|Pair _|Clo _|Prim _
              |Proc _|V.Tag (_, _)|V.Tuple _) ->
                  error env.EV.silent (get_loc e2)
                    "this expression of type '%s' should be a set"
                    (pp_typ (type_val v2))
            | _,ClassRel _ ->
                error env.EV.silent (get_loc e2)
                  "this expression of type '%s' cannot occur here"
                  (pp_typ (type_val v2))
            end
        | Op (_,(Diff|Inter|Cartesian|Add),_) -> assert false (* By parsing *)
(* Application/bindings *)
        | App (loc,f,e) ->
            eval_app loc env (eval env f) (eval env e)
        | Bind (_,bds,e) ->
            let m = eval_bds env bds in
            eval { env with EV.env = m;} e
        | BindRec (loc,bds,e) ->
            let m =
              match env_rec (fun _ -> true)
                  env loc (fun pp -> pp) bds
              with
              | CheckOk env -> env
              | CheckFailed _ -> assert false (* No check in expr binding *) in
            eval { env with EV.env=m;} e
        | Match (loc,e,cls,d) ->
            let v = eval env e in
            begin match v with
            | V.Tag (_,s) ->
                let rec match_rec = function
                  | [] ->
                      begin match d with
                      | Some e ->  eval env e
                      | None ->
                          error env.EV.silent
                            loc "pattern matching failed on value '%s'" s
                      end
                  | (ps,es)::cls ->
                      if s = ps then eval env es
                      else match_rec cls in
                match_rec cls
            | V.Empty ->
                error env.EV.silent (get_loc e) "matching on empty"
            | V.Unv ->
                error env.EV.silent (get_loc e) "matching on universe"
            | _ ->
                error env.EV.silent (get_loc e)
                  "matching on non-tag value of type '%s'"
                  (pp_typ (type_val v))
            end
        | MatchSet (loc,e,ife,cl) ->
            let v = eval env e in
            begin match v with
            | V.Empty -> eval env ife
            | V.Unv ->
                error env.EV.silent loc
                  "%s" "Cannot set-match on universe"
            | Set es ->
                if E.EventSet.is_empty es then eval env ife
                else begin match cl with
                | EltRem (x,xs,ex) ->
                    let elt =
                      lazy begin
                        try E.EventSet.choose es
                        with Not_found -> assert false
                      end in
                    let s =
                      lazy begin
                        Set (E.EventSet.remove (Lazy.force elt) es)
                      end in
                    let elt = lazy (Event (Lazy.force elt)) in
                    let m = env.EV.env in
                    let m = add_val x elt m in
                    let m = add_val xs s m in
                    eval { env with EV.env = m; } ex
                | PreEltPost (xs1,x,xs2,ex) ->
                    let s1,elt,s2 =
                      try E.EventSet.split3 es with Not_found -> assert false in
                    let m = env.EV.env in
                    let m = add_val x (lazy (Event elt)) m in
                    let m = add_val xs1 (lazy (Set s1)) m in
                    let m = add_val xs2 (lazy (Set s2)) m in
                    eval { env with EV.env = m; } ex
                end
            | Rel r ->
                if E.EventRel.is_empty r then eval env ife
                else begin match cl with
                | EltRem (x,xs,ex) ->
                    let p =
                      lazy begin
                        try E.EventRel.choose r
                        with Not_found -> assert false
                      end in
                    let s =
                      lazy begin
                        Rel (E.EventRel.remove (Lazy.force p) r)
                      end in
                    let p = lazy (Pair (Lazy.force p)) in
                    let m = env.EV.env in
                    let m = add_val x p m in
                    let m = add_val xs s m in
                    eval { env with EV.env = m; } ex
                | PreEltPost (xs1,x,xs2,ex) ->
                    let s1,elt,s2 =
                      try E.EventRel.split3 r with Not_found -> assert false in
                    let m = env.EV.env in
                    let m = add_val x (lazy (Pair elt)) m in
                    let m = add_val xs1 (lazy (Rel s1)) m in
                    let m = add_val xs2 (lazy (Rel s2)) m in
                    eval { env with EV.env = m; } ex
                end
            | ValSet (t,s) ->
                if ValSet.is_empty s then eval env ife
                else begin match cl with
                | EltRem (x,xs,ex) ->
                    let elt =
                      lazy begin
                        try ValSet.choose s
                        with Not_found -> assert false
                      end in
                    let s =
                      lazy begin
                        try ValSet (t,ValSet.remove (Lazy.force elt) s)
                        with  CompError msg ->
                          error env.EV.silent (get_loc e) "%s" msg
                      end in
                    let m = env.EV.env in
                    let m = add_val x elt m in
                    let m = add_val xs s m in
                    eval { env with EV.env = m; } ex
                | PreEltPost (xs1,x,xs2,ex) ->
                    let s1,elt,s2 =
                      try ValSet.split3 s with Not_found -> assert false in
                    let m = env.EV.env in
                    let m = add_val x (lazy elt) m in
                    let m = add_val xs1 (lazy (ValSet (t,s1))) m in
                    let m = add_val xs2 (lazy (ValSet (t,s2))) m in
                    eval { env with EV.env = m; } ex
                end
            | _ ->
                error env.EV.silent
                  (get_loc e) "set-matching on non-set value of type '%s'"
                  (pp_typ (type_val v))
            end
        | Try (loc,e1,e2) ->
            begin
              try eval { env with EV.silent = true; } e1
              with Misc.Exit ->
                if O.debug then warn loc "caught failure" ;
                eval env e2
            end
        | If (loc,cond,ifso,ifnot) ->
            if eval_cond loc env cond then eval env ifso
            else eval env ifnot

      and eval_diff env loc e1 e2 =
        let loc1,v1 = eval_loc env e1
        and loc2,v2 = eval_loc env e2 in
        match tag2set v1,tag2set v2 with
        | (V.Tag _,_)|(_,V.Tag _) -> assert false
        | Rel r1,Rel r2 -> Rel (E.EventRel.diff r1 r2)
        | ClassRel r1,ClassRel r2 -> ClassRel (ClassRel.diff r1 r2)
        | Set s1,Set s2 -> Set (E.EventSet.diff s1 s2)
        | ValSet (t,s1),ValSet (_,s2) ->
           set_op env loc t ValSet.diff s1 s2
        | Unv,Rel r -> Rel (E.EventRel.diff (Lazy.force env.EV.ks.unv) r)
        | Unv,Set s -> Set (E.EventSet.diff env.EV.ks.evts s)
        | Unv,ValSet (TTag ts as t,s) ->
           ValSet (t,ValSet.diff (tags_universe env.EV.env ts) s)
        | Unv,ClassRel _ ->
           error env.EV.silent
             loc1 "cannot build universe for element type %s"
             (pp_typ TClassRel)
        | Unv,ValSet (t,_) ->
           error env.EV.silent
             loc1 "cannot build universe for element type %s"
             (pp_typ t)
        | Unv,V.Empty -> Unv
        | (Rel _|ClassRel _|Set _|V.Empty|Unv|ValSet _),Unv
          | V.Empty,(Rel _|ClassRel _|Set _|V.Empty|ValSet _) -> V.Empty
        | (Rel _|ClassRel _|Set _|ValSet _),V.Empty -> v1
        | (Event _|Pair _|Clo _|Proc _|Prim _|V.Tuple _),_ ->
           error env.EV.silent loc1
             "difference on %s" (pp_typ (type_val v1))
        | _,(Event _|Pair _|Clo _|Proc _|Prim _|V.Tuple _) ->
           error env.EV.silent loc2
             "difference on %s" (pp_typ (type_val v2))
        | ((Set _|ValSet _),(ClassRel _|Rel _))|((ClassRel _|Rel _),(Set _|ValSet _)) ->
           error env.EV.silent
             loc "mixing set and relation in difference"
        | (Set _,ValSet _)|(ValSet _,Set _) ->
           error env.EV.silent
             loc "mixing event set and set in difference"
        | (Rel _,ClassRel _)|(ClassRel _,Rel _) ->
           error env.EV.silent
             loc "mixing event relation and class relation in difference"

      and eval_cond loc env c = match c with
      | In (e1,e2) ->
          let loc1,v1 = eval_loc env e1 in
          begin match v1 with
          | Event e ->
              let v2 = eval_events env e2 in
              E.EventSet.mem e v2
          | Pair p ->
              let v2 = eval_rel env e2 in
              E.EventRel.mem p v2
          | Tuple [Event ev1;Event ev2;] ->
              let v2 = eval_rel env e2 in
              E.EventRel.mem (ev1,ev2) v2
          | _ ->
              begin match eval_loc env e2 with
              | _,ValSet (t2,vs) ->
                  let t1 = V.type_val v1 in
                  if type_equal t1 t2 then ValSet.mem v1 vs
                  else
                    error_typ env.EV.silent loc1 t2 t1
              | loc2,v2 ->
                  error env.EV.silent loc2 "set expected, found %s" (pp_typ (type_val v2))
              end
          end
      | Eq (e1,e2) ->
          let v1 = eval env e1
          and v2 = eval env e2 in
          ValOrder.compare v1 v2 = 0
      | Subset(e1,e2) -> (*ici*)
          let v1 = eval_loc env e1
          and v2 = eval_loc env e2 in
          let t,_ = type_list env.EV.silent [v1;v2] in
          let a' = snd v1
          and b' = snd v2 in
          begin match t with
          | TRel -> E.EventRel.subset (as_rel env.EV.ks a') (as_rel env.EV.ks b')
          | TEvents -> E.EventSet.subset (as_set env.EV.ks a') (as_set env.EV.ks b')
          | TSet _ -> ValSet.subset (as_valset a') (as_valset b')
          | ty ->
              error env.EV.silent loc
                "cannot perform subset on type '%s'" (pp_typ ty) end
      | VariantCond v -> eval_variant_cond loc v

      and eval_app loc env vf va = match vf with
      | Clo f ->
          let env =
            { env with
              EV.env=add_args loc f.clo_args va env f.clo_env;} in
          if O.debug then begin try
            eval env f.clo_body
          with
          |  Misc.Exit ->
              error env.EV.silent loc "Calling"
          | e ->
              error env.EV.silent loc "Calling (%s)" (Printexc.to_string e)
          end else
            eval env f.clo_body
      | Prim (name,_,f) ->
          begin try f va with
          | PrimError msg ->
              error env.EV.silent loc "primitive %s: %s" name msg
          | Misc.Exit ->
              error env.EV.silent loc "Calling primitive %s" name
          end
      | _ -> error env.EV.silent loc "closure or primitive expected"

      and eval_fun is_rec env loc pat body name fvs =
        if O.debug && O.verbose > 1 then begin
          let sz =
            StringMap.fold
              (fun _ _ k -> k+1) env.EV.env.vals 0 in
          let fs = StringSet.pp_str "," (fun x -> x) fvs in
          warn loc "Closure %s, env=%i, free={%s}" name sz fs
        end ;
        let vals =
          StringSet.fold
            (fun x k ->
              try
                let v = just_find_env (not is_rec) loc env x in
                StringMap.add x v k
              with Not_found -> k)
            fvs StringMap.empty in
        let env = { env.EV.env with vals; } in
        {clo_args=pat; clo_env=env; clo_body=body; clo_name=(name,next_id ()); }

      and add_args loc pat v env_es env_clo =
        let xs = pat_as_vars pat in
        let vs = match xs with [_] -> [v] | _ -> v_as_vs v in
        let bds =
          try
            List.combine xs vs
          with _ -> error env_es.EV.silent loc "argument_mismatch" in
        let env_call =
          List.fold_right
            (fun (x,v) env -> add_val x (lazy v) env)
            bds env_clo in
        env_call

      and eval_rel env e =  match eval env e with
      | Rel v -> v
      | Pair p -> E.EventRel.singleton p
      | V.Empty -> E.EventRel.empty
      | Unv -> Lazy.force env.EV.ks.unv
      | v -> error_rel env.EV.silent (get_loc e) v

      and eval_rels env e =  match check_id env e with
      | Some es -> Rid es
      | None -> match eval env e with
        | Rel v -> Revent v
        | ClassRel v -> Rclass v
        | Pair p -> Revent (E.EventRel.singleton p)
        | V.Empty -> raise Exit
        | Unv -> Revent (Lazy.force env.EV.ks.unv)
        | v -> error_rel env.EV.silent (get_loc e) v

      and eval_events env e = match eval env e with
      | Set v -> v
      | Event e -> E.EventSet.singleton e
      | V.Empty -> E.EventSet.empty
      | Unv -> env.EV.ks.evts
      | v -> error_events env.EV.silent (get_loc e) v

      and eval_rel_set env e = match eval env e with
      | Rel _ | Set _ |ClassRel _ as v ->  v
      | Event e -> Set (E.EventSet.singleton e)
      | Pair p -> Rel (E.EventRel.singleton p)
      | V.Empty -> Rel E.EventRel.empty
      | Unv -> Rel (Lazy.force env.EV.ks.unv)
      | _ -> error env.EV.silent (get_loc e) "relation or set expected"

      and eval_shown env e = match eval_rel_set env e with
      | Set v -> Shown.Set v
      | Rel v -> Shown.Rel v
      | ClassRel _ ->  Shown.Rel E.EventRel.empty (* Show nothing *)
      | _ -> assert false

      and eval_events_mem env e = match eval env e with
      | Set s -> fun e -> E.EventSet.mem e s
      | Event e0 -> fun e -> E.event_compare e0 e = 0
      | V.Empty -> fun _ -> false
      | Unv -> fun _ -> true
      | v -> error_events env.EV.silent (get_loc e) v

      and eval_proc loc env x = match find_env_loc loc env x with
      | Proc p -> p
      | _ -> Warn.user_error "procedure expected"

(* For let *)
      and eval_bds env_bd  =
        let rec do_rec bds = match bds with
        | [] -> env_bd.EV.env
        | (loc,p,e)::bds ->
            (*
              begin match v with
              | Rel r -> printf "Defining relation %s = {%a}.\n" k debug_rel r
              | Set s -> printf "Defining set %s = %a.\n" k debug_set s
              | Clo _ -> printf "Defining function %s.\n" k
              end;
             *)
            add_pat_val env_bd.EV.silent loc
              p (lazy (eval env_bd e)) (do_rec bds) in
        do_rec

(* For let rec *)

      and env_rec check env loc pp bds =
        let fs,nfs =  List.partition  (fun (_,_,e) -> is_fun e) bds in
        let fs =
          List.map
            (fun (loc,p,e) -> match p with
            | Pvar x -> x,e
            | Ptuple _ ->
                error env.EV.silent loc "%s" "binding mismatch")
            fs in
        match nfs with
        | [] -> CheckOk (env_rec_funs env loc fs)
        | _  -> env_rec_vals check env loc pp fs nfs


(* Recursive functions *)
      and env_rec_funs env_bd _loc bds =
        let env = env_bd.EV.env in
        let clos =
          List.map
            (function
              | f,Fun (loc,xs,body,name,fvs) ->
                  f,eval_fun true env_bd loc xs body name fvs,fvs
              | _ -> assert false)
            bds in
        let add_funs pred env =
          List.fold_left
            (fun env (f,clo,_) ->
              if pred f then add_val f (lazy (Clo clo)) env
              else env)
            env clos in
        List.iter
          (fun (_,clo,fvs) ->
            clo.clo_env <-
              add_funs
                (fun x -> match x with
                | None -> false
                | Some x -> StringSet.mem x fvs)
                clo.clo_env)
          clos ;
        add_funs (fun _ -> true) env


(* Compute fixpoint of relations *)
      and env_rec_vals check env_bd loc pp funs bds =

        (* Pretty print (relation) current values *)
        let vb_pp vs =
          List.fold_left2
            (fun k (_loc,pat,_) v ->
              let pp_id x v k =
                try
                  let v = match v with
                  | V.Empty -> E.EventRel.empty
                  | Unv -> Lazy.force env_bd.EV.ks.unv
                  | Rel r -> r
                  | _ -> raise Exit in
                  let x = match x with
                  | Some x -> x
                  | None -> "_" in
                  (x, rt_loc x v)::k
                with Exit -> k in
              let rec pp_ids xs vs k = match xs,vs with
              | ([],_)|(_,[]) -> k (* do not fail for pretty print *)
              | x::xs,v::vs ->
                  pp_id x v (pp_ids xs vs k) in
              match pat with
              | Pvar x -> pp_id x v k
              | Ptuple xs -> pp_ids xs (v_as_vs v) k)
            [] bds vs in

(* Fixpoint iteration *)
        let rec fix k env vs =
          if O.debug && O.verbose > 1 then begin
            let vb_pp = pp (vb_pp vs) in
            U.pp_failure test env_bd.EV.ks.conc (sprintf "Fix %i" k) vb_pp
          end ;
          let env,ws = fix_step env_bd env bds in
          let env = env_rec_funs { env_bd with EV.env=env;} loc funs in
          let check_ok = check { env_bd with EV.env=env; } in
          if not check_ok then begin
            if O.debug then warn loc "Fix point interrupted" ;
            CheckFailed env
          end else
            let over =
              begin try stabilised env_bd.EV.ks env vs ws
              with Stabilised t ->
                error env_bd.EV.silent loc "illegal recursion on type '%s'"
                  (pp_typ t)
              end in
            if over then CheckOk env
            else fix (k+1) env ws in

        let env0 =
          List.fold_left
            (fun env (_,pat,_) -> match pat with
            | Pvar k -> add_val k (lazy V.Empty) env
            | Ptuple ks ->
                List.fold_left
                  (fun env k -> add_val k (lazy V.Empty) env)
                  env ks)
            env_bd.EV.env bds in
        let env0 = env_rec_funs { env_bd with EV.env=env0;} loc funs in
        let env =
          if O.bell then CheckOk env0 (* Do not compute fixpoint in bell *)
          else fix 0 env0 (List.map (fun (_,pat,_) -> pat2empty pat) bds) in
        if O.debug then warn loc "Fix point over" ;
        env

      and fix_step env_bd env bds = match bds with
      | [] -> env,[]
      | (loc,k,e)::bds ->
          let v = eval {env_bd with EV.env=env;} e in
          let env = add_pat_val env_bd.EV.silent loc k (lazy v) env in
          let env,vs = fix_step env_bd env bds in
          env,(v::vs) in

(* Showing bound variables, (-doshow option) *)

      let find_show_shown ks env x =

        let loc_asrel v = match v with
        | Rel r -> Shown.Rel (rt_loc x r)
        | Set r ->
            if _dbg then eprintf "Found set %s: %a\n%!" x debug_set r;
            Shown.Set r
        | V.Empty -> Shown.Rel E.EventRel.empty
        | Unv -> Shown.Rel (rt_loc x (Lazy.force ks.unv))
        | v ->
            Warn.warn_always
              "Warning show: %s is not a relation: '%s'" x (pp_val v) ;
            raise Not_found in
        try
          loc_asrel (Lazy.force (StringMap.find x env.vals))
        with Not_found -> Shown.Rel E.EventRel.empty in

      let doshowone x st =
        if O.showsome && StringSet.mem x O.doshow then
          let show =
            lazy begin
              StringMap.add x
                (find_show_shown st.ks st.env x) (Lazy.force st.show)
            end in
          { st with show;}
        else st in

      let doshow bds st =
        if O.showsome then begin
          let to_show =
            StringSet.inter O.doshow
              (StringSet.unions (List.map bdvars bds)) in
          if StringSet.is_empty to_show then st
          else
            let show = lazy begin
              StringSet.fold
                (fun x show  ->
                  let r = find_show_shown st.ks st.env x in
                  StringMap.add x r show)
                to_show
                (Lazy.force st.show)
            end in
            { st with show;}
        end else st in

      let check_bell_enum =
        if O.bell then
          fun loc st name tags ->
            try
              if name = BellName.scopes then
                let bell_info = BellModel.add_rel name tags st.bell_info in
                { st with bell_info;}
              else if name = BellName.regions then
                let bell_info = BellModel.add_regions tags st.bell_info in
                { st with bell_info;}
              else if name = BellName.levels then
                let bell_info = BellModel.add_rel name tags st.bell_info in
                let bell_info = BellModel.add_order name
                    (StringRel.order_to_succ tags) bell_info in
                { st with bell_info }
              else st
            with BellModel.Defined ->
              error_not_silent loc "second definition of bell enum %s" name
        else
          fun _loc st _v _tags -> st in

(* Check if order is being defined by a "narrower" function *)
      let check_bell_order =
        if O.bell then
          let fun_as_rel f_order loc st id_tags id_fun =
(* This function evaluate all calls to id_fun on all tags in id_tags *)
            let env = from_st st in
            let cat_fun =
              try find_env_loc TxtLoc.none env id_fun
              with _ -> assert false in
            let cat_tags =
              let cat_tags =
                try StringMap.find id_tags env.EV.env.vals
                with Not_found ->
                  error false
                    loc "tag set %s must be defined while defining %s"
                    id_tags id_fun in
              match Lazy.force cat_tags with
              | V.ValSet (TTag _,scs) -> scs
              | v ->
                  error false loc "%s must be a tag set, found %s"
                    id_tags (pp_typ (type_val v)) in
            let order =
              ValSet.fold
                (fun tag order ->
                  let tgt =
                    try
                      eval_app loc { env with EV.silent=true;} cat_fun tag
                    with Misc.Exit -> V.Empty in
                  let tag = as_tag tag in
                  let add tgt order = StringRel.add (tag,as_tag tgt) order in
                  match tgt with
                  | V.Empty -> order
                  | V.Tag (_,_) -> add tgt order
                  | V.ValSet (TTag _,vs) ->
                      ValSet.fold add vs order
                  | _ ->
                      error false loc
                        "implicit call %s('%s) must return a tag, found %s"
                        id_fun tag (pp_typ (type_val tgt)))
                cat_tags StringRel.empty in
            let tags_set = as_tags cat_tags in
            let order = f_order order in
            if O.debug then begin
              warn loc "Defining hierarchy on %s from function %s"
                id_tags id_fun ;
              eprintf "%s: {%a}\n" id_tags
                (fun chan ->
                  StringSet.pp chan "," output_string) tags_set ;
              eprintf "hierarchy: %a\n"
                (fun chan ->
                  StringRel.pp chan " "
                    (fun chan (a,b) -> fprintf chan "(%s,%s)" a b))
                order
            end ;
            if not (StringRel.is_hierarchy tags_set order) then
              error false loc
                "%s defines the non-hierarchical relation %s"
                id_fun
                (BellModel.pp_order_dec order) ;
            try
              let bell_info =
                BellModel.add_order id_tags order st.bell_info in
              { st with bell_info;}
            with BellModel.Defined ->
              let old =
                try BellModel.get_order id_tags st.bell_info
                with Not_found -> assert false in
              if not (StringRel.equal old order) then
                error_not_silent
                  loc "incompatible definition of order on %s by %s" id_tags id_fun ;
              st in

          fun bds st ->
            List.fold_left
              (fun st (_,pat,e) -> match pat with
              | Pvar (Some v) ->
                  if v = BellName.wider then
                    let loc = get_loc e in
                    fun_as_rel Misc.identity loc st BellName.scopes v
                  else if v = BellName.narrower then
                    let loc = get_loc e in
                    fun_as_rel StringRel.inverse loc st BellName.scopes v
                  else st
              | Pvar None -> st
              | Ptuple _ -> st)
              st bds
        else fun _bds st -> st in

(* Evaluate test -> bool *)

      let eval_test check env t e =
        check (test2pred env t e (eval_rel_set env e)) in

      let make_eval_test = function
        | None -> fun _env -> true
        | Some (_,_,t,e,name) ->
            if skip_this_check name then
              fun _env -> true
            else
              fun env ->
                eval_test (check_through Check) env t e in

      let pp_check_failure (st:st) (loc,pos,_,e,_) =
        warn loc "check failed" ;
        show_call_stack st.st.loc ;
        if O.debug && O.verbose > 0 then begin
          let pp = match pos with
          | Pos _ -> "???"
          | Txt txt -> txt in
          let v = eval_rel (from_st st) e in
          let cy = E.EventRel.get_cycle v in
          U.pp_failure test st.ks.conc
            (sprintf "Failure of '%s'" pp)
            (let k = show_to_vbpp st in
            ("CY",E.EventRel.cycle_option_to_rel cy)::k)
        end

      and show_cycle st (_loc,pos,tst,e,_) =
        let pp = match pos with
        | Pos _ -> "???"
        | Txt txt -> txt in
        let v = eval_rel (from_st st) e in
        let tst = match tst with Yes tst|No tst -> tst in
        let v = match tst with
        | Acyclic -> v
        | Irreflexive | TestEmpty -> E.EventRel.remove_transitive_edges v in

        let tag,cy = match tst with
        | Acyclic |Irreflexive ->
            let cy = E.EventRel.get_cycle v in
            if O.verbose > 0 then begin match cy with
            | Some xs ->
                eprintf "Cycle: %s\n"
                  (String.concat " " (List.map E.pp_eiid xs))
            | None -> ()
            end ;

            "CY",E.EventRel.cycle_option_to_rel cy
        | TestEmpty -> "NE",v in
        U.pp test st.ks.conc
          (sprintf "%s for '%s'"
             (match tst with
             | Acyclic | Irreflexive -> "Cycle"
             | TestEmpty -> "Relation")
             pp)
          (let k = show_to_vbpp st in
          (tag,cy)::k) in

(* Execute one instruction *)

      let eval_st st e = eval (from_st st) e in

      let rec exec : 'a. st -> ins -> ('a -> 'a) ->
        (st -> 'a -> 'a) -> 'a -> 'a  =
          fun  (type res) st i kfail kont (res:res) ->  match i with
          | IfVariant (loc,v,ins_true,ins_false) ->
              let ins = if eval_variant_cond loc v then ins_true else ins_false in
              run st ins kfail kont res
          | Debug (_,e) ->
              if O.debug then begin
                let v = eval_st st e in
                eprintf "%a: value is %a\n%!"
                  TxtLoc.pp (get_loc e) debug_val v
              end ;
              kont st res
          | Show (_,xs) when not O.bell ->
              if O.showsome then
                let xs =
                  List.filter
                    (fun x ->
                      try ignore (StringMap.find x st.env.vals); true
                      with Not_found -> false) xs in
                let show = lazy begin
                  List.fold_left
                    (fun show x ->
                      StringMap.add x (find_show_shown st.ks st.env x) show)
                    (Lazy.force st.show) xs
                end in
                kont { st with show;} res
              else kont st res
          | UnShow (_,xs) when not O.bell ->
              if O.showsome then
                let show = lazy begin
                  List.fold_left
                    (fun show x -> StringMap.remove x show)
                    (Lazy.force st.show)
                    (StringSet.(elements (diff (of_list xs) O.doshow)))
                end in
                kont { st with show;} res
              else kont st res
          | ShowAs (_,e,id) when not O.bell  ->
              if O.showsome then
                let show = lazy begin
                  try
                    let v =
                      Shown.apply_rel
                        (rt_loc id)
                        (eval_shown { (from_st st) with EV.silent=true } e) in
                    StringMap.add id v (Lazy.force st.show)
                  with Misc.Exit -> Lazy.force st.show
                end in
                kont { st with show; } res
              else kont st res
          | Test (tst,ty) when not O.bell  ->
              exec_test st tst ty kfail kont res
          | Let (_loc,bds) ->
              let env = eval_bds (from_st st) bds in
              let st = { st with env; } in
              let st = doshow bds st in
              let st = check_bell_order bds st in
              kont st res
          | Rec (loc,bds,testo) ->
              let env =
                match
                  env_rec
                    (make_eval_test testo) (from_st st)
                    loc (fun pp -> pp@show_to_vbpp st) bds
                with
                | CheckOk env -> Some env
                | CheckFailed env ->
                    if O.debug then begin
                      let st = { st with env; } in
                      let st = doshow bds st in
                      pp_check_failure st (Misc.as_some testo)
                    end ;
                    None in
              begin match env with
              | None -> kfail res
              | Some env ->
                  let st = { st with env; } in
                  let st = doshow bds st in

(* Check again for strictskip *)
                  let st = match testo with
                  | None -> st
                  | Some (_,_,t,e,name) ->
                      if
                        O.strictskip &&
                        skip_this_check name &&
                        not (eval_test Misc.identity (from_st st) t e)
                      then begin
                        { st with
                          skipped =
                          StringSet.add (Misc.as_some name) st.skipped;}
                      end else st in
(* Check bell definitions *)
                  let st = check_bell_order bds st in
                  kont st res
              end

          | InsMatch (loc,e,cls,d) ->
              let v = eval_st st e in
              begin match v with
              | V.Tag (_,s) ->
                  let rec match_rec = function
                    | [] ->
                        begin match d with
                        | Some dseq -> run st dseq kfail kont res
                        | None ->
                            error_not_silent
                              loc "pattern matching failed on value '%s'" s
                        end
                    | (ps,pprog)::cls ->
                        if s = ps then run st pprog kfail kont res
                        else match_rec cls in
                  match_rec cls
              | V.Empty ->
                  error_not_silent (get_loc e) "matching on empty"
              | V.Unv ->
                  error_not_silent (get_loc e) "matching on universe"
              | _ ->
                  error_not_silent (get_loc e)
                    "matching on non-tag value of type '%s'"
                    (pp_typ (type_val v))
              end

          | Include (loc,fname) ->
              let fname = match fname with
              | "lock.cat" when O.compat -> "cos-opt.cat"
              | _ -> fname in
              if
                StringSet.mem fname st.st.included &&
                (O.verbose > 0 || O.debug_files)
              then begin
                Warn.warn_always "%a: including %s another time"
                  TxtLoc.pp loc fname
              end ;
              do_include loc fname st kfail kont res
          | Procedure (_,name,args,body,is_rec) ->
              let p =  { proc_args=args; proc_env=st.env; proc_body=body; } in
              let proc = Proc p in
              let env_plus_p = do_add_val name (lazy proc) st.env in
              begin match is_rec with
              | IsRec -> p.proc_env <- env_plus_p
              | IsNotRec -> () end ;
              kont { st with env = env_plus_p } res
          | Call (loc,name,es,tname) when not O.bell ->
              let skip =
                skip_this_check tname || skip_this_check (Some name) in
              if O.debug && skip then
                warn loc "skipping call: %s"
                  (match tname with | Some n -> n | None -> name);
              if skip && not O.strictskip then (* won't call *)
                kont st res
              else (* will call *)
                let env0 = from_st st in
                let p = protect_call st (eval_proc loc env0) name in
                let env1 =
                  protect_call st
                    (fun penv ->
                      add_args loc p.proc_args (eval env0 es) env0 penv)
                    p.proc_env in
                if skip then (* call for boolean... *)
                  let pname = match tname with
                  | Some _ -> tname
                  | None -> Some name in
                  let tval =
                    let benv = env1 in
                    run { (push_loc st (loc,pname)) with env=benv; } p.proc_body
                      (fun x -> x)
                      (fun _ _ -> true) false in
                  if tval then kont st res
                  else
                    kont
                      { st with skipped =
                        StringSet.add (Misc.as_some tname) st.skipped;}
                      res
                else
                  let pname = match tname with
                  | Some _ -> tname
                  | None -> Some name in
                  let st = push_loc st (loc,pname) in
                  run { st with env = env1; } p.proc_body kfail
                    (fun st_call res ->
                      let st_call = pop_loc st_call in
                      kont { st_call with env = st.env ;} res) (* Note, show is preserved *)
                    res
          | Enum (loc,name,xs) ->
              let env = st.env in
              let tags =
                List.fold_left
                  (fun env x -> StringMap.add x name env)
                  env.tags xs in
              let enums = StringMap.add name xs env.enums in
(* add a set of all tags... *)
              let alltags =
                lazy begin
                  let vs =
                    List.fold_left
                      (fun k x -> ValSet.add (V.Tag (name,x)) k)
                      ValSet.empty xs in
                  V.ValSet (TTag name,vs)
                end in
              let env = do_add_val name alltags env in
              if O.debug && O.verbose > 1 then
                warn loc "adding set of all tags for %s" name ;
              let env = { env with tags; enums; } in
              let st = { st with env;} in
              let st = check_bell_enum loc st name xs in
              kont st res
          | Forall (_loc,x,e,body) when not O.bell  ->
              let st0 = st in
              let env0 = st0.env in
              let v = eval (from_st st0) e in
              begin match tag2set v with
              | V.Empty -> kont st res
              | ValSet (_,set) ->
                  let rec run_set st vs res =
                    if ValSet.is_empty vs then
                      kont st res
                    else
                      let v =
                        try ValSet.choose vs
                        with Not_found -> assert false in
                      let env = do_add_val x (lazy v) env0 in
                      run { st with env;} body kfail
                        (fun st res ->
                          run_set { st with env=env0;} (ValSet.remove v vs) res)
                        res in
                  run_set st set res
              | _ ->
                  error_not_silent
                    (get_loc e) "forall instruction applied to non-set value"
              end
          | WithFrom (_loc,x,e) when not O.bell  ->
              let st0 = st in
              let env0 = st0.env in
              let v = eval (from_st st0) e in
              begin match v with
              | V.Empty -> kfail res
              | ValSet (_,vs) ->
                  ValSet.fold
                    (fun v res ->
                      let env = do_add_val x (lazy v) env0 in
                      kont (doshowone x {st with env;}) res)
                    vs res
              | _ -> error_not_silent (get_loc e) "set expected"
              end
          | Events (loc,x,es,def) when O.bell ->
              let x = BellName.tr_compat x in
              if not (StringSet.mem x BellName.all_sets) then
                error_not_silent loc
                  "event type %s is not part of legal {%s}\n"
                  x (StringSet.pp_str "," Misc.identity BellName.all_sets) ;
              let vs = List.map (eval_loc (from_st st)) es in
              let event_sets =
                List.map
                  (fun (loc,v) -> match v with
                  | ValSet(TTag _,elts) ->
                      let tags =
                        ValSet.fold
                          (fun elt k ->
                            let tag = as_tag elt in match tag with
                            | "release" when O.compat ->
                                "assign"::"release"::k
                            | "acquire"when O.compat ->
                                "deref"::"lderef"::"acquire"::k
                            | _ -> tag::k)
                          elts [] in
                      StringSet.of_list tags
                  | V.Tag (_,tag) -> StringSet.singleton tag
                  | _ ->
                      error false loc
                        "event declaration expected a set of tags, found %s"
                        (pp_val v))
                  vs in
              let bell_info =
                BellModel.add_events x event_sets st.bell_info in
              let bell_info =
                if def then
                  let defarg =
                    List.map2
                      (fun ss e -> match StringSet.as_singleton ss with
                      | None ->
                          error_not_silent (get_loc e) "ambiguous default declaration"
                      | Some a -> a) event_sets es in
                  try BellModel.add_default x defarg bell_info
                  with BellModel.Defined ->
                    error_not_silent loc "second definition of default for %s" x
                else bell_info in
              let st = { st with bell_info;} in
              kont st res
          | Events _ ->
              assert (not O.bell) ;
              kont st res (* Ignore bell constructs when executing model *)
          | Test _|UnShow _|Show _|ShowAs _
          | Call _|Forall _
          | WithFrom _ ->
              assert O.bell ;
              kont st res (* Ignore cat constructs when executing bell *)

          and exec_test :
                'a.st -> app_test -> test_type ->
                  ('a -> 'a) ->
                    (st -> 'a -> 'a) -> 'a -> 'a =
                      fun st (loc,_,t,e,name as tst) test_type kfail kont res ->
                        let skip = skip_this_check name in
                        let cycle = cycle_this_check name in
                        if O.debug &&  skip then warn loc "skipping check: %s" (Misc.as_some name) ;
                        if
                          O.strictskip || not skip || cycle
                        then
                          let ok = eval_test (check_through test_type) (from_st st) t e in
                          if
                            cycle &&
                            begin match ok,t with
                            | (false,Yes _)
                            | (true,No _) -> true
                            | (false,No _)
                            | (true,Yes _) -> false
                            end
                          then show_cycle st tst ;
                          if ok then
                            match test_type with
                            | Check|UndefinedUnless|Assert -> kont st res
                            | Flagged ->
                                begin match name with
                                | None ->
                                    warn loc "this flagged test does not have a name" ;
                                    kont st res
                                | Some name ->
                                    if O.debug then
                                      warn loc "flag %s recorded" name ;
                                    kont
                                      {st with flags=
                                       Flag.Set.add (Flag.Flag name) st.flags;}
                                      res
                                end
                          else begin
                            if skip then begin
                              assert O.strictskip ;
                              kont
                                { st with
                                  skipped = StringSet.add (Misc.as_some name) st.skipped;}
                                res
                            end else begin
                              match test_type with
                              | Check ->
                                  if O.debug then pp_check_failure st tst ;
                                  kfail res
                              | UndefinedUnless ->
                                  kont {st with flags=Flag.Set.add Flag.Undef st.flags;} res
                              | Flagged -> kont st res
                              | Assert ->
                                 let a = match name with
                                   | None ->
                                      "(unknown)"
                                   | Some name ->
                                      name
                                 in
                                 Warn.user_error "%s assertion failed, check input test." a
                              end
                          end else begin
                            W.warn "Skipping check %s" (Misc.as_some name) ;
                            kont st res
                          end

          and do_include : 'a . TxtLoc.t -> string ->st -> ('a -> 'a) ->
            (st -> 'a -> 'a) -> 'a -> 'a =
              fun loc fname st kfail kont res ->
                (* Run sub-model file *)
                if O.debug then warn loc "include \"%s\"" fname ;
                let module P =
                  ParseModel.Make
                    (struct
                      include LexUtils.Default
                      let libfind = O.libfind
                    end) in
                let (_,_,iprog) =
                  try P.parse fname
                  with Misc.Fatal msg | Misc.UserError msg ->
                    error_not_silent loc "%s" msg  in
                let stst = st.st in
                let included = StringSet.add fname stst.included in
                let stst = { stst with included; } in
                let st = { st with st=stst; } in
                run st iprog kfail kont res

          and run : 'a.st -> ins list ->
            ('a -> 'a) -> (st -> 'a -> 'a) -> 'a -> 'a =
           fun st c kfail kont ->
            match c with
            | [] -> kont st
            | i :: c ->
                if O.profile then
                  let t0 = Sys.time () in
                  exec st i kfail @@ fun st res ->
                  let () = end_profile ~t0 ~loc:(ASTUtils.ins2loc i) in
                  run st c kfail kont res
                else exec st i kfail @@ fun st res -> run st c kfail kont res
      in

      fun ks m vb_pp kont res ->
(* Primitives *)
        let m = add_primitives ks (env_from_ienv m) in
(* Initial show's *)
        if _dbg then begin
          eprintf "showsome=%b, doshow={%s}\n" O.showsome
            (StringSet.pp_str ", " Misc.identity O.doshow)
        end ;
        let show =
          if O.showsome then
            lazy begin
              let show =
                List.fold_left
                  (fun show (tag,v) ->
                    StringMap.add tag (Shown.Rel v) show)
                  StringMap.empty (Lazy.force vb_pp) in
              StringSet.fold
                (fun tag show ->
                  StringMap.add tag (find_show_shown ks m tag) show)
                O.doshow show
            end else lazy StringMap.empty in

        let st =
          {env=m; show=show; skipped=StringSet.empty;
           flags=Flag.Set.empty;
           ks; bell_info=BellModel.empty_info;
           st=inter_st_empty; } in

        let kont st res =  kont (st2out st) res in

        let just_run st res = run st mprog kfail kont res in
        do_include TxtLoc.none "stdlib.cat" st kfail
          (match O.bell_fname with
          | None -> just_run (* No bell file, just run *)
          | Some fname ->
              fun st res ->
                do_include TxtLoc.none fname st kfail just_run res)
          res
  end
