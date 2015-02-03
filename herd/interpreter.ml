(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Interpreter for a user-specified model *)

open Printf

module type S = sig

  module S : Sem.Semantics

(* Values *)
  type ks =
      { id : S.event_rel Lazy.t; unv : S.event_rel Lazy.t;
        evts : S.event_set; }

  module V : sig type env end


(* Helpers, initialisation *)
  val env_empty : V.env
  val add_rels : V.env -> (string * S.event_rel Lazy.t) list -> V.env
  val add_sets : V.env -> (string * S.event_set Lazy.t) list -> V.env

(* State of interpreter *)

  type st = {
    env : V.env ;
    show : S.event_rel StringMap.t Lazy.t ;
    seen_requires_clause : bool ;
    skipped : StringSet.t ;
  }

  val show_to_vbpp :
    st -> (StringMap.key * S.event_rel) list

  val interpret :
    (unit -> unit) -> (* function called when a requires clause fails *)
    S.test ->
    S.concrete ->
    V.env ->
    ks ->
    (StringMap.key * S.event_rel) list Lazy.t ->
    (st -> 'a -> 'a) -> 'a -> 'a
end


module type Config = sig
  val m : AST.pp_t
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    :
    (S with module S = S)
    =
  struct

    let dbg = false

(****************************)
(* Convenient abbreviations *)
(****************************)

    module S = S
    module A = S.A
    module E = S.E
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)
    module W = Warn.Make(O)
(*  Model interpret *)
    let (txt,(_,_,prog)) = O.m

(*
  let debug_proc chan p = fprintf chan "%i" p
  let debug_event chan e = fprintf chan "%s" (E.pp_eiid e)
  let debug_set chan s =
  output_char chan '{' ;
  E.EventSet.pp chan "," debug_event s ;
  output_char chan '}'

  let debug_events = debug_set

  let debug_rel chan r =
  E.EventRel.pp chan ","
  (fun chan (e1,e2) -> fprintf chan "%a -> %a"
  debug_event e1 debug_event e2)
  r
 *)

    type ks =
        { id : S.event_rel Lazy.t; unv : S.event_rel Lazy.t;
          evts : S.event_set; }

(* Internal typing *)
    type typ =
      | TEmpty | TEvents | TRel | TTag of string |TClo | TProc | TSet of typ

    let rec eq_type t1 t2 = match t1,t2 with
    | TEmpty,TSet _ -> Some t2
    | TSet _,TEmpty -> Some t1
    | TSet t1,TSet t2 ->
        begin match eq_type t1 t2 with
        | None -> None
        | Some t -> Some (TSet t)
        end
    | _,_ -> if t1 = t2 then Some t1 else None


    let type_equal t1 t2 = match eq_type t1 t2 with
    | None -> false
    | Some _ -> true

    exception CompError of string


    let rec pp_typ = function
      | TEmpty -> "{}"
      | TEvents -> "event set"
      | TRel -> "rel"
      | TTag ty -> ty
      | TClo -> "closure"
      | TProc -> "procedure"
      | TSet elt -> sprintf "%s set" (pp_typ elt)



(*
  module V = Ivalue.Make(S)
 *)

    module rec V : sig
      type v =
        | Empty | Unv
        | Rel of S.event_rel
        | Set of S.event_set
        | Clo of closure
        | Proc of procedure
        | Tag of string * string     (* type  X name *)
        | ValSet of typ * ValSet.t   (* elt type X set *)
      and env =
          { vals  : v Lazy.t StringMap.t;
            enums : string list StringMap.t;
            tags  : string StringMap.t; }
      and closure =
          { clo_args : AST.var list ;
            mutable clo_env : env ;
            clo_body : AST.exp;
            clo_name : string; }
      and procedure = {
          proc_args : AST.var list;
          proc_env : env;
          proc_body : AST.ins list; }

      val type_val : v -> typ
    end = struct

      type v =
        | Empty | Unv
        | Rel of S.event_rel
        | Set of S.event_set
        | Clo of closure
        | Proc of procedure
        | Tag of string * string     (* type  X name *)
        | ValSet of typ * ValSet.t   (* elt type X set *)

      and env =
          { vals  : v Lazy.t StringMap.t;
            enums : string list StringMap.t;
            tags  : string StringMap.t; }

      and closure =
          { clo_args : AST.var list ;
            mutable clo_env : env ;
            clo_body : AST.exp;
            clo_name : string; }

      and procedure = {
          proc_args : AST.var list;
          proc_env : env;
          proc_body : AST.ins list; }

      let type_val = function
        | Empty -> TEmpty
        | Unv -> assert false (* Discarded before *)
        | Rel _ -> TRel
        | Set _ -> TEvents
        | Clo _ -> TClo
        | Proc _ -> TProc
        | Tag (t,_) -> TTag t
        | ValSet (t,_) -> TSet t


    end
    and ValOrder : Set.OrderedType = struct
      (* Note: cannot use Full in sets.. *)
      type t = V.v
      open V

      let error fmt = ksprintf (fun msg -> raise (CompError msg)) fmt

      let compare v1 v2 = match v1,v2 with
      | V.Empty,V.Empty -> 0
(* Expand all legitimate empty's *)
      | V.Empty,ValSet (_,s) -> ValSet.compare ValSet.empty s
      | ValSet (_,s),V.Empty -> ValSet.compare s ValSet.empty
      | V.Empty,Rel r -> E.EventRel.compare E.EventRel.empty r
      | Rel r,V.Empty -> E.EventRel.compare r E.EventRel.empty
      | V.Empty,Set s -> E.EventSet.compare E.EventSet.empty s
      | Set s,V.Empty -> E.EventSet.compare s E.EventSet.empty
(* Legitimate cmp *)
      | Tag (t1,s1), Tag (t2,s2) when t1=t2 ->
          String.compare s1 s2
      | ValSet (_,s1),ValSet (_,s2) -> ValSet.compare s1 s2
      | Rel r1,Rel r2 -> E.EventRel.compare r1 r2
      | Set s1,Set s2 -> E.EventSet.compare s1 s2
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

    end and ValSet : (MySet.S with type elt = V.v) = MySet.Make(ValOrder)


    let error loc fmt =
      ksprintf
        (fun msg ->
          eprintf "%a: %s\n" TxtLoc.pp loc msg ;
          raise Misc.Exit) (* Silent failure *)
        fmt

    let set_op loc t op s1 s2 =
      try V.ValSet (t,op s1 s2)
      with CompError msg -> error loc "%s" msg

    open V

(* pretty *)
    let rec pp_val = function
      | Unv -> "<universe>"
      | V.Empty -> "{}"
      | Tag (_,s) -> sprintf "'%s" s
      | ValSet (_,s) ->
          sprintf "{%s}" (ValSet.pp_str "," pp_val s)
      | v -> sprintf "<%s>" (pp_typ (type_val v))

(* lift a tag to a singleton set *)
    let tag2set v = match v with
    | Tag (t,_) -> ValSet (TTag t,ValSet.singleton v)
    | _ -> v


(* Add values to env *)
    let add_val k v env = { env with vals = StringMap.add k v env.vals; }

    let env_empty =
      {vals=StringMap.empty;
       enums=StringMap.empty;
       tags=StringMap.empty; }

    let add_vals mk env bds =
      let vals =
        List.fold_left
          (fun vals (k,v) -> StringMap.add k (mk v) vals)
          env.vals bds in
      { env with vals; }


    let add_rels env bds =
      add_vals (fun v -> lazy (Rel (Lazy.force v))) env bds

    and add_sets env bds =
      add_vals (fun v -> lazy (Set (Lazy.force v))) env bds

    type st = {
        env : V.env ;
        show : S.event_rel StringMap.t Lazy.t ;
        seen_requires_clause : bool ;
        skipped : StringSet.t ;
      }

    let tags_universe {enums=env} t =
      let tags =
        try StringMap.find t env
        with Not_found -> assert false in
      let tags = ValSet.of_list (List.map (fun s -> Tag (t,s)) tags) in
      tags

    let find_env {vals=env} k =
      Lazy.force begin
        try StringMap.find k env
        with
        | Not_found -> Warn.user_error "unbound var: %s" k
      end

    let find_env_loc loc env k =
      try  find_env env k
      with Misc.UserError msg -> error loc "%s" msg

(* find without forcing lazy's *)
    let just_find_env fail loc env k =
      try StringMap.find k env.vals
      with Not_found ->
        if fail then error loc "unbound var: %s" k
        else raise Not_found

    let as_rel ks = function
      | Rel r -> r
      | Empty -> E.EventRel.empty
      | Unv -> Lazy.force ks.unv
      | v ->
          eprintf "not a relation: '%s'\n" (pp_val v) ;
          assert false

    let as_set ks = function
      | Set s -> s
      | Empty -> E.EventSet.empty
      | Unv -> ks.evts
      | _ -> assert false

    let as_valset = function
      | ValSet (_,v) -> v
      | _ -> assert false

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
            raise (Stabilised (type_val w))

      end
      | _,_ -> assert false in
      stabilised

    open AST


(* Syntactic function *)
    let is_fun = function
      | Fun _ -> true
      | _ -> false


(* Get an expression location *)
    let get_loc = function
      | Konst (loc,_)
      | Tag (loc,_)
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
        -> loc


(* State of interpreter *)

    let rt_loc lbl =
      if
        O.verbose <= 1 &&
        not (StringSet.mem lbl S.O.PC.symetric) &&
        not (StringSet.mem lbl S.O.PC.showraw)
      then S.rt else (fun x -> x)

    let show_to_vbpp st =
      StringMap.fold (fun tag v k -> (tag,v)::k)   (Lazy.force st.show) []

    let empty_rel = Rel E.EventRel.empty
    let noid r =
      Rel
        (E.EventRel.filter
           (fun (e1 ,e2) -> not (E.event_equal e1 e2))
           r)
    let sameloc r =
      Rel
        (E.EventRel.filter
           (fun (e1 ,e2) -> E.same_location e1 e2)
           r)
        
    let error_typ loc t0 t1  =
      error loc"type %s expected, %s found" (pp_typ t0) (pp_typ t1)

    let error_rel loc v = error_typ loc TRel (type_val v)
    and error_set loc v = error_typ loc TEvents (type_val v)

    let type_list = function
      | [] -> assert false
      | (_,v)::vs ->
          let rec type_rec t0 = function
            | [] -> t0,[]
            | (loc,v)::vs ->
                let t1 = type_val v in
                match eq_type t0 t1 with
                | Some t0 ->
                    let t0,vs = type_rec t0 vs in
                    t0,v::vs
                | None ->
                    error loc
                      "type %s expected, %s found" (pp_typ t0) (pp_typ t1) in
          let t0,vs = type_rec (type_val v) vs in
          t0,v::vs

(* Helpers for n-ary operations *)

(* Check explicit set arguments *)
    let set_args =
      let rec s_rec = function
        | [] -> []
        | (loc,Unv)::_ ->
            error loc "universe in explicit set"
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

(* Sequence applies to relations *)
    let seq_args ks =
      let rec seq_rec = function
        | [] -> []
        | (_,V.Empty)::_ -> raise Exit
        | (_,V.Unv)::xs -> Lazy.force ks.unv::seq_rec xs
        | (_,Rel r)::xs -> r::seq_rec xs
        | (loc,v)::_ -> error_rel loc v in
      seq_rec

    let is_dir = function
        (* Todo: are these still needed? *)
      | Unv_Set -> (fun _ -> true)
      | Bar_Set -> E.is_barrier
      | WriteRead -> E.is_mem
      | Write -> E.is_mem_store
      | Read -> E.is_mem_load
      | Atomic -> E.is_atomic
      | Plain -> fun e -> not (E.is_atomic e)

(* interpreter *)
(* For all success call kont, accumulating results *)
    let interpret
        failed_requires_clause test conc m ks vb_pp kont res =

      let rec eval_loc env e = get_loc e,eval env e

      and eval env = function
        | Konst (_,Empty SET) -> V.Empty (* Polymorphic empty *)
        | Konst (_,Empty RLN) -> empty_rel
        | AST.Tag (loc,s) ->
            begin try
              V.Tag (StringMap.find s env.tags,s)
            with Not_found ->
              error loc "tag '%s is undefined" s
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
            | Rel r -> Rel (S.tr r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Star,e) ->
            begin match eval env e with
            | V.Empty -> Rel (Lazy.force ks.id)
            | Unv -> Unv
            | Rel r -> Rel (S.union (S.tr r) (Lazy.force ks.id))
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Opt,e) ->
            begin match eval env e with
            | V.Empty -> Rel (Lazy.force ks.id)
            | Unv -> Unv
            | Rel r -> Rel (S.union r (Lazy.force ks.id))
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Select (s1,s2),e) ->
            let f1 = is_dir s1 and f2 = is_dir s2 in
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv ->
                Rel (S.restrict f1 f2 (Lazy.force ks.unv))
            | Rel r ->
                Rel (S.restrict f1 f2 r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Comp _,e) -> (* Back to polymorphism *)
            begin match eval env e with
            | V.Empty -> Unv
            | Unv -> V.Empty
            | Set s ->
                Set (E.EventSet.diff ks.evts s)
            | Rel r ->
                Rel (E.EventRel.diff (Lazy.force ks.unv) r)
            | ValSet (TTag ts as t,s) ->
                ValSet (t,ValSet.diff (tags_universe env ts) s)
            | v ->
                error (get_loc e)
                  "set or relation expected, %s found"
                  (pp_typ (type_val v))
            end
        | Op1 (_,Inv,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Unv
            | Rel r -> Rel (E.EventRel.inverse r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Square,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Unv
            | Set s -> Rel (E.EventRel.cartesian s s)
            | v -> error_set (get_loc e) v
            end
        | Op1 (_,Ext,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Rel (U.ext (Lazy.force ks.unv))
            | Rel r -> Rel (U.ext r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Int,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Rel (U.internal (Lazy.force ks.unv))
            | Rel r -> Rel (U.internal r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,NoId,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> noid (Lazy.force ks.unv)
            | Rel r -> noid r
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Set_to_rln,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Rel (Lazy.force ks.id)
            | Set s ->  Rel (E.EventRel.set_to_rln s)
            | v -> error_set (get_loc e) v
            end
        | Op1 (_,SameLoc,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> sameloc (Lazy.force ks.unv)
            | Rel r -> sameloc r
            | v -> error_rel (get_loc e) v
            end
(* One xplicit N-ary operator *)
        | ExplicitSet (loc,es) ->
            let vs = List.map (eval_loc env) es in
            let vs = set_args vs in
            begin match vs with
            | [] -> V.Empty
            | _ ->
                let t,vs = type_list vs in
                try ValSet (t,ValSet.of_list vs)
                with CompError msg ->
                  error loc "%s" msg
            end
(* N-ary operators, those associative binary operators are optimized *)
        | Op (loc,Union,es) ->
            let vs = List.map (eval_loc env) es in
            begin try
              let vs = union_args vs in
              match vs with
              | [] -> V.Empty
              | _ ->
                  let t,vs = type_list vs in
                  match t with
                  | TRel -> Rel (S.unions (List.map (as_rel ks) vs))
                  | TEvents ->
                      Set (E.EventSet.unions  (List.map (as_set ks) vs))
                  | TSet telt ->
                      ValSet (telt,ValSet.unions (List.map as_valset vs))
                  | ty ->
                      error loc
                        "cannot perform union on type '%s'" (pp_typ ty)
            with Exit -> Unv end
        | Op (_,Seq,es) ->
            let vs = List.map (eval_loc env) es in
            begin try
              let vs = seq_args  ks vs in
              match vs with
              | [] -> Rel (Lazy.force ks.id)
              | _ -> Rel (S.seqs vs)
            with Exit -> empty_rel
            end
(* Binary operators *)
        | Op (loc,Inter,[e1;e2;]) -> (* Binary notation kept in parser *)
            let loc1,v1 = eval_loc env e1
            and loc2,v2 = eval_loc env e2 in
            begin match tag2set v1,tag2set v2 with
            | (V.Tag _,_)|(_,V.Tag _) -> assert false
            | Rel r1,Rel r2 -> Rel (E.EventRel.inter r1 r2)
            | Set s1,Set s2 -> Set (E.EventSet.inter s1 s2)
            | ValSet (t,s1),ValSet (_,s2) ->
                set_op loc t ValSet.inter s1 s2
            | (Unv,r)|(r,Unv) -> r
            | (V.Empty,_)|(_,V.Empty) -> V.Empty
            | (Clo _|Proc _),_ ->
                error loc1
                  "intersection on %s" (pp_typ (type_val v1))
            | _,(Clo _|Proc _) ->
                error loc2
                  "intersection on %s" (pp_typ (type_val v2))
            | (Rel _,Set _)
            | (Set _,Rel _)
            | (Rel _,ValSet _)
            | (ValSet _,Rel _) ->
                error loc "mixing sets and relations in intersection"
            | (ValSet _,Set _)
            | (Set _,ValSet _) ->
                error loc "mixing event sets and sets in intersection"
            end
        | Op (loc,Diff,[e1;e2;]) ->
            let loc1,v1 = eval_loc env e1
            and loc2,v2 = eval_loc env e2 in
            begin match tag2set v1,tag2set v2 with
            | (V.Tag _,_)|(_,V.Tag _) -> assert false
            | Rel r1,Rel r2 -> Rel (E.EventRel.diff r1 r2)
            | Set s1,Set s2 -> Set (E.EventSet.diff s1 s2)
            | ValSet (t,s1),ValSet (_,s2) ->
                set_op loc t ValSet.diff s1 s2
            | Unv,Rel r -> Rel (E.EventRel.diff (Lazy.force ks.unv) r)
            | Unv,Set s -> Set (E.EventSet.diff ks.evts s)
            | Unv,ValSet (TTag ts as t,s) ->
                ValSet (t,ValSet.diff (tags_universe env ts) s)
            | Unv,ValSet (t,_) ->
                error loc1 "cannot build universe for element type %s"
                  (pp_typ t)
            | Unv,V.Empty -> Unv
            | (Rel _|Set _|V.Empty|Unv|ValSet _),Unv
            | V.Empty,(Rel _|Set _|V.Empty|ValSet _) -> V.Empty
            | (Rel _|Set _|ValSet _),V.Empty -> v1
            | (Clo _|Proc _),_ ->
                error loc1
                  "difference on %s" (pp_typ (type_val v1))
            | _,(Clo _|Proc _) ->
                error loc2
                  "difference on %s" (pp_typ (type_val v2))
            | ((Set _|ValSet _),Rel _)|(Rel _,(Set _|ValSet _)) ->
                error loc "mixing set and relation in difference"
            | (Set _,ValSet _)|(ValSet _,Set _) ->
                error loc "mixing event set and set in difference"
            end
        | Op (_,Cartesian,[e1;e2;]) ->
            let s1 = eval_set env e1
            and s2 = eval_set env e2 in
            Rel (E.EventRel.cartesian s1 s2)
        | Op (loc,Add,[e1;e2;]) ->
            let v1 = eval env e1
            and v2 = eval env e2 in
            begin match v1,v2 with
            | V.Unv,_ -> error loc "universe in set ++"
            | _,V.Unv -> V.Unv
            | _,V.Empty -> V.ValSet (type_val v1,ValSet.singleton v1)
            | V.Empty,V.ValSet (TSet e2 as t2,s2) ->
                let v1 = ValSet (e2,ValSet.empty) in
                set_op loc t2 ValSet.add v1 s2
            | _,V.ValSet (_,s2) ->
                set_op loc (type_val v1) ValSet.add v1 s2
            | _,(Rel _|Set _|Clo _|Proc _|V.Tag (_, _)) ->
                error (get_loc e2)
                  "this expression of type '%s' should be a set"
                  (pp_typ (type_val v2))
            end
        | Op (_,(Diff|Inter|Cartesian|Add),_) -> assert false (* By parsing *)
(* Application/bindings *)
        | App (loc,f,es) ->
            let f = eval_clo env f in
            let env = add_args loc f.clo_args es env f.clo_env in
            begin try eval env f.clo_body
            with Misc.Exit ->
              error loc "Calling"
            end
        | Bind (_,bds,e) ->
            let env = eval_bds env bds in
            eval env e
        | BindRec (loc,bds,e) ->
            let env = env_rec loc (fun pp -> pp) bds env in
            eval env e
        | Match (loc,e,cls,d) ->
            let v = eval env e in
            begin match v with
            | V.Tag (_,s) ->
                let rec match_rec = function
                  | [] ->
                      begin match d with
                      | Some e ->  eval env e
                      | None ->
                          error loc "pattern matching failed on value '%s'" s
                      end
                  | (ps,es)::cls ->
                      if s = ps then eval env es
                      else match_rec cls in
                match_rec cls
            | V.Empty ->
                error (get_loc e) "matching on empty"
            | V.Unv ->
                error (get_loc e) "matching on universe"
            | _ ->
                error (get_loc e) "matching on non-tag value of type '%s'"
                  (pp_typ (type_val v))
            end
        | MatchSet (loc,e,ife,(x,xs,ex)) ->
            let v = eval env e in
            begin match v with
            | V.Empty -> eval env ife
            | V.Unv ->
                error loc
                  "%s" "Cannot set-match on universe"
            | V.ValSet (t,s) ->
                if ValSet.is_empty s then
                  eval env ife
                else
                  let elt =
                    lazy begin
                      try ValSet.choose s
                      with Not_found -> assert false
                    end in
                  let s =
                    lazy begin
                      try ValSet (t,ValSet.remove (Lazy.force elt) s)
                      with  CompError _ -> assert false
                    end in
                  let env = add_val x elt env in
                  let env = add_val xs s env in
                  eval env ex
            | _ ->
                error (get_loc e) "set-matching on non-set value of type '%s'"
                  (pp_typ (type_val v))
            end

      and eval_fun is_rec env loc xs body name fvs =
        if dbg then begin
          let sz =
            StringMap.fold
              (fun _ _ k -> k+1) env.vals 0 in
          let fs = StringSet.pp_str "," (fun x -> x) fvs in
          eprintf "Closure %s, env=%i, free={%s}\n" name sz fs
        end ;
        let vals =
          StringSet.fold
            (fun x k ->
              try
                let v = just_find_env (not is_rec) loc env x in
                StringMap.add x v k
              with Not_found -> k)
            fvs StringMap.empty in
        let env = { env with vals; } in
        {clo_args=xs; clo_env=env; clo_body=body; clo_name=name; }

      and add_args loc xs es env_es env_clo =
        let vs = List.map (eval env_es) es in
        let bds =
          try
            List.combine xs vs
          with _ -> error loc "argument_mismatch" in
        List.fold_right
          (fun (x,v) env -> add_val x (lazy v) env)
          bds env_clo

      and eval_rel env e =  match eval env e with
      | Rel v -> v
      | _ -> error (get_loc e) "relation expected"

      and eval_set env e = match eval env e with
      | Set v -> v
      | V.Empty -> E.EventSet.empty
      | Unv -> ks.evts
      | _ -> error (get_loc e) "set expected"

      and eval_clo env e = match eval env e with
      | Clo v -> v
      | _ -> error (get_loc e) "closure expected"

      and eval_proc loc env x = match find_env_loc loc env x with
      | Proc p -> p
      | _ ->
          Warn.user_error "procedure expected"

(* For let *)
      and eval_bds env bds = match bds with
      | [] -> env
      | (k,e)::bds ->
          let v = eval env e in
          (*
            begin match v with
            | Rel r -> printf "Defining relation %s = {%a}.\n" k debug_rel r
            | Set s -> printf "Defining set %s = %a.\n" k debug_set s
            | Clo _ -> printf "Defining function %s.\n" k
            end;
           *)
          add_val k (lazy v) (eval_bds env bds)

(* For let rec *)

      and env_rec loc pp bds =
        let fs,nfs =  List.partition  (fun (_,e) -> is_fun e) bds in
        match nfs with
        | [] -> env_rec_funs loc fs
        | _  -> env_rec_vals loc pp fs nfs


(* Recursive functions *)
      and env_rec_funs _loc bds env =
        let clos =
          List.map
            (function
              | f,Fun (loc,xs,body,name,fvs) ->
                  f,eval_fun true env loc xs body name fvs,fvs
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
              add_funs (fun x -> StringSet.mem x fvs) clo.clo_env)
          clos ;
        add_funs (fun _ -> true) env


(* Compute fixpoint of relations *)
      and env_rec_vals loc pp funs bds =
        let rec fix  k env vs =
          if O.debug && O.verbose > 1 then begin
            let vb_pp =
              List.fold_left2
                (fun k (x,_) v ->
                  try
                    let v = match v with
                    | V.Empty -> E.EventRel.empty
                    | Unv -> Lazy.force ks.unv
                    | Rel r -> r
                    | _ -> raise Exit in
                    (x, rt_loc x v)::k
                  with Exit -> k)
                [] bds vs in
            let vb_pp = pp vb_pp in
            MU.pp_failure test conc
              (sprintf "Fix %i" k)
              vb_pp
          end ;
          let env,ws = fix_step env bds in
          let ok =
            try stabilised ks env vs ws
            with Stabilised t ->
              error loc "illegal recursion on type '%s'" (pp_typ t) in
          if ok then env
          else
            (* Update recursive functions *)
            let env = env_rec_funs loc funs env in
            fix (k+1) env ws in
        fun env ->
          let env0 =
            List.fold_left
              (fun env (k,_) -> add_val k (lazy V.Empty) env)
              env bds in
          let env0 = env_rec_funs loc funs env0 in
          fix 0 env0 (List.map (fun _ -> V.Empty) bds)

      and fix_step env bds = match bds with
      | [] -> env,[]
      | (k,e)::bds ->
          let v = eval env e in
          let env = add_val k (lazy v) env in
          let env,vs = fix_step env bds in
          env,(v::vs) in

(* Showing bound variables, (-doshow option) *)

      let find_show_rel env x =
        let as_rel v = match v with
        | Rel r -> r
        | V.Empty -> E.EventRel.empty
        | Unv -> Lazy.force ks.unv
        | v ->
            Warn.warn_always
              "Warning show: %s is not a relation: '%s'" x (pp_val v) ;
            raise Not_found in
        try
          rt_loc x (as_rel (Lazy.force (StringMap.find x env.vals)))
        with Not_found -> E.EventRel.empty in

      let doshowone x st =
        if StringSet.mem x  S.O.PC.doshow then
          let show =
            lazy begin
              StringMap.add x (find_show_rel st.env x) (Lazy.force st.show)
            end in
          { st with show;}
        else st in

      let doshow bds st =
        let to_show =
          StringSet.inter S.O.PC.doshow (StringSet.of_list (List.map fst bds)) in
        if StringSet.is_empty to_show then st
        else
          let show = lazy begin
            StringSet.fold
              (fun x show  ->
                let r = find_show_rel st.env x in
                StringMap.add x r show)
              to_show
              (Lazy.force st.show)
          end in
          { st with show;} in

(* Execute one instruction *)

      let rec exec txt st i kont res =  match i with
      | Debug (_,e) ->
          let v = eval st.env e in
          eprintf "%a: value is %s\n"
            TxtLoc.pp (get_loc e) (pp_val v) ;
          kont st res
      | Show (_,xs) ->
          let show = lazy begin
            List.fold_left
              (fun show x ->
                StringMap.add x (find_show_rel st.env x) show)
              (Lazy.force st.show) xs
          end in
          kont { st with show;} res
      | UnShow (_,xs) ->
          let show = lazy begin
            List.fold_left
              (fun show x -> StringMap.remove x show)
              (Lazy.force st.show) xs
          end in
          kont { st with show;} res
      | ShowAs (_,e,id) ->
          let show = lazy begin
            StringMap.add id
              (rt_loc id (eval_rel st.env e)) (Lazy.force st.show)
          end in
          kont { st with show; } res
      | ProcedureTest (loc,pname,es,name) ->
          let skip_this_check =
            match name with
            | Some name -> StringSet.mem name O.skipchecks
            | None -> false in
          if
            O.strictskip || not skip_this_check
          then
            let env0 = st.env in
            let p = eval_proc loc env0 pname in
            let env1 = add_args loc p.proc_args es env0 p.proc_env in
            run txt  { st with env = env1; } p.proc_body
              (fun st_call res ->  kont { st_call with env=env0;} res)
              res
          else
            let () = W.warn "Skipping check %s" (Misc.as_some name) in
            kont st res
      | Test (_,pos,t,e,name,test_type) ->
          (* If this is a provides-clause and we've previously
             seen a requires-clause, abort. *)
          if st.seen_requires_clause && test_type = Provides then
            begin
              let pp = String.sub txt pos.pos pos.len in
              Warn.user_error
                "A provided condition must not come after an `undefined_unless' condition. Culprit: '%s'." pp
            end;
          (* If this is a requires-clause, record the fact that
             we have now seen at least one requires-clause. *)
          let st = {st with seen_requires_clause =
                    (test_type = Requires) || st.seen_requires_clause;} in
          let skip_this_check =
            match name with
            | Some name -> StringSet.mem name O.skipchecks
            | None -> false in
          if
            O.strictskip || not skip_this_check
          then
            let v = eval_rel st.env e in
            let pred = match t with
            | Acyclic -> E.EventRel.is_acyclic
            | Irreflexive -> E.EventRel.is_irreflexive
            | TestEmpty -> E.EventRel.is_empty in
            let ok = pred v in
            let ok = MU.check_through ok in
            if ok then kont st res
            else if skip_this_check then begin
              assert O.strictskip ;
              kont
                { st with
                  skipped = StringSet.add (Misc.as_some name) st.skipped;}
                res
            end else begin
              if (O.debug && O.verbose > 0) then begin
                let pp = String.sub txt pos.pos pos.len in
                let cy = E.EventRel.get_cycle v in
                MU.pp_failure test conc
                  (sprintf "%s: Failure of '%s'" test.Test.name.Name.name pp)
                  (let k = show_to_vbpp st in
                  match cy with
                  | None -> k
                  | Some r -> ("CY",U.cycle_to_rel r)::k)
              end ;
              match test_type with
              | Provides -> res
              | Requires ->
                  let () = failed_requires_clause () in
                  kont st res
            end
          else begin
            W.warn "Skipping check %s" (Misc.as_some name) ;
            kont st res
          end
      | Let (_,bds) ->
          let env = eval_bds st.env bds in
          let st = { st with env; } in
          let st = doshow bds st in
          kont st res
      | Rec (loc,bds) ->
          let env =
            env_rec loc
              (fun pp -> pp@show_to_vbpp st)
              bds st.env in
          let st = { st with env; } in
          let st = doshow bds st in
          kont st res
      | Include (loc,fname) ->
          (* Run sub-model file *)
          let module P = ParseModel.Make(LexUtils.Default) in
          let itxt,(_,_,iprog) =
            try P.parse fname
            with Misc.Fatal msg | Misc.UserError msg ->
              error loc "%s" msg  in
          run itxt st iprog kont res
      | Procedure (_,name,args,body) ->
          let p =
            Proc { proc_args=args; proc_env=st.env; proc_body=body; } in
          kont { st with env = add_val name (lazy p) st.env } res
      | Call (loc,name,es) ->
          let env0 = st.env
          and show0 = st.show in
          let p = eval_proc loc env0 name in
          let env1 = add_args loc p.proc_args es env0 p.proc_env in
          run txt { st with env = env1; } p.proc_body
            (fun st_call res ->
              kont { st_call with env = env0; show=show0;} res)
            res
      | Enum (_loc,name,xs) ->
          let env = st.env in
          let tags =
            List.fold_left
              (fun env x -> StringMap.add x name env)
              env.tags xs in
          let enums = StringMap.add name xs env.enums in
          let env = { env with tags; enums; } in
          kont { st with env;} res
      | Foreach (_loc,x,e,body) ->
          let env0 = st.env in
          let v = eval env0 e in
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
                  let env = add_val x (lazy v) env0 in
                  run txt { st with env;} body
                    (fun st res ->
                      run_set { st with env=env0;} (ValSet.remove v vs) res)
                    res in
              run_set st set res
          | _ ->
              error (get_loc e) "foreach instruction applied to non-set value"
          end
      | ForOrder (loc,x,e1,e2,name) ->
          let env0 = st.env in
          let es = eval_set env0 e1
          and r = eval_rel env0 e2 in
          U.apply_orders es r
            (fun r ->
              let skip_this_check =
                match name with
                | Some name -> StringSet.mem name O.skipchecks
                | None -> false in
              if skip_this_check then
                let mk_o r =
                  E.EventRel.filter
                    (fun (e1,e2) ->
                      E.EventSet.mem e1 es &&  E.EventSet.mem e2 es)
                    (S.tr r) in
                let env = add_val x (lazy (Rel (mk_o r))) env0 in
                kont (doshowone x {st with env;}) res
              else begin
                if (O.debug && O.verbose > 0) then begin
                  eprintf "%a: cyclic order\n" TxtLoc.pp loc ; 
                  let cy = E.EventRel.get_cycle r in
                  MU.pp_failure test conc
                    (sprintf "%s: cyclic order" test.Test.name.Name.name)
                    (let k = show_to_vbpp st in
                    match cy with
                    | None -> k
                    | Some r -> ("CY",U.cycle_to_rel r)::k)
                end ;
                res
              end)
            (fun o res ->
              let env = add_val x (lazy (Rel o)) env0 in
              kont (doshowone x {st with env;}) res)
            res
      | Latex _ -> kont st res

      and run txt st c kont res = match c with
      | [] ->  kont st res
      | i::c ->
          exec txt st i
            (fun st res -> run txt st c kont res)
            res in

      let show =
        lazy begin
          let show =
            List.fold_left
              (fun show (tag,v) -> StringMap.add tag v show)
              StringMap.empty (Lazy.force vb_pp) in
          StringSet.fold
            (fun tag show -> StringMap.add tag (find_show_rel m tag) show)
            S.O.PC.doshow show
        end in
      run txt {env=m; show=show;
               seen_requires_clause=false;
               skipped=StringSet.empty;} prog kont res



              end
