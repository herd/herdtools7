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

module Make (O:Indent.S) (I:CompCondUtils.I) =
 struct
      open Printf
      open ConstrGen

      module S = Switch.Make(O)(I)
      module V = I.C.V

      (* Convert unsigned constants to match the type of scrutinized
         variable.
         This avoid untimely consequences of C 'integer promotion'. *)

      let cast_prop cast =
        ConstrGen.map_prop
        (function
         | LV (loc,v) -> LV (loc,cast loc v)
         | LL _|FF _ as a -> a)

      (* Simply print proposition, when optimized
         printing as a cascade of switch constructs
         has failed *)

      let dump_v v = I.dump_value v
      let dump_loc f loc = I.Loc.dump f (ConstrGen.Loc loc)

      let dump_vec f loc vs =
        let mk_elem_check i v =
          "(" ^ (I.Loc.dump f loc) ^
            "["^ string_of_int i ^ "] == "^ (dump_v None v) ^")"
        in
        String.concat " && " (List.mapi mk_elem_check vs)

      let dump p =
        let rec dump_prop p f = match p with
        | Atom (LV (loc,v)) ->
            begin match v with
            | Constant.ConcreteVector (_,vs) ->
                O.fprintf "(%s)" (dump_vec f loc vs)
            | _ ->
                O.fprintf "%s == %s"
                  (I.Loc.dump f loc)
                  (dump_v (Some (ConstrGen.loc_of_rloc loc)) v)
            end
        | Atom (LL (loc1,loc2)) ->
            O.fprintf"%s == %s" (dump_loc f loc1) (dump_loc f loc2)
        | Atom (FF ((_,_,None) as f1)) ->
            O.fprintf "%s" (I.Loc.dump_fatom (V.pp O.hexa) f1)
        | Atom (FF ((_,_,Some prop) as f1)) ->
            O.fprintf "%s && (" (I.Loc.dump_fatom (V.pp O.hexa) f1);
            assert(f = None);
            dump_prop prop (Some f1);
            O.fprintf ")"
        | Not p ->
            O.output "!(" ;
            dump_prop p f;
            O.output ")"
        | Or [] -> O.output "0"
        | Or [p] -> dump_prop p f
        | Or (p::ps) ->
            O.output "(" ;
            dump_prop p f ;
            O.output ") || (" ;
            dump_prop (Or ps) f ;
            O.output ")" ;
            ()
        | And [] -> O.output "1"
        | And [p] -> dump_prop p f
        | And (p::ps) ->
            O.output "(" ;
            dump_prop p f ;
            O.output ") && (" ;
            dump_prop (And ps) f ;
            O.output ")" ;
            ()
        | Implies (p1,p2) ->
            O.output "!(" ;
            dump_prop p1 f ;
            O.output ") || (" ;
            dump_prop p2 f ;
            O.output ")" ;
            () in
        dump_prop p None

      (* Conventional names *)
      let funname = "final_cond"
      let funname_ok = "final_ok"

      (* Check condition *)
      let dump_ok cond =
        O.f "inline static int %s(int cond) {"  funname_ok ;
        O.fi
          "return %scond;"
          (let open ConstrGen in
          match cond with
          | ExistsState _|NotExistsState _ -> ""
          | ForallStates _ -> "!") ;
        O.o "}" ;
        O.o "" ;
        ()

      (* Check proposition, standard case:
          locations accessible as arguments. *)

      let fundef_prop fname cast find_type p =
        let p = cast_prop cast p in
        let rlocs = I.C.rlocations_prop p in
        let plocs =
          I.C.RLocSet.map_list
            (fun rloc ->
              let t,is_ptr = find_type rloc in
              sprintf "%s %s" t (I.Loc.dump None rloc),is_ptr)
            rlocs in
        let plocs,is_ptr = List.split plocs in
        let is_ptr = List.exists (fun b -> b) is_ptr in
        let vals = I.C.location_values_prop p in
        let pvals =
          List.map
            (fun loc -> sprintf
                "void *%s" (dump_v None (Constant.mk_sym loc)))
            vals in
        let is_ptr = is_ptr || Misc.consp pvals in
        let formals =
          let p = plocs@pvals in
          match p with
          | [] -> "void"
          | _::_ -> String.concat "," p in
        O.f "inline static int %s(%s) {" fname formals ;
        begin try
          if is_ptr then raise Exit ;
          let switch_tree = S.compile p in
          S.dump Indent.indent switch_tree
        with Switch.Cannot|Exit ->
          O.fprintf "%sreturn " (Indent.as_string Indent.indent) ;
          dump p ;
          O.output ";\n"
        end ;
        O.o "}" ;
        O.o "" ;
        ()

      let fundef cast find_type cond =
        fundef_prop funname cast find_type (ConstrGen.prop_of cond) ;
        if I.with_ok then dump_ok cond ;
        ()

      (* Check proposition, presi case:
         all locations acccessible from a 'log_t' struct *)

      let fundef_onlog_prop fname cast p =
        let p = cast_prop cast p in
        O.f "inline static int %s(log_t *p) {" fname ;
        begin try
          let switch_tree = S.compile p in
          S.dump Indent.indent switch_tree
        with Switch.Cannot ->
          O.fprintf "%sreturn " (Indent.as_string Indent.indent) ;
          dump p ;
          O.output ";\n"
        end ;
        O.o "}" ;
        O.o "" ;
        ()

      let fundef_onlog cast cond =
        fundef_onlog_prop funname cast (ConstrGen.prop_of cond) ;
        dump_ok cond ;
        ()

      (* Call check functions *)

      let funcall_prop fname  prop dump_loc dump_val =
        let rlocs = I.C.rlocations_prop prop in
        let plocs = I.C.RLocSet.map_list dump_loc rlocs in
        let vals = I.C.location_values_prop prop in
        let pvals = List.map dump_val vals in
        sprintf "%s(%s)" fname (String.concat "," (plocs@pvals))

      let funcall cond dump_loc dump_val =
        funcall_prop funname  (ConstrGen.prop_of cond) dump_loc dump_val

    end
