(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
 open Printf

exception Error of string

module type Config = sig
  val verbose : bool
  module Source : Arch.S
  module Target : Arch.S
  val conversions : (string * string) list
end

module Make(C:Config) = struct
  let debug = false

  module Source = C.Source
  module Target = C.Target

  module Env = struct
    type sub = {
        reg : (Source.reg * Target.reg) list;
        addr: (string * Target.reg) list;
        lab : (string * string) list
      }

    let pp_sub {reg; addr; _} =
      sprintf
        "{reg=<%s>; addr=<%s>;}"
        (String.concat " "
           (List.map
              (fun (sr,tr) -> sprintf "%s->%s"
                  (Source.pp_reg sr)
                  (Target.pp_reg tr))
              reg))
        (String.concat " "
           (List.map
              (fun (sr,tr) -> sprintf "%s->%s" sr (Target.pp_reg tr))
              addr))

    type t = sub * Target.reg list

    let init = {reg = []; lab = []; addr = []},Target.allowed_for_symb

    let get_register_from_reg (binds,free) reg =
      try (List.assoc reg binds.reg,(binds,free)) with
      | Not_found ->
          match free with
          | [] -> raise (Error "No fresh register available.")
          | r::fs ->
              if debug then
                eprintf "Allocating %s->%s\n"
                  (Source.pp_reg reg) (Target.pp_reg r) ;
              r,({binds with reg=(reg,r)::binds.reg},fs)

    let get_register_from_addr (binds,free) addr =
      try (List.assoc addr binds.addr,(binds,free)) with
      | Not_found ->
          match free with
          | [] -> raise (Error "No fresh register available.")
          | r::fs -> r,({binds with addr=(addr,r)::binds.addr},fs)

    let get_label =
      let fresh_label =
        let i = ref 0 in (fun () -> incr i;"label"^(string_of_int !i)) in
      fun (binds,free) l ->
        try (List.assoc l binds.lab,(binds,free)) with
        | Not_found -> let lbl = fresh_label () in
          lbl,({binds with lab=(l,lbl)::binds.lab},free)

    let get_lab_convs (binds,_) =
      List.map (fun (s,t) -> Target.Lab(s,t)) binds.lab

    let get_free_register (_,free) = free

  end

  let conversions =
    List.map
      (fun (s,t) ->
        let s =
          try Source.Parser.instr_from_string s with
          | e -> eprintf "Error while parsing instructions :\n\"%s\"\n" s;
              raise e in
        let t =
          try Target.Parser.instr_from_string t with
          | e -> eprintf "Error while parsing instructions :\n\"%s\"\n" t;
              raise e in
        (s,t)
      )
      C.conversions

  let rec dig subs pat instr = match pat,instr with
  | [],_ -> Some (instr,[])
  | _::_,[] -> None
  | p::ps,i::is ->
      match Source.match_instruction subs p i with
      | Some _ -> Some([],i::is)
      | None ->
          match dig subs (p::ps) is with
          | None -> None
          | Some(stash,rem) -> Some(i::stash,rem)

  let pp_pat_instr ps is =
    eprintf "[%s] vs. [%s]\n"
      (Source.debug_pats ps)
      (Source.debug_pseudos is)

  let pp_lab tag is =
    if false then
      let open Source in
      match is with
      | Label (_,_)::_ ->
          eprintf "*%s*\n" tag ;
          eprintf "%s" (dump_pseudos is) ;
          eprintf "------\n" ;
          ()
      | _ -> ()

  let rec find_pattern tag pat instrs subs =
    eprintf "%s: " tag ; pp_pat_instr pat instrs ;
    let open Source in
    pp_lab (tag ^ "FIND") instrs ;
    match pat,instrs with
    | pat,Nop::instrs
    | Nop::pat,instrs ->
        find_pattern tag pat instrs subs
    | pat,Label(l,Nop)::i::is ->
        find_pattern tag pat (Label(l,i)::is) subs
    | [],Label(_,Nop)::[] ->
        Some ([],instrs,subs)
    | pat,Label(_,Nop)::[] ->
        find_pattern tag pat [] subs

    | Symbolic s::pat,instrs ->
        begin
          match dig subs pat instrs with
          | None -> None
          | Some(stash,rem) ->
              find_pattern "SYMB" pat rem (Code(s,stash)::subs)
        end
    | p::ps,i::is ->
        begin
          match match_instruction subs p i with
          | None -> None
          | Some subs ->
              match find_pattern (sprintf "REC<%i>" (List.length ps))
                  ps is subs with
              | None -> None
              | Some(is,rs,subs) -> Some(i::is,rs,subs)
        end
    | [],rs ->
        Some([],rs,subs)

    | _,_ -> None

  let get_pattern_seq instrs =
    let rec aux instrs =
      pp_lab "AUX" instrs ;
      let rec find = function
        | [] ->
            begin
              match find_pattern "EMPTY" [] instrs [] with
              | Some(is,[],[]) -> Some((is,[],[]),[])
              | Some([],[Source.Label (lab,Source.Nop)],[]) ->
                  Some ((instrs,[Target.Label(lab,Target.Nop)],[]),[])
              | _ ->
                  if C.verbose then
                    eprintf "Unmatched instructions:\n%s"
                      (Source.dump_pseudos instrs);
                  None
            end
        | (p,conv)::ps ->
            match find_pattern "LOC" p instrs [] with
            | None -> find ps
            | Some(is,rs,subs) ->
                match is,conv with
                | Source.Label(l,_)::_ as is,(Target.Instruction(_) as c)::cs
                  -> Some((is,Target.Label(l,c)::cs,subs),rs)
                | _,_ -> Some((is,conv,subs),rs)
      in
      match find conversions with
      | None -> raise (Error "Cannot find conversion rule.")
      | Some(ins,[]) -> [ins]
      | Some(ins,rs) -> ins::(aux rs)
    in aux instrs

  let rec convert env instrs =
    let rec aux env l = match l with
    | [] -> [],env
    | (_src,tgt,subs)::ts ->
        let conv,env =
          List.fold_left
            (fun (cv,env) -> function
              | Source.Reg(s,c) ->
                  let r,env = Env.get_register_from_reg env c in
                  if debug then
                    eprintf "From %s->%s we get %s->%s\n"
                      s (Source.pp_reg c) s (Target.pp_reg r) ;
                  (Target.Reg(s,r)::cv,env)
              | Source.Addr(s,a) ->
                  let r,env = Env.get_register_from_addr env a in
                  (Target.Reg(s,r)::cv,env)
              | Source.Cst(s,c) -> (Target.Cst(s,c)::cv,env)
              | Source.Lab(s,l) ->
                  let lbl,env = Env.get_label env l in
                  (Target.Lab(s,lbl)::cv,env)
              | Source.Code(s,c) ->
                  let c,env = convert env c in
                  (Target.Code(s,c)::cv,env)
            )
            ([],env) subs in
        let flw,env = aux env ts
        in (tgt,(Env.get_lab_convs env)@conv)::flw,env
    in
    let chunks,env = aux env (get_pattern_seq instrs) in
    let chunks = List.map (fun (tgt,conv) ->
      Target.instanciate_with
        conv (Env.get_free_register env) tgt)
        chunks in
    let pseudo_p = List.flatten chunks in
    (pseudo_p,env)

  let reg_mapping =
    List.map (fun (i,(b,_)) ->
      (i,
       (List.map (fun (sr,tr) ->
         (Source.pp_reg sr,Target.pp_reg tr))
          b.Env.reg)))

  let addr_init =
    let open MiscParser in
    List.fold_left (fun acc (i,(b,_)) ->
      acc@
      (List.map (fun (sa,tr) ->
        (Location_reg(i,Target.pp_reg tr),
         (TyDef,ParsedConstant.nameToV sa)))
         b.Env.addr)
                   ) []

  let rec dump_map =
    let rec assocs i = function
      | [] -> ""
      | [sr,tr] -> (string_of_int i)^":"^tr^"="^sr
      | (sr,tr)::r -> (string_of_int i)^":"^tr^"="^sr^","^(assocs i r)
    in
    function
      | [] -> ""
      | [i,asc] -> assocs i asc
      | (i,asc)::r ->
          let s = assocs i asc in
          if String.compare s "" = 0
          then dump_map r
          else s^","^(dump_map r)

  let conv_loc map = MiscParser.(function
    | Location_reg(i,r) ->
        let r' =
          try
            let asc = List.assoc i map in
            List.assoc r asc
          with Not_found ->
            let msg = sprintf "register %i:%s does not appear in code." i r in
            raise (Error msg)
        in Location_reg(i,r')
    | l -> l)

  let translate name chin sres =
    let src = Source.Parser.parse chin sres in
    let open MiscParser in
    let prog = List.map (fun (i,p) ->
      let p,e = convert Env.init p in
      if debug then
        eprintf "Sub %s\n" (Env.pp_sub (fst e)) ;
      ((i,p),(i,e))) src.prog in
    let prog,convs = List.split prog in
    let map = reg_mapping convs in
    let init =
      addr_init convs @
      List.fold_right
        (fun (l,r) k ->
          try
            let loc = conv_loc map l in
            (loc,r)::k
          with Error msg ->
            Warn.warn_always "File \"%s\": %s" name msg ;
            k)
        src.init [] in
    let map_lv_ll =
      ConstrGen.(function
        | LV(l,v) -> LV(conv_loc map l,v)
        | LL(l1,l2) -> LL(conv_loc map l1,conv_loc map l2)) in
    let condition = ConstrGen.map_constr map_lv_ll src.condition
    and filter = Misc.app_opt (ConstrGen.map_prop map_lv_ll) src.filter in
    let locations =
      List.map (fun (loc,ty) -> conv_loc map loc,ty) src.locations in
    { info = (OutMapping.key,dump_map map)::src.info;
      init = init;
      prog = prog;
      filter = filter;
      condition = condition;
      locations = locations;
      extra_data = src.extra_data;
    }

end
