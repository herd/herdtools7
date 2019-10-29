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

(** Producing .dot output *)

open Test_herd
open Printf

module type S = sig

  module S : SemExtra.S
  open S

  val init_pretty : unit -> unit

  val pp_no_solutions :  out_channel -> S.test -> string -> unit

(* Standard dump (to dot file)
   just_dump chan test es rfm rels vos.
   - chan is output channel
   - es is event_structure (includes intra_causality and atomicity)
   - legend is a legend.
   - rels is a list of (tags,relation) to be pictured
     tag will label the edges
 *)

  val dump_legend :
      out_channel ->S.test -> string ->
        S.concrete -> S.rel_pp -> unit

(* Simpler function, just to dump event structures with and without rfmaps *)
  val dump_es :
      out_channel -> test -> event_structure -> unit
  val dump_es_rfm :
      out_channel -> test -> event_structure -> rfmap -> unit

(************************************)
(* Show feature: pop up a gv window *)
(************************************)

  val show_es_rfm : test -> event_structure -> rfmap -> unit

  val show_legend : S.test -> string -> S.concrete -> S.rel_pp -> unit

end

module Make (S:SemExtra.S) : S with module S = S  = struct

  module S = S
  module A = S.A
  module E = S.E
  module PC = S.O.PC

  let dbg = false
(* One init *)

  let one_init = match PC.graph with
  | Graph.Columns -> PC.oneinit
  | Graph.Free|Graph.Cluster -> false

(* Attempt *)

  let _reduces_more r1 r2 =
    let open E.EventRel in
    filter
      (fun (e1,_e2) ->
        not
          (E.EventSet.is_empty (succs r2 e1)) &&
        not
          (E.EventSet.is_empty (preds r2 e1)))
      r1


  open PrettyConf
  open PPMode


  let show_all_events = match PC.showevents with
  | AllEvents -> true
  | MemEvents|NonRegEvents|MemFenceEvents -> false


(* Printing the program with the nice_prog field *)

(* Please avoid insterting references to Global in,
   for instance X86Base.ml, since this
   source file is shared with litmus.new & gen.new,
   It is nicer if TeX processing belongs to this module, Pretty *)

(* partial escaping of strings into legal tex command identifiers *)


  let escape_tex s =
    let buff = Buffer.create 16 in
    for k=0 to String.length s-1 do
      let c = s.[k] in
      begin match c with
      | '_' -> Buffer.add_char buff '\\'
      | _ -> ()
      end ;
      Buffer.add_char buff c
    done ;
    Buffer.contents buff

  let escape_dot s =
    let buff = Buffer.create 16 in
    for k=0 to String.length s-1 do
      let c = s.[k] in
      begin match c with
      | '\\' -> Buffer.add_char buff '\\'
      | _ -> ()
      end ;
      Buffer.add_char buff c
    done ;
    Buffer.contents buff

  let escape_html s =
    let buff = Buffer.create 16 in
    for k=0 to String.length s-1 do
      let c = s.[k] in
      begin match c with
      | '<' -> Buffer.add_string buff "&lt;"
      | '>' -> Buffer.add_string buff "&gt;"
      | '&' -> Buffer.add_string buff "&amper;"
      | _ -> Buffer.add_char buff c
      end
    done ;
    Buffer.contents buff

  let escape_label dm = match dm with
  | Plain -> escape_dot
  | Fig -> (fun s -> escape_dot (escape_tex s))


  let a_pp_instruction dm m i =
    let bodytext = A.pp_instruction m i in
    let pp =
      if PC.texmacros
      then "\\asm{" ^ bodytext ^ "}"
      else bodytext in
    escape_label dm pp

      (* Pretty printing and display *)


  let pp_color chan color =
    if not PC.mono then
      fprintf chan "[color=%s]" color


(* dot pretty printing *)

  let pp_no_solutions chan test msg =
    let legend =
      escape_dot(readable_name test) ^ ": " ^ test.Test_herd.name.Name.doc in

    fprintf chan "digraph G {\n\n" ;
    fprintf chan "/* legend */\n" ;
    fprintf chan "label=\"%s\" ; \n\n" legend  ;
    fprintf chan "no_solns [shape=box] %a" pp_color "red" ;
    fprintf chan "[label=\" %s\\l\"]\n}\n" msg

  let pp_instruction dm m chan iiid = match iiid with
  | None -> fprintf chan "Init"
  | Some iiid ->
      let instruction = iiid.A.inst in
      fprintf chan "%s" (a_pp_instruction dm m instruction)

  let extra_thick = "setlinewidth(3)"

(* Scales that depend upon font size *)
  let pt2inch f = f /. 72.0

  let defaultpenwidth = 1.0
  let scalepenwidth = match PC.penwidth with
  | Some f -> fun x -> ((x *. f) /. defaultpenwidth)
  | None -> fun x -> x


  let defaultfontsize = 14.0
  let dsize = defaultfontsize /. 72.0
  let fscale = match PC.fontsize with
  | None -> 1.0
  | Some f -> float_of_int f /. defaultfontsize

(*
  let condensednode =
  sprintf "[fixedsize=\"true\"][width=\"%f\"][height=\"%f\"]"
  (fscale *.0.4) (fscale *. 0.1)
 *)

  let pp_po_edge = PC.showpo

  let do_pp_fontsize sep chan = match PC.fontsize with
  | None -> ()
  | Some f -> fprintf chan "%c fontsize=%i" sep f

  let pp_fontsize = do_pp_fontsize ','

  let do_pp_fontsize_edge sep chan = match PC.fontsize with
  | None -> ()
  | Some f -> fprintf chan "%c fontsize=%i" sep (f+PC.edgedelta)

  let pp_fontsize_edge = do_pp_fontsize_edge ','

  let pp_penwidth chan = match PC.penwidth with
  | Some f -> fprintf chan ", penwidth=\"%f\"" f
  | None -> ()

  let pp_arrowsize chan = match PC.arrowsize with
  | Some f -> fprintf chan ", arrowsize=\"%f\"" f
  | None -> ()

(* Edges attributes as a function of label, or
   of prefix label.... *)
  type edge_attr =
      {
       color:string ;
       style:string ;
     }

  let ea_t = Hashtbl.create 17
  let def_ea = {color="black" ; style="" }

  let add_ea key a1 a2 = Hashtbl.add ea_t key (a1,a2)
  let add_eas lbls a1 a2 =
    List.iter (fun lbl -> add_ea lbl a1 a2) lbls

  let relabel_t =  Hashtbl.create 17

  let merge_labels2 lbl lbl2 nlbl =
    Hashtbl.add relabel_t lbl nlbl ;
    Hashtbl.add relabel_t lbl2 nlbl

  let merge_labels lbl lbl2 =
    let nlbl = sprintf "%s/%s" lbl lbl2 in
    merge_labels2 lbl lbl2 nlbl

  let relabel lbl =
    try Hashtbl.find relabel_t lbl
    with Not_found -> lbl


  let init_pretty () =
(* Relabeling *)
    if PC.relabel then begin
      merge_labels "dmb" "sync" ;
      merge_labels "isb" "isync"  ;
      merge_labels2 "ctrlisb" "ctrlisync"  "ctrlisb/isync";
      ()
    end ;
(* Colors *)
    add_ea "rf"
      {color="red" ; style = "" ; }
      {color="black" ; style="" ; } ;
    add_eas ["vo:" ; "seq"; ]
      {def_ea with color="magenta"; }
      {def_ea with color="grey30"; } ;
    add_eas ["po"; "po:"]
      def_ea {color="black" ; style="" ; } ;
    add_eas
      ["grf" ; "ppo"; "A/B:" ; "ppo-ext:";"ppoext";
       "membar:"; "poloc"; "w*w*"; "ppo-direct"]
      { color="brown" ; style="" ; }
      { color="black" ;  style="" ; } ;
    add_eas
      ["ppo-sela";]
      { color="#00b000" ; style="" ; }
      { color="black" ;  style="" ; } ;
    add_eas
      ["fr" ]
      {color="#ffa040"; style=""; }
      {color="black" ; style =""; };
    add_eas
      [ "lwfence"; "lwf"; "ffence"; "ff";
        "implied"; "mfence"; "dmb"; "lwsync"; "eieio" ; "sync" ; "dmb-cumul" ; "dsb";
        "dmb.st"; "dsb.st" ;
        "dmb.ld"; "dsb.ld" ;
        "dmb.sy"; "dsb.sy";
        "dmbst"; "dsbst" ;
        "dsb-cumul"; "sync-cumul"; "lwsync-cumul";
        "sync_cumul" ; "lwsync_cumul" ;
        "syncext";"lwsyncext";"dmbext";"dsbext";]
      {color="darkgreen"; style="";  }
      {color="black"; style="" ;  } ;
    add_eas ["A" ; "B"; "co"; "ws";]
      {color="blue" ; style = "" ; }
      {color="black" ; style = "" ; } ;
    add_eas ["LL" ; "LS" ; "PML"; "ll" ; "ls"; "pml"; "isync"; "dep"; "ctrl"; "data"; "addr"; "ctrlisync"; "isync"; "isb"; "ctrlisb";"ppo";"success";]
      { color="indigo" ; style = "" ; }
      { color="black" ; style = "" ; } ;
    add_ea "iico"
      { def_ea with color="black" ; }
      { def_ea with color="black" ; } ;
    add_ea "iico_data"
      { def_ea with color="black" ; }
      { def_ea with color="black" ; } ;
    add_ea "iico_control"
      { def_ea with color="grey" ; }
      { def_ea with color="grey" ; } ;
    add_ea "iico_ctrl"
      { def_ea with color="grey" ; }
      { def_ea with color="grey" ; } ;
    add_eas ["After";]
      {color="orange" ; style = extra_thick ; }
      {color="black" ; style= extra_thick ; } ;
    add_eas ["Before";]
      {color="blue" ; style = extra_thick ; }
      {color="black" ; style= extra_thick ; } ;
    add_eas ["CY";]
      {color="black" ; style = extra_thick ; }
      {color="black" ; style= extra_thick ; } ;
    ()

  let () = init_pretty ()

  (* Get complexified *)
  let get_ea def_color lbl =
    let col,mono =
      let key =
        try let idx = String.index lbl ':' in
        String.sub lbl 0 (idx+1)
        with Not_found -> lbl in
      try Hashtbl.find ea_t key
      with Not_found -> def_color def_ea ,def_ea in
    if PC.mono then mono else col



  let pp_attr chan attr value = match value  with
  | "" -> ()
  | _  -> fprintf chan ", %s=\"%s\"" attr value

  and pp_extra chan attr_value = match attr_value  with
  | "" -> ()
  | _  -> fprintf chan ", %s" attr_value

        (*******************)
        (* Sort out events *)
        (*******************)

        (* Collect events (1) by proc, (2) by poi.
           As a list of list of events.
           The second "list of" follows  po order *)

  module PU = PrettyUtils.Make(S)

  let rec order_one_proc = function
    | []|[_] -> []
    | i1::(i2::_ as rem) ->
        E.EventRel.cartesian i1 i2::order_one_proc rem

  let shift_max =
    let r = ref 0.0 in
    for k = 0 to Array.length PC.shift-1 do
      let x = PC.shift.(k) in
      if !r < x then r := x
    done ;
    !r

  let get_shift p =
    if p >= Array.length PC.shift then 0.0
    else  PC.shift.(p)

  let make_posy y env (p,es) =
    let s = get_shift p in
    let rec do_make_posy y env = function
      | [] -> env
      | e::es -> do_make_posy (y -. 1.0) (E.EventMap.add e y env) es in
    do_make_posy (y -. s) env es


  let order_events es by_proc_and_poi =
    let iico =
      S.union es.E.intra_causality_data es.E.intra_causality_control in
    let iicos = E.proj_rel es iico in
    let rs =
      List.map2
        (fun ess iico ->
          let r = S.unions (iico::order_one_proc ess)
          and es = E.EventSet.unions ess in
          E.EventRel.topo es r)
        by_proc_and_poi iicos in
    let max =
      List.fold_left
        (fun n es -> max n (List.length es)) 0 rs in
    let max = max-1 in
    let max = float_of_int max in
    let max = max +. shift_max in
    let rs = List.mapi (fun k es -> k,es) rs in
    let env = List.fold_left (make_posy max) E.EventMap.empty rs in
    max,env,E.EventMap.empty

  let debug_event_set chan s =
    output_char chan '{' ;
    E.EventSet.pp chan ","  (fun chan e -> fprintf chan "%s" (E.pp_eiid e)) s;
    output_char chan '}'


  let pp_by_proc_and_poi chan s =
    List.iteri
      (fun i ess ->
        fprintf chan "P%i:" i ;
        List.iter (fun es -> fprintf stderr " %a" debug_event_set es) ess ;
        eprintf "\n")
      s ;
    flush stderr

  let make_posy_mult y env (p,ess) =
    let s = get_shift p in
    let rec do_make_posy y env = function
      | [] -> env
      | es::ess ->
          let env =
            E.EventSet.fold
              (fun e env -> E.EventMap.add e y env)
               es env in
          do_make_posy (y -. 1.0) env ess in
    do_make_posy (y -. s) env ess

  let dsiy = PC.dsiy
  let siwidth = PC.siwidth

  let order_events_mult es by_proc_and_poi =
    let iico =
      S.union es.E.intra_causality_data es.E.intra_causality_control in
    let  by_proc_and_poi =
      List.map
        (fun ess ->
          List.fold_right
            (fun es k -> E.EventRel.strata es iico @ k)
            ess [])
         by_proc_and_poi in
    if dbg then
      pp_by_proc_and_poi stderr by_proc_and_poi ;
    let max =
      List.fold_left
        (fun n ess -> max n (List.length ess)) 0 by_proc_and_poi in
    let max = max-1 in
    let max = float_of_int max in
    let max = max +. shift_max in
    if dbg then eprintf "max=%.02f\n%!" max ;
    let ps = List.mapi (fun k es -> k,es) by_proc_and_poi in
    let envy = List.fold_left (make_posy_mult max) E.EventMap.empty ps in
    let envx,envy =
      List.fold_left
        (fun envp (k,ess) ->
          let kf = float_of_int k in
          List.fold_left
            (fun (envx,envy as envp) es ->
              let n = E.EventSet.cardinal es in
              match n with
              | 1 ->
                  E.EventSet.fold
                    (fun e env -> E.EventMap.add e kf env)
                    es envx,envy
              | n ->
                  assert (n > 1) ;
                  let nf = float_of_int n in
                  let delta = siwidth /. (nf -. 1.0) in
                  let _,envp =
                    E.EventSet.fold
                      (fun e ((dx,dy),(envx,envy)) ->
                        let envx = E.EventMap.add e dx envx
                        and envy =
                          try
                            let old = E.EventMap.find e envy in
                            E.EventMap.add e (old +. dy) envy
                          with Not_found -> envy in
                      (dx +. delta,0.0 -. dy),(envx,envy))
                      es ((kf -.0.5 +. (1.0 -. siwidth) *. 0.5,if k mod 2 = 0 then dsiy else -. dsiy),envp) in
                  envp)
            envp ess)
        (E.EventMap.empty,envy) ps in
    max,envy,envx


(*******************************)
(* Build "visible" po relation *)
(*******************************)

(*
  Only successor edges are shown,
  and only from maximal intra-causality to minimal intra-causality.
 *)

  let rec min_max_to_succ = function
    | []|[_] -> E.EventRel.empty
    | (_xmin,xmax)::((ymin,_ymax)::_ as rem) ->
        E.EventRel.union
          (E.EventRel.cartesian xmax ymin)
          (min_max_to_succ rem)

  let make_visible_po es by_proc_and_poi =
    let intra =
      E.EventRel.transitive_closure
        (E.EventRel.union
           es.E.intra_causality_data
           es.E.intra_causality_control) in
    let min_max_list =
      List.map
        (List.map
           (fun es ->
             let mins =
               E.EventSet.filter
                 (fun e -> not (E.EventRel.exists_pred intra e))
                 es
             and maxs =
               E.EventSet.filter
                 (fun e -> not (E.EventRel.exists_succ intra e))
                 es in
             mins,maxs))
        by_proc_and_poi in
    E.EventRel.unions
      (List.map  min_max_to_succ min_max_list)

  let dm = PC.dotmode
  let m = match dm with | Plain -> Ascii| Fig -> DotFig
  let mmode = m

  let pp_edge_label movelabel lbl =
    let lbl =
      if PC.relabel then relabel lbl
      else lbl in
    let lbl =
      if PC.movelabel && movelabel then
        let sz = String.length lbl in
        if PC.texmacros then
          sprintf "%s" lbl
        else
          lbl ^ String.make sz ' '
      else lbl in
    let lbl =
      if PC.tikz then
        sprintf "{\\small %s}" lbl
      else lbl in
    escape_label dm lbl

  let pp_thread chan i =
    let pp =
      if PC.showthread then
        if PC.texmacros then
          sprintf "\\myth{%i}" i
        else
          sprintf "Thread %i" i
      else "" in
    fprintf chan "%s" (escape_label dm pp)

  module StringPair = struct
    type t = string * string

    let compare (e1,e2) (e3,e4) =
      match String.compare e1 e3 with
      | 0 ->  String.compare e2 e4
      | r -> r
  end

  module PairSet = MySet.Make(StringPair)
  module PairMap = MyMap.Make(StringPair)

  type info = { ikey:string; icolor:string; }

  let edges = ref PairMap.empty
  let edges_seen = ref StringMap.empty

  let reset_pairs () =
    edges := PairMap.empty ;
    edges_seen := StringMap.empty ;
    ()

  let find_pair p m =
    try PairMap.find p m with Not_found -> []


  let add_if_new p i m add =
    let old = find_pair p m in
    if
      List.exists
        (fun {ikey=k; _} -> k = i.ikey)
        old
    then m
    else
      PairMap.add p (add i old) m

  let do_add_pair p i m =
    add_if_new p i m (fun i old -> i::old)


  let add_end p i m =
    add_if_new p i m (fun i old -> old@[i])

  let handle_symetric m =
    let yes,no =
      PairMap.fold
        (fun (n1,n2 as p) infos (m_yes,m_no) ->
          let yes,no =
            List.partition
              (fun i -> StringSet.mem i.ikey PC.symetric)
              infos in
          let m_yes =
            let q =
              if String.compare n2 n1 < 0 then (n2,n1) else p in
            List.fold_left
              (fun m_yes i -> ((q,i)::m_yes))
              m_yes yes
          and m_no =
            match no with
            | [] -> m_no
            | _ -> PairMap.add p no m_no in
          m_yes,m_no)
        m ([],PairMap.empty) in
    let new_m,rem =
      List.fold_left
        (fun (new_m,rem) ((n1,n2 as p),i) ->
          let q = n2,n1 in
          let no_p = find_pair p no
          and no_q = find_pair q no in
          match no_p,no_q with
          | [],[] -> new_m,(p,i)::rem
          | [],_ -> add_end q i new_m,rem
          | _,[] -> add_end p i new_m,rem
          | _,_  -> add_end p i (add_end q i new_m),rem)
        (no,[]) yes in
    List.fold_left
      (fun m (p,i) -> do_add_pair p i m) new_m rem

  let compute_colors cs = (* NB keep order *)
    let rec do_rec = function
      | [] -> []
      | c::cs ->
          if List.mem c cs then do_rec cs
          else c::do_rec cs in
    String.concat ":" (do_rec cs)

  let fmt_merged_label fst i =
    let pp_label = escape_html i.ikey in
    let pp_label = pp_edge_label false pp_label in
    sprintf "<font color=\"%s\">%s%s</font>" i.icolor
      (if fst then "" else "") pp_label

  let fmt_merged_labels infos = match infos with
  | [] -> []
  | i::rem ->
      fmt_merged_label true i::
      List.map (fmt_merged_label false) rem

  let dump_pairs chan =
    let new_edges = handle_symetric !edges in
    PairMap.iter
      (fun (n1,n2) infos ->
        let all_syms =
          List.for_all
            (fun i -> StringSet.mem i.ikey PC.symetric)
            infos in
        let colors = compute_colors (List.map (fun i -> i.icolor) infos)
        and lbl = String.concat "" (fmt_merged_labels infos) in
        fprintf chan "%s -> %s [label=<%s>, color=\"%s\""
          n1 n2 lbl colors ;
        pp_fontsize_edge chan ;
        pp_penwidth chan ;
        pp_arrowsize chan ;
        if all_syms then pp_attr chan "arrowhead" "none" ;
        fprintf chan "];\n" ;
        ())
      new_edges ;
    reset_pairs ()

  let add_pair p i = edges := do_add_pair p i !edges

  let do_merge_edge n1 n2 lbl def_color =
    let color =
      try
        DotEdgeAttr.find lbl "color" PC.edgeattrs
      with Not_found ->
        let {color;_} = get_ea def_color lbl in
        color in
    add_pair (n1,n2) {ikey=lbl; icolor=color; }

  let real_do_pp_edge
      chan n1 n2 lbl def_color override_style extra_attr backwards
      movelbl
      =

    let backwards = match PC.graph with
    | Graph.Cluster|Graph.Free -> false
    | Graph.Columns ->
        if lbl = "po" then false
        else backwards in

    let overridden a =
      try
        ignore (DotEdgeAttr.find lbl a PC.edgeattrs) ; true
      with Not_found -> false in

    let checklabel a =
      try begin match  DotEdgeAttr.find a "label" PC.edgeattrs with
      | "tail" -> "taillabel"
      | "head" -> "headlabel"
      | _ -> "label"
      end with
      | Not_found -> "label" in

    let {color=color ; style=style; } = get_ea def_color lbl in
    fprintf chan "%s -> %s [%s=\"%s\""
      (if backwards then n2 else n1)
      (if backwards then n1 else n2)
      (if not (overridden "label") && PC.movelabel && movelbl then "taillabel"
      else checklabel lbl)
      (pp_edge_label movelbl lbl) ;

    if StringSet.mem lbl PC.symetric then pp_attr chan "arrowhead" "none" ;
    if not (overridden "color") then begin
      pp_attr chan "color" color ;
      if not (PC.tikz) then
        pp_attr chan "fontcolor" color
    end ;
    if PC.tikz then
      pp_attr chan "lblstyle" "auto, midway, inner sep=0.7mm";
    if not PC.tikz && not (overridden "fontsize") then
      pp_fontsize_edge chan;
    if not (overridden "penwidth") then pp_penwidth chan ;
    if not (overridden "arrowsize") then pp_arrowsize chan ;
    if not (overridden "style") then
      pp_attr chan "style"
        (if override_style = "" then style  else override_style) ;
    pp_extra chan extra_attr ;
    if backwards then pp_attr chan "dir" "back" ;
    List.iter
      (fun (a,v) -> match a with
      | "color" ->
          pp_attr chan "color" v ;
          pp_attr chan "fontcolor" v
      | "label" -> ()
      | _ ->
          pp_attr chan a v)
      (DotEdgeAttr.find_all lbl PC.edgeattrs) ;
    fprintf chan "];\n" ;
    ()

  let get_edge_seen lbl = StringMap.safe_find PairSet.empty lbl !edges_seen

  let known_edge n1 n2 lbl =
    let seen = get_edge_seen lbl in
    PairSet.mem (n1,n2) seen ||  PairSet.mem (n2,n1) seen

  let record_edge_seen n1 n2 lbl =
    let seen = get_edge_seen lbl in
    edges_seen := StringMap.add lbl (PairSet.add (n1,n2) seen) !edges_seen

  let do_pp_edge
      chan n1 n2 lbl def_color override_style extra_attr backwards
      movelbl
      =
    try
      if StringSet.mem lbl PC.unshow then raise Exit ;
      let is_symetric = StringSet.mem lbl PC.symetric in
      if is_symetric then begin
        if known_edge n1 n2 lbl then raise Exit ;
        record_edge_seen n1 n2 lbl
      end ;
      if PC.edgemerge then
        do_merge_edge n1 n2 lbl def_color
      else
        real_do_pp_edge
          chan n1 n2 lbl def_color override_style extra_attr (backwards && not is_symetric)
          movelbl
    with Exit -> ()

  let pp_edge chan n1 n2 lbl backwards =
    do_pp_edge chan n1 n2 lbl (fun x -> x) "" "" backwards

  let pp_point chan n lbl pos =
    let {color=color;_} = get_ea (fun x -> x) lbl in
    let sz = (pt2inch (scalepenwidth PC.ptscale)) in
    fprintf chan "%s [shape=point, height=%.2f, width=%.2f" n sz sz ;
    pp_attr chan "color" color ;
    pp_extra chan pos ;
    fprintf chan "];\n"


  let pp_none chan n pos =
    fprintf chan "%s [shape=none, height=0.0, width=0.0, label=\"\"" n  ;
    pp_extra chan pos ;
    fprintf chan "];\n"


  let pp_node_eiid_label e = match dm with
  | Plain | Fig -> sprintf "%s: " (E.pp_eiid e)

  let pp_node_eiid e = sprintf "eiid%i" e.E.eiid

  let pp_node_ii chan ii = match ii with
  | None -> ()
  | Some ii ->
      fprintf chan "proc:%i poi:%i\\l"
        ii.A.proc
        ii.A.program_order_index

(*
  This complex function is not meant to be used directly,
  in case you wish, here is its type...

  val pp_dot_event_structure :
  out_channel ->
  test ->
  string option -> (* Legend *)
  event_structure ->
  rfmap ->
  rel_pp -> (* Relations *)
  event_set -> (* Nodes to be marked *)
  -> unit

 *)

  let do_pp_dot_event_structure chan _test legend es rfmap vbss mark =

    let vbss =
      List.filter (fun (tag,_) -> not (StringSet.mem tag PC.unshow)) vbss in
    let pl = fprintf chan "%s\n"
    and pf fmt = fprintf chan fmt in

(************************)
(* Position computation *)
(************************)
    let max_proc = Misc.last (E.procs_of es) in
    (* Collect events (1) by proc, then (2) by poi *)
    let events_by_proc_and_poi = PU.make_by_proc_and_poi es in
    let maxy,envy,envx =
      let mult = true in
      if mult then order_events_mult es events_by_proc_and_poi
      else order_events es events_by_proc_and_poi in
    let inits = E.mem_stores_init_of es.E.events in
    let n_inits = E.EventSet.cardinal inits in
    let init_envx =
      if one_init then
        let w1 = float_of_int max_proc in
        let x =
          match PC.initpos with
          | Some (x,_) -> x
          | None ->
              match max_proc with
              | 1 -> -0.3333
              | _ -> (w1 /. 2.0) -. 0.5 in
        E.EventSet.fold
          (fun e env ->
            E.EventMap.add e x env)
          inits E.EventMap.empty
      else
        let delta = if max_proc+1 >= n_inits then 1.0 else  0.75 in
        let w1 = float_of_int (max_proc+1)
        and w2 = float_of_int n_inits *. delta in
        let xinit = Misc.proj_opt 0.0 (Misc.app_opt fst PC.initpos) in
(*        eprintf "w1=%f, w2=%f\n" w1 w2 ; *)
        let shift = (w1 -. w2) /. 2.0 +. xinit in
        let _,r = E.EventSet.fold
            (fun e (k,env) ->
              k+1,
              let x =  shift +. (float_of_int k) *. delta in
(*          eprintf "k=%i, x=%f\n" k x ; *)
              E.EventMap.add e x env)
            inits (0,E.EventMap.empty) in
        r in

    let pp_node_eiid =
      if one_init then
        fun e ->
          if E.EventSet.mem e inits then "eiidinit" else pp_node_eiid e
      else pp_node_eiid in
    let yinit = Misc.proj_opt 0.66667 (Misc.app_opt snd PC.initpos) in
    let maxy =
      if E.EventSet.is_empty inits then maxy
      else maxy +. yinit in

    let get_proc e = match E.proc_of e with
    | Some p -> p
    | None -> (-1) in

    let get_posx_int e = get_proc e in

    let get_posx e =
      if E.is_mem_store_init e then
        try E.EventMap.find e init_envx
        with Not_found -> assert false
      else try E.EventMap.find e envx
      with Not_found -> float_of_int (get_posx_int e) in

    let get_posy e =
      if E.is_mem_store_init e then maxy
      else
        try E.EventMap.find e envy
        with Not_found -> 10.0 in

    let is_even e1 e2 =
      let d =  abs (get_posx_int e1 - get_posx_int e2) in
      d >= 2 && (d mod 2) = 0 in

(* Hum...
   At least it seems that, -> right label below
   <- left label above
   Or...
 *)

    let is_up e1 e2 =
      let d =  abs (get_posx_int e1 - get_posx_int e2) in
      d >= 2 &&
      not (is_even e1 e2) &&
      get_posy e1 < get_posy e2 in

    let back = false in
    let is_back e1 e2 = back && get_posx_int e1 <  get_posx_int e2 in


    let xorigin=1.0 in

(* Size of one step, horizontal *)
    let xstep = 1.0 in
(*Was
  begin  match max_proc with
  | 3 -> if PC.condensed then 0.7 else 2.0
  | 2 -> if PC.condensed then 0.7 (*WAS 1.0*) else 2.5
  | _ -> if PC.condensed then 0.7 (*WAS 1.0*) else 3.0
  end
 *)
(* size of one step, vertical *)
    let ystep = 0.75 in
(* WAS
   begin match maxy with
   | 2 -> if PC.condensed then 0.35 else 2.0
   | 5 -> if PC.condensed then 0.35 else 1.0
   (* SS: HACK!! for ppo1/ppo3. We should take this as input maybe *)
   | _ -> if PC.condensed then 0.35 else 3.0
   end
 *)
    let xscale= PC.scale *. PC.xscale *. xstep in
    let yscale= PC.scale *. PC.yscale *. ystep in



    (*
      Pick out the vertical edges of the last thread.
      so that the edge label can be put on their rhs,
      to reduce the label overlaps
     *)

    let last_thread e e' =
      let p = get_proc e and p' = get_proc e' in
      p = p' && p = max_proc in


(* Position of events *)
    let xfinal f = xscale *. f  +. xorigin
    and yfinal f = yscale *. f in
    let xevent e = xfinal (get_posx e) in
    let yevent e = yfinal (get_posy e) in


    let pp_event_position = match PC.graph with
    | Graph.Columns ->
        fun chan e ->
          fprintf chan ", pos=\"%f,%f!\"" (xevent e) (yevent e)
    | Graph.Free|Graph.Cluster ->
        fun _chan _e -> () in

    let pp_init_rf_position = match  PC.graph with
    | Graph.Cluster|Graph.Free -> fun _e -> ""
    | Graph.Columns ->
        fun e ->
          let x = xevent e
          and y = yevent e in
          let dx,dy = PC.initdotpos in
          sprintf "pos=\"%f,%f!\""
            (x +. xscale *. dx)
            (y +. yscale *. dy) in

    let pp_final_rf_position = match  PC.graph with
    | Graph.Cluster|Graph.Free -> fun _e -> ""
    | Graph.Columns ->
        fun e ->
          let x = xevent e
          and y = yevent e in
          let dx,dy = PC.finaldotpos in
          sprintf "pos=\"%f,%f!\""
            (x +. xscale *. dx)
            (y +. yscale *. dy) in

    let pp_action e =
      let pp = E.pp_action e in
      let pp =
        if E.EventSet.mem e mark then
          sprintf "*%s*" pp
        else pp in
      pp in

    let boxwidth = xscale *. 0.65 *. PC.boxscale in
    let boxheight = yscale *. 0.25 in

    let pp_loc e = match E.location_of e with
    | None -> assert false
    | Some loc -> A.pp_location loc in

    let pp_event ?lbl isinit color chan e =
      let act = pp_action e in
      let act =
        if PC.verbose > 0 then begin
          if E.EventSet.mem e es.E.data_ports then
            act ^ " (data)"
          else if E.EventSet.mem e es.E.success_ports then
            act ^ " (success)"
          else act
        end else act in
      if not PC.squished then begin
        begin match lbl with
        | None ->
            fprintf chan "%s [label=\"%s%s%s\\l%a%a\""
              (pp_node_eiid e) (pp_node_eiid_label e)
              (escape_label dm act)
              (if E.EventSet.mem e es.E.data_ports then " (data)" else "")
              pp_node_ii e.E.iiid
              (pp_instruction dm m) e.E.iiid

        | Some _ ->
            fprintf chan "eiidinit [label=\"Init\""
        end ;
        pp_attr chan "shape" "box" ;
        pp_fontsize chan ;
        pp_attr chan "color" color ;
        fprintf chan "];\n"
      end else begin
        let act =
          match lbl with
          | None ->
              let eiid_lab =
                if PC.labelinit && isinit then
                  sprintf "i%s:" (pp_loc e)
                else
                  pp_node_eiid_label e in
              fprintf chan "%s [label=\"%s%s\""
                (pp_node_eiid e) (eiid_lab)
                (escape_label dm act) ;
              act
          | Some es ->
              let acts =
                E.EventSet.fold
                  (fun e k ->
                    let act = pp_action e in
                    if PC.labelinit then
                      let  loc = pp_loc e in
                      sprintf "i%s: %s" loc act::k
                    else
                      act::k)
                  es [] in
              let acts = String.concat ", " acts in
              fprintf chan "eiidinit [label=\"%s\""
                (escape_label dm acts) ;
              acts in
        pp_attr chan "shape" (if PC.verbose > 2 then "box" else "none") ;
        pp_fontsize chan ;
        if PC.verbose > 2 then pp_attr chan "color" color ;
        pp_event_position chan e ;
        pp_attr chan "fixedsize" (if PC.fixedsize then "true" else "false") ;
        pp_attr chan "height"
          (sprintf "%f"
             (if PC.fixedsize then boxheight
             else fscale *. dsize)) ;
        pp_attr chan "width"
          (sprintf "%f"
(* For neato to route splines... *)
             (if PC.fixedsize then boxwidth
             else
               (float_of_int (String.length act) +. PC.extrachars)
                 *. PC.boxscale *. fscale *. dsize)) ;
        fprintf chan "];\n"
      end in

    let pp_init_event color chan inits =
      let e = try E.EventSet.choose inits with Not_found -> assert false in
      pp_event ~lbl:inits false color chan e in

    let pp_event_structure chan vbss es =
(* Extract relation to represent as classes, if any *)
      let asclass = match PC.classes with
      | Some n ->
          begin try
            eprintf "classes=\"%s\"\n" n ;
            let equiv = List.assoc n vbss in
            let cls = E.EventRel.classes equiv in
            eprintf
              "%s\n"
              (E.EventRel.pp_str " "
                 (fun (e1,e2) ->
                   sprintf "%s-%s" (E.pp_eiid e1) (E.pp_eiid e2))
                 equiv) ;
            List.iter
              (fun es ->
                eprintf " {%s}" (E.EventSet.pp_str "," E.pp_eiid es))
              cls ;
            eprintf "\n" ;
            Some (n,cls)
          with Not_found -> None
          end
      | None -> None in

      let pl = fprintf chan "%s\n" in

(* Init events, if any *)
      if not (E.EventSet.is_empty inits) then begin
        pl "" ;
        pl "/* init events */" ;
        if one_init then
          pp_init_event "blue" chan inits
        else
          E.EventSet.iter
            (fun ew -> pp_event true "blue" chan ew)
            inits
      end ;
      pl "" ;
      pl "/* the unlocked events */" ;
      Misc.iteri
        (fun n evtss ->
(* Prelude *)
          begin match PC.graph with
          | Graph.Cluster ->
              fprintf chan "subgraph cluster_proc%i" n ;
              fprintf chan
                " { rank=sink; label = \"%a\"%a; %sshape=box;\n"
                pp_thread n
                (fun chan () -> do_pp_fontsize ';' chan)  ()
                (if not PC.mono then "color=magenta; " else "")
          | Graph.Columns ->
              if PC.showthread then begin
                let pos =
                  sprintf "%f,%f"
                    (xfinal (float_of_int n))
                    (yfinal (maxy +. PC.threadposy)) in
                fprintf chan
                  "proc%i_label_node [shape=%s%a, label=\"%a\", pos=\"%s!\", fixedsize=true, width=%f, height=%f]\n"
                  n (if PC.verbose > 2 then "box" else "none")
                  (fun chan () -> pp_fontsize chan) ()
                  pp_thread n pos boxwidth boxheight
              end
          | Graph.Free -> ()
          end ;
(* Now output events *)
          let pp_events = match asclass with
          | None ->
              fun _m pp_evt evts ->
                E.EventSet.pp chan "" pp_evt evts
          | Some (name,cls) ->
              let color =
                lazy begin
                  try DotEdgeAttr.find name "color" PC.edgeattrs
                  with Not_found ->
                    let {color;_} =
                      get_ea (fun r -> { r with color="blue";}) name in
                    color end in
              fun m pp_evt evts ->
                let cls,evts =
                  List.fold_left
                    (fun (cls,k) cl ->
                      let cl = E.EventSet.inter cl evts in
                      if E.EventSet.is_empty cl then (cls,k)
                      else (cl::cls, E.EventSet.diff k cl))
                    ([],evts) cls in
                E.EventSet.pp chan "" pp_evt evts ;
                Misc.iteri
                  (fun j cl ->
                    let color = Lazy.force color in
                    fprintf chan "subgraph cluster_class_%02i_%02i_%02i" n m j ;
                    fprintf chan
                      " { %s label=\"%s\"; shape=box;\n"
                      (if not PC.mono then
                        sprintf "color=%s;" color
                      else "color=\"grey30\"; style=dashed; ")
                      "" ;
                    E.EventSet.pp chan "" pp_evt cl ;
                    fprintf chan "}\n")
                  cls in
          Misc.iteri
            (fun m evts -> (* evts = all events from one instruction.. *)
              if
                PC.withbox &&
                show_all_events
              then begin
                let pp_ins =
                  if PC.labelbox then
                    let e0 =
                      try E.EventSet.choose evts
                      with Not_found -> assert false in
                    let ins =
                      match e0.E.iiid with
                      | Some iiid -> iiid.A.inst
                      | None -> assert false in
                    a_pp_instruction dm mmode ins
                  else "" in
                fprintf chan "subgraph cluster_proc%i_poi%i" n m ;
                fprintf chan
                  " { %s label = \"%s\"; labelloc=\"b\"; shape=box;\n"
                  (if not PC.mono then
                    "color=green;"
                  else "color=\"grey30\"; style=dashed; ")
                  pp_ins ;
                (* assuming atomicity sets are always full instructions *)
                pp_events m (pp_event false "blue") evts ;
                fprintf chan "}\n"
              end else begin (* no green box around one event only *)
                pp_events m (pp_event false "blue") evts
              end)
            evtss;
(* Postlude *)
          begin match PC.graph with
          | Graph.Cluster ->  fprintf chan "}\n"
          | Graph.Free|Graph.Columns -> ()
          end)
        events_by_proc_and_poi;

      pl "" ;
      pl "/* the intra_causality_data edges */\n" ;
      E.EventRel.pp chan ""
        (fun chan (e,e') ->
          pp_edge chan (pp_node_eiid e) (pp_node_eiid e') "iico"
            false false)
        es.E.intra_causality_data ;

      pl "" ;
      pl "/* the intra_causality_control edges */" ;
      E.EventRel.pp chan ""
        (fun chan (e,e') ->
          pp_edge chan
            (pp_node_eiid e) (pp_node_eiid e')
            "iico_ctrl" false false)
        es.E.intra_causality_control ;


(****************)
(* new po edges *)
(****************)


      let make_rf_from_rfmap rfmap =
        S.RFMap.fold
          (fun wt rf k -> match wt,rf with
          | S.Load er,S.Store ew when E.is_mem er ->
              E.EventRel.add (ew,er) k
          | _ -> k)
          rfmap
          E.EventRel.empty in

      if pp_po_edge then begin
        let replaces_po =
          match PC.graph with
          | Graph.Columns|Graph.Cluster ->
              let all_vbss = E.EventRel.unions (List.map snd vbss) in
              let rf = make_rf_from_rfmap rfmap in
              let r = E.EventRel.union rf all_vbss in
              E.EventRel.union r (E.EventRel.inverse r)
          | Graph.Free -> E.EventRel.empty in


        let po_edges = make_visible_po es events_by_proc_and_poi in
        let po_edges =  E.EventRel.diff po_edges replaces_po in
(*        let po_edges = reduces_more  po_edges replaces_po in *)
        pl "" ;
        pl "/* the poi edges */" ;
        E.EventRel.pp chan ""
          (fun chan (e,e') ->
            let lbl = match PC.graph with
            | Graph.Free ->
                if PC.showthread then
                  sprintf "po:%i" (get_proc e)
                else "po"
            | Graph.Columns|Graph.Cluster ->
                "po" in
            pp_edge chan
              (pp_node_eiid e) (pp_node_eiid e') lbl
              (last_thread e e') (is_even e e'))
          po_edges
      end ;
      vbss in



    reset_pairs () ;
    pl "digraph G {" ;
    pl "" ;
    begin match PC.dotheader with
    | Some h -> pl h
    | None ->
(* Best trade-off for those two parameters, beware of changes... *)
        begin match PC.splines with
        | Some s -> pf "splines=%s;\n" (Splines.pp s)
        | None -> ()
        end ;
        begin match PC.overlap with
        | Some s -> pf "overlap=%s;\n" s
        | None -> ()
        end ;
(*    pl "compound=true;\n " ;   *)
        begin match PC.margin with
        | None -> ()
        | Some f ->
            pf "margin=\"%f\";\n" f
        end ;
        begin match PC.pad with
        | None -> ()
        | Some f ->
            pf "pad=\"%f\";\n" f
        end ;
        begin match PC.fontname with
        | Some s -> pf "fontname=\"%s\";\n" s
        | None -> ()
        end ;
        begin match PC.sep with
        | None-> ()
        | Some s ->
            pf "sep=\"%s\"\n" s
        end ;
(*    pl "bgcolor=\"transparent\";\n " ;   *)
    end ;
    pl "" ;
    begin match legend with
    | Some legend ->
        pl "/* legend */" ;
        begin match PC.fontsize with
        | None -> ()
        | Some f ->
            pf "fontsize=%i;\n" f
        end ;
        pf "label=\"%s\";\n\n" (escape_dot legend) ;
        ()
    | None -> ()
    end ;

    let vbss = pp_event_structure chan vbss es in

    pl "/* the rfmap edges */" ;
    let show_ref_rel = List.exists (fun (lab,_) -> lab = "rf") vbss in
    S.pp_rfmap chan ""
      (fun chan wt rf -> match wt,rf with
      | S.Load er,S.Store ew ->
          if not show_ref_rel then
            pp_edge chan
              (pp_node_eiid ew)
              (pp_node_eiid er)
              "rf"
              (last_thread ew er || is_up ew er || is_back ew er)
              (is_even ew er)
      | S.Final _,S.Store ew ->
          if PC.showfinalrf then
            let final_id = "final"^pp_node_eiid ew in
            pp_none chan final_id (pp_final_rf_position ew);
            pp_edge chan  (pp_node_eiid ew) final_id "rf" false false
          else ()
      | S.Load er,S.Init ->
          if PC.showinitrf then begin
            let init_id = "init"^pp_node_eiid er in
            pp_point chan init_id "rf" (pp_init_rf_position er);
            pp_edge chan init_id (pp_node_eiid er) "rf" false false
          end
      | S.Final _,S.Init -> ())
      rfmap ;
    pl "" ;
(* A bunch of arrows *)
    pl "" ;
    pl "/* The viewed-before edges */" ;
    if true then begin
      if dbg then begin
        let ns = List.map fst vbss in
        eprintf "Names: {%s}\n" (String.concat "," ns)
      end ;
      List.iter
        (fun (label,vbs) ->
          E.EventRel.pp chan ""
            (fun chan (e,e') ->
              do_pp_edge chan (pp_node_eiid e) (pp_node_eiid e') label
(* Overides default color... *)
                (fun s -> { s with color="brown" ; })
(* Overides any style given *)
                (if (try "mo" = String.sub label 0 2 with Invalid_argument _ -> false) && E.is_mem_store e && E.is_mem_store e' then "" (*"penwidth=10.0"*) else "")
(* Extra attributes, overrides nothing *)
                ""
                (last_thread e e' || is_up e e' || is_back e e')
                (is_even e e'))
            vbs)
        vbss
    end ;
    dump_pairs chan ;
    pl "}"

(*********************************************)
(* get rid of register events before dumping *)
(*********************************************)

  let select_non_init =
    if PC.showinitwrites then
      fun _ -> true
    else
      fun e -> not (E.is_mem_store_init e)

  let select_event = match PC.showevents with
  | AllEvents -> (fun _ -> true)
  | MemEvents -> E.is_mem
  | NonRegEvents -> (fun e -> not (E.is_reg_any e))
  | MemFenceEvents -> let open Misc in E.is_mem ||| E.is_barrier
  let select_event = let open Misc in select_event &&& select_non_init

  let select_events = E.EventSet.filter select_event
  let select_rel =
    E.EventRel.filter (fun (e1,e2) -> select_event e1 && select_event e2)

  let select_es es =
    { es with
      E.events = select_events
        es.E.events ;
      intra_causality_data = select_rel
        es.E.intra_causality_data;
      intra_causality_control = select_rel
        es.E.intra_causality_control; }

  let select_rfmap rfm =
    S.RFMap.fold
      (fun wt rf k ->  match wt,rf with
      | (S.Load e1,S.Store e2) ->
          begin match select_event e1, select_event e2 with
          | true,true -> S.RFMap.add wt rf k
          | true,false ->
              if E.is_mem_store_init e2 then
                S.RFMap.add wt S.Init k
              else k
          | _,_ -> k
          end
      | (S.Final _,S.Store e)
      | (S.Load e,S.Init)
        ->
          if select_event e then
            S.RFMap.add wt rf k
          else k
      | S.Final _,S.Init -> k)
      rfm S.RFMap.empty

  let pp_dot_event_structure chan
      test
      legend es rfmap vbss _conc =

    let obs =
      if PC.showobserved then
        PU.observed test es
      else
        E.EventSet.empty in
    do_pp_dot_event_structure chan
      test
      legend
      (select_es es)
      (select_rfmap rfmap)
      (List.map
         (fun (tag,rel) -> tag,select_rel rel)
         vbss)
      obs



  let dump_legend chan test legend conc vbs =
    pp_dot_event_structure
      chan test (if PC.showlegend then Some legend else None)
      conc.S.str conc.S.rfmap vbs S.conc_zero

  let dump_es_rfm_legend chan legend test es rfm =
    pp_dot_event_structure chan test legend es rfm [] S.conc_zero

  let dump_es chan test es =  dump_es_rfm_legend chan None test es S.RFMap.empty
  let dump_es_rfm chan =  dump_es_rfm_legend chan None

(* Showed versions of dump functions *)

  module SHOW = Show.Make(PC)

  let show_es_rfm test es rfm =
    SHOW.show (fun chan -> dump_es_rfm chan test es rfm)

  let show_legend test legend conc vbs  =
    SHOW.show
      (fun chan ->
        let legend = if PC.showlegend then Some legend else None in
        pp_dot_event_structure
          chan test legend conc.S.str conc.S.rfmap vbs conc)

end
