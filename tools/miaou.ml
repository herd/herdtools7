(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Miaou, this cat talks! *)

open Printf

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "miaou7"

module Make
    (O:sig
(* Configuration *)
         val verbose : int
         val includes : string list
         val libdir : string
         val expand : bool
         val flatten : bool
         val std : bool
(* Definitons *)
         val names : StringSet.t
         val testmode : bool
         val texfile : string option
    end) =
  struct

    (***********)
    (* Parsing *)
    (***********)

    let libfind =
      let module ML =
        MyLib.Make
          (struct
            let includes = O.includes
            let env = Some "HERDLIB"
            let libdir = O.libdir
            let debug = O.verbose > 0
          end) in
      ML.find

    module ParserConfig =
      struct
        let debug = false
        let libfind = libfind
      end

    module P = ParseModel.Make(ParserConfig)

    (*******************)
    (* Normalize names *)
    (*******************)

    open AST

    let tr_id = MiaouNames.to_csname

    let defs =
      match O.texfile with
      | None -> None
      | Some fname ->
         Some (LexMiaou.csnames (libfind fname))

    let get_id_type  =
      match defs with
      | None -> fun _ -> None
      | Some defs ->
         fun name ->
         try StringMap.find (tr_id name) defs
         with Not_found -> None

    let nodefs = ref StringSet.empty

    let get_nodefs () =
      let d = !nodefs in
      nodefs := StringSet.empty ;
      d

    let check_nodefs =
      match defs with
      | None -> fun _ name -> name
      | Some defs ->
         fun loc name ->
         if not (StringMap.mem name defs) then begin
           if O.verbose > 1 then
             eprintf "%a: found undefined %s\n"
               TxtLoc.pp loc name ;
           nodefs := StringSet.add name !nodefs
         end ;
         name

    let pp_id loc s =
      let name = tr_id s in
      check_nodefs loc name

    let id_name id = tr_id id |> sprintf "\\%sname"

    let pp_transitive = sprintf "transitive{%s}"

    let transitive id = id_name id |> pp_transitive

    (* Advance indentation and event numbers *)

    let next_indent indent = indent ^ "  "

    module Next : sig
      val reset : unit -> unit
      val next : unit -> int
    end = struct
      let c = ref 1
      let reset () = c := 1
      let next () = let r = !c in incr c ; r
    end

    let pp_evt k = sprintf "E\\textsubscript{%d}" k

    (* Pretty print elements for n-ary operators *)

    let intro  op =
      match op with
      | Union ->
         "one of the following applies"
      | Inter|Seq|Cartesian ->
         "all of the following apply"
      | _ -> assert false

    let sep op =
      match op with
      | Union -> ".","."
      | Inter|Seq|Cartesian -> ".","."
      | _ -> assert false

    (***********)
    (* Failure *)
    (***********)

    module Cache = TxtLoc.Extract()

    let pp_expr e = ASTUtils.exp2loc  e |> Cache.extract

    let fail loc msg =
      if O.verbose >= 0 then
        eprintf "%a: %s\n%!" TxtLoc.pp loc msg ;
      let txt = Cache.extract loc in
      sprintf "Missed: \\verb$%s$" txt

    (************************************************)
    (* Translation to nested itemize list structure *)
    (************************************************)


    let rec get_type = function
      | Konst (_,(Empty ty|Universe ty)) -> Some ty
      | Op (_,(Seq|Cartesian),_)
      | Op1 (_,(ToId|Star|Plus),_)
        -> Some RLN
      | If (_,_,e1,e2) ->
         get_type2 e1 e2
      | Op (_,(Union|Inter|Diff),es) ->
         get_types es
      | Var (_,id) ->
         get_id_type id
      | _ -> None

      and get_type2 e1 e2 =
        match get_type e1 with
        | None -> get_type e2
        | Some _ as r -> r

      and get_types  = function
        | [] -> None
        | e::es ->
           begin
             match get_type e with
             | None -> get_types es
             | Some _ as r -> r
           end

    type t =
      | Item of string
      | List of AST.op2 * string * (string * string) * t list
      | DiffPair of t * t
      | IfCond of string * t * t

    let mk_list op itms = List (op,intro op,sep op,itms)

    type atom = Pos of string | Neg of string

    let rec variant_dnf neg = function
      | Variant v ->
         [[if neg then Neg v else Pos v]]
      | OpNot vc ->
         variant_dnf (not neg) vc
      | OpOr (vc1,vc2) ->
         variant_dnf neg vc1 @ variant_dnf neg vc2
      | OpAnd (vc1,vc2) ->
         let d1 = variant_dnf neg vc1
         and d2  =variant_dnf neg vc2 in
         List.fold_right
           (fun a1 k ->
             List.fold_right
               (fun a2 k -> (a1@a2)::k) d2 k)
           d1 []

    let pp_dnf d =
      List.map
        (fun a ->
          List.map
            (function
             | Pos s -> sprintf "\\Variant{%s}" s
             | Neg s -> sprintf "\\NotVariant{%s}" s)
            a |> String.concat " and ")
        d |> String.concat "{} or "

    let pp_vc vc = variant_dnf false vc |> pp_dnf

    let do_pp_rel_id  e1 e2 id =
      sprintf "\\%s{%s}{%s}" id (pp_evt e1) (pp_evt e2)

    let makeuppercase =
      if O.std then
        sprintf "\\expandafter\\MakeUppercase%s"
      else
        sprintf "\\expandafter{\\MakeUppercase%s}"

    let pp_rel_id e1 e2 id =
      Item (makeuppercase  @@ do_pp_rel_id e1 e2 id)

    let tr_rel_id e1 e2 loc id = pp_rel_id  e1 e2 (pp_id loc id)

    let do_pp_evts_id e id = sprintf "\\%s{%s}" id (pp_evt e)

    let pp_evts_id e id = Item  (do_pp_evts_id e id)

    let tr_evts_id e loc id = pp_evts_id e (pp_id loc id)

    let tr_fail loc = Item (fail loc "ignoring expression")

    let id_expr = Var (TxtLoc.none,"id")
    let opt_expr loc e = Op (loc,Union,[e;id_expr;])
    let star_expr loc e = opt_expr loc (Op1 (loc,Plus,e))


    let do_tr_diff tr a b =
      let a = tr a
      and b = tr b in
      DiffPair (a,b)

    let tr_diff tr tr_not tr_id a b =
      match a,b with
      | Var (loc1,id1),Op (_,Seq,[c;Var (_,id2);])
           when String.equal id1 id2
        ->
        begin
          match tr_not c with
          | None -> do_tr_diff tr a b
          | Some c ->
             mk_list Inter [tr_id loc1 id1 ;c]
        end
      | _,_ ->
         begin
           match tr_not b with
           | None ->
              do_tr_diff tr a b
           | Some c ->
              mk_list Inter [tr a; c;]
         end

    let flatten_if_not =
      if O.flatten then Fun.id
      else ASTUtils.flatten

    let rec tr_rel e1 e2 = function
      | Konst (loc,Empty _) ->
         tr_rel_id e1 e2 loc "norel"
      | Konst (loc,Universe _) ->
         tr_rel_id e1 e2 loc "anyrel"
      | Op (_,Inter,[a;Op1 (_,Comp,b)])
      | Op (_,Inter,[Op1 (_,Comp,b);a])
        ->
         tr_rel_diff e1 e2 a b
      | Op (_,(Union|Inter as op),es) ->
         tr_op e1 e2 op es
      | Op (_,(Seq as op),es) ->
         List (op,intro op,sep op,tr_seq e1 e2 es)
      | Op (_,Diff,[a;b]) ->
         tr_rel_diff e1 e2 a b
      | Op (_,Cartesian,[a;b;]) ->
         let a = tr_evts_from_rel e1 a and b = tr_evts_from_rel e2 b in
         mk_list Cartesian [a;b;]
      | Var (loc,id) ->
         tr_rel_id e1 e2 loc id
      | If (_,VariantCond vc,Konst (_,Empty _),e) ->
         let op = Inter in
         mk_list op
           [Item (pp_vc (OpNot vc)); tr_rel e1 e2 e;]
      | If (_,VariantCond vc,e,Konst (_,Empty _)) ->
         let op = Inter in
         mk_list op
           [Item (pp_vc vc); tr_rel e1 e2 e;]
      | If (_,VariantCond vc,a,b) ->
         let c = pp_vc vc
         and a = tr_rel e1 e2 a
         and b =  tr_rel e1 e2 b in
         IfCond (c,a,b)
      | Op1 (loc,ToId,Konst (_,Universe _)) ->
         tr_rel_id e1 e2 loc "id"
      | Op1 (_,ToId,e) ->
         tr_evts_from_rel e1 e
      | Op1 (_,Inv,Op (loc2,Seq,es)) ->
         let es =
           List.rev_map
             (fun e -> Op1 (ASTUtils.exp2loc e,Inv,e))
             es in
         tr_rel e2 e1 (Op (loc2,Seq,es))
      | Op1 (_,Inv,e) ->
         tr_rel e2 e1 e
      | Op1 (loc,Comp,e) ->
         begin
           match tr_rel_not e1 e2 e with
           | None -> tr_fail loc
           | Some r -> r
         end
      | Op1 (loc,Plus,e) ->
         tr_plus e1 e2 loc e
      | Op1 (loc,Opt,e) ->
         tr_rel e1 e2 (opt_expr loc e)
      | Op1 (loc,Star,e) ->
         tr_rel e1 e2 (star_expr loc e)
      | App (_,Var (locf,("intervening-write" as f)),Var (loc,id)) ->
         let txt1 = do_pp_rel_id e1 e2 (pp_id locf f) in
         let txt2 = sprintf "{\\%s}" (pp_id loc id) in
         Item (txt1 ^ txt2)
      | e -> tr_fail (ASTUtils.exp2loc e)

    and tr_evts_from_rel e1 e2 = tr_evts e1 @@ flatten_if_not e2

    and tr_rel_not e1 e2 = function
      | Op1 (_,ToId,e) -> tr_evts_not e1 @@ flatten_if_not e
      | e -> notItem (tr_rel e1 e2 e)

    and tr_op e1 e2 op es = List.map (tr_rel e1 e2) es |> mk_list op

    and tr_plus e1 e2 loc = function
      | Var (_,id) ->
         pp_rel_id e1 e2 (id_name id |> pp_transitive)
      | Op (_,op,es) ->
         let tr_es pp_op =
           match ASTUtils.as_vars es with
           | None -> raise Exit
           | Some ids ->
              let itms =
                List.map
                  (fun (loc,id) -> tr_rel_id e1 e2  loc id)
                  ids
              and lst =
                let names =
                  List.map (fun (_,id) -> id_name id) ids
                  |> String.concat pp_op in
                pp_rel_id e1 e2 (pp_transitive names) in
              itms,lst in
         begin
           try
             match op with
             | Union ->
                let itms,lst = tr_es "{} or " in
                mk_list Union (itms@[lst])
             | Inter ->
                let itms,lst = tr_es "{} and " in
                mk_list Union [mk_list Inter itms;lst;]
             | _ -> tr_fail loc
           with Exit -> tr_fail loc
         end
      | _ -> tr_fail loc

    and tr_rel_diff e1 e2 a b =
      tr_diff
        (fun e -> tr_rel e1 e2 e)
        (fun e -> tr_rel_not e1 e2 e)
        (fun loc id -> tr_rel_id e1 e2 loc id)
        a b

    and tr_seq e1 e2 = function
      | [] -> []
      | Op1 (_,ToId,(Var (_,("DATA"|"ADDR")) as id))::es ->
         tr_rel e1 e2 id
         ::tr_seq e1 e2 es
      | Op1 (_,ToId,e)::es ->
         tr_evts_from_rel e1 e::tr_seq e1 e2 es
      | [e] ->
         [tr_rel e1 e2 e]
      | Var (loc1,id1)::Op1 (_,Opt,Var (loc2,id2))::es ->
         let e1,e3,e2 =
           match es with
           | [] -> e1,e2,e2
           | _::_ ->
              e1,Next.next (),e2 in
         Item
           (sprintf
            "\\%s{%s}{either  %s} or \\%s{an Effect which}{%s}"
            (pp_id loc1 id1) (pp_evt e1) (pp_evt e3)
            (pp_id loc2 id2) (pp_evt e3))
         ::tr_seq e3 e2 es
      | [e;Op1 (_,ToId,f);] ->
         [tr_rel e1 e2 e; tr_evts_from_rel e2 f;]
      | e::es ->
         let e3 = Next.next () in
         tr_rel e1 e3 e::tr_seq e3 e2 es

    and notItem = function
      | Item txt ->
         Some
           (Item
              (makeuppercase @@ sprintf "\\notthecase{%s}" txt))
      | List (op,intro_txt,sep_txt,es) ->
          Some
            (List
               (op,
                makeuppercase @@ sprintf "\\notthecase{%s}" intro_txt,
                sep_txt,es))
      | DiffPair _|IfCond _ ->
         None

    and tr_evts e1 = function
      | Konst (loc,Empty _) ->
         tr_evts_id e1 loc "noevent"
      | Konst (loc,Universe _) ->
         tr_evts_id e1 loc "anyevent"
      | Var (loc,id) ->
         tr_evts_id e1 loc id
      | Op1 (loc,Comp,e) | App (loc,Var (_,"exempt"),e) ->
         begin
           match tr_evts_not e1 e with
           | None -> tr_fail loc
           | Some r -> r
         end
      | Op (loc,Inter,es)
           when O.flatten && List.for_all ASTUtils.is_var es
        ->
         let id =
           List.map (function Var (_,id) -> id | _ -> assert false) es
           |> String.concat ""
           |> pp_id loc in
         Item (sprintf "\\%s{%s}" id (pp_evt e1))
      | Op (_,(Union|Inter as op),es) ->
          List (op,intro op,sep op,List.map (tr_evts e1) es)
      | Op (_,Diff,[a;b;]) ->
         tr_evts_diff e1 a b
      | If (_,VariantCond vc,Konst (_,Empty _),e) ->
         let op = Inter in
         mk_list op
           [Item (pp_vc (OpNot vc)); tr_evts e1 e;]
      | If (_,VariantCond vc,e,Konst (_,Empty _)) ->
         let op = Inter in
         mk_list op
           [Item (pp_vc vc); tr_evts e1 e;]
      | If (_,VariantCond vc,a,b) ->
         let c = pp_vc vc
         and a = tr_evts e1 a
         and b =  tr_evts e1 b in
         IfCond (c,a,b)
      | App
          (_,Var (_,"range"),
           Op (_,Seq,
               [Op1 (_,ToId,Var (_,"A"));
                Var (_,"amo");
                Op1 (_,ToId,Var (_,"L"));]))
        -> pp_evts_id e1 "rangeAamoL"
      | App (_,Var (_,"range"),Var (_,"lxsx")) ->
         pp_evts_id e1 "rangelxsx"
      |  App (_,
              Var (_,"range"),
              Op (_,
                  Seq,
                  [Op1 (_,ToId,e);Op1 (_,Inv,Var (_,"tr-ib"));]))
         ->
          begin
            match tr_evts 0 e with
            | Item txt0 ->
               let txt =
                 do_pp_evts_id e1 "rangetribminus"
                 ^ sprintf "{%s}" (pp_evt 0)
                 ^ sprintf "{%s}" txt0 in
               Item txt
            | _ ->
               Item (fail (ASTUtils.exp2loc e) "ignoring expression")
          end
      | e ->
         Item (fail (ASTUtils.exp2loc e) "ignoring expression")

    and tr_evts_not e1 e = notItem (tr_evts e1 e)

    and tr_evts_diff e1 a b =
      tr_diff
        (fun e -> tr_evts e1 e)
        (fun e -> tr_evts_not e1 e)
        (fun loc id -> tr_evts_id e1 loc id)
        a b

    (*********************************)
    (* Flatten associative operators *)
    (*********************************)

    let same_op op1 op2 = match op1,op2 with
      | (Inter|Seq),(Seq|Inter)
      | (Union,Union)
        -> true
      | _,_ -> false

    let rec flatten_out = function
      | List ((Inter|Union|Seq as op),intro_txt,sep_txt,ts)
        ->
         List (op,intro_txt,sep_txt,(flatten_op op ts))
      | List (op,txt,s,ts) ->
         List (op,txt,s,List.map flatten_out ts)
      | DiffPair (e1,e2) ->
         DiffPair (flatten_out e1,flatten_out e2)
      | IfCond (txt,e1,e2) ->
         IfCond (txt,flatten_out e1,flatten_out e2)
      | Item _ as t -> t

    and flatten_op op = function
      | [] -> []
      | e::es ->
         begin
           match flatten_out e with
           | List (op0,_,_,ts) when same_op op op0
             ->
              ts@flatten_op op es
           | t ->
              t::flatten_op op es
         end

    (*********************************)
    (* Pretty print nested structure *)
    (*********************************)

    let pp_diff pp pref s t1 t2 =
      pp pref s
        (List
           (Diff,"The following",(","," and not"),[t1;t2;]))

    let rec pp_def pref s = function
      | Item txt ->
         printf "%s %s%s\n" pref txt s
      | List (_,txt,(s1,s2),ts) ->
         printf "%s %s:\n" pref txt ;
         printf "\\begin{itemize}\n" ;
         pp_txts "" s1 s2 s ts ;
         printf "\\end{itemize}\n"
      | DiffPair (Item txt1,Item txt2) ->
         printf "%s %s except when %s%s\n" pref txt1 txt2 s
      | DiffPair (t1,t2) ->
         pp_def pref "" t1 ;
         pp_def "except when" s t2
      | IfCond (txt,a,b) ->
         let pref = sprintf "%s when %s" pref txt in
         pp_def pref "" a ;
         pp_def "otherwise" s b

    and pp_txt indent s = function
      | Item txt
      | List (_,_,_,[Item txt]) ->
         printf "%s\\item %s%s\n" indent txt s
      | List (_,txt,(s1,s2),txts) ->
         printf "%s\\item %s:\n" indent (Misc.capitalize txt) ;
         let indent = next_indent indent in
         printf "%s\\begin{itemize}\n" indent ;
         pp_txts indent s1 s2 s txts ;
         printf "%s\\end{itemize}\n" indent
      | DiffPair (Item txt1,Item txt2) ->
         printf "%s\\item %s except when %s%s\n" indent txt1 txt2 s
      | DiffPair (Item txt1,List (op2,txt2,s2,ts2)) ->
         pp_txt indent s
           (List
              (op2,txt1 ^ " except when " ^ Misc.uncapitalize txt2,
               s2,ts2))
      | DiffPair (t1,t2) ->
         let ts =
           match t2 with
           | Item txt2 ->
              [t1;Item ("Except when " ^ txt2)]
            | List (op2,txt2,s2,ts2) ->
               [t1;
                List
                  (op2,
                   "Except when " ^ Misc.uncapitalize txt2,
                   s2,ts2)]
            | _ ->
               [t1;Item "Except when";t2;] in
            pp_txt indent s
              (List
              (Diff,
               "The following applies",("",""),
               ts))
      | IfCond (txt,a,b) ->
         let txt = sprintf "When %s" txt
         and ts = [a; Item "Otherwise"; b;] in
         pp_txt indent s (List (Inter,txt,("",""),ts))

        and pp_txts indent s1 s2 s3 = function
      | [] -> ()
      | [txt] ->
         pp_txt indent s3 txt
      | [txt1; txt2;] ->
         pp_txt indent s2 txt1 ;
         pp_txt indent s3 txt2
      | txt::txts ->
         pp_txt indent s1 txt ;
         pp_txts indent s1 s2 s3 txts

    (* Translate and print a definition *)
    let get_id_e_type id e =
      match get_id_type id with
      | None -> get_type e
      | Some _ as r -> r

    let tr_def loc id d =
      Next.reset () ;
      let ty = get_id_e_type id d in
      if O.verbose > 0 then begin
        match ty with
        | None ->
           eprintf "%a: %s has no type\n" TxtLoc.pp loc id
        | Some ty ->
           eprintf "%a: %s is %s\n"
             TxtLoc.pp loc id
             (match ty with
              | SET -> "an event set"
              | RLN -> "a relation")
        end ;
        let es,tr =
        match ty with
        | Some RLN ->
            let e1 = Next.next () in
            let e2 = Next.next () in
            [| e1; e2; |],tr_rel e1 e2
        | Some SET ->
            let e = Next.next () in
            [| e |],tr_evts e
        | None ->
           eprintf "%a: Cannot find type of %s\n"
             TxtLoc.pp loc id ;
           let e1 = Next.next () in
           let e2 = Next.next () in
           [| e1; e2; |],tr_rel e1 e2 in
      let pref =
        match ty with
        | Some RLN|None ->
          (* assumes /<name>emph macro *)
          let def_txt = (pp_id loc id) ^ "emph" in
           sprintf
             "%s if"
             (makeuppercase
              @@ sprintf "\\%s{an Effect %s}{an Effect %s}"
                   def_txt (pp_evt es.(0)) (pp_evt es.(1)))
        | Some SET ->
           sprintf
             "%s if"
             (makeuppercase
              @@ sprintf "\\%s{an Effect %s}"
                   (pp_id loc id) (pp_evt es.(0))) in
      let d =
        if O.flatten then
         ASTUtils.flatten d |> tr |> flatten_out 
        else tr d in
      pp_def pref "." d ;
      if O.testmode || StringSet.cardinal O.names > 1 then printf "\\par\n"

    (********************)
    (* Find definitions *)
    (********************)

    let in_names = function
      | Pvar (Some id) -> if StringSet.mem id O.names then Some id else None
      | Pvar None|Ptuple _ -> None

    let as_name = function
      | Pvar id -> id
      | Ptuple _ ->  None

    let rec tr_ast fname =
      let (_,_,ast) = P.parse fname in
      List.iter tr_ins ast

    and tr_ins = function
      | Let (_,bds) -> tr_bds in_names bds
      | Rec (_,bds,_) ->
         if
           List.exists (fun (_,id,_) -> in_names id |> Misc.is_some) bds
           && List.for_all
                (function | (_,Pvar (Some _),_) -> true |_ -> false)
                bds
         then tr_bds as_name bds
      | Include (_,fname) when O.expand -> tr_ast fname
      | _ -> ()

    and tr_bds pred bds =
      List.iter
        (fun (loc,p,d) ->
          match pred p with
          | Some id ->
             if O.verbose > 0 then
               eprintf
                 "%a: Handling definition of %s\n"
                 TxtLoc.pp loc id ;
             tr_def loc id d
          | None -> ())
        bds

    let ok_def pat e =
      match pat with
      | Pvar (Some id) when Misc.is_some (get_id_e_type id e) -> Some id
      | _ -> None

    let rec tst_ast fname =
      let (_,_,ast) = P.parse fname in
      List.iter tst_ins ast

    and tst_ins = function
      | Let (_,bds)
      | Rec (_,bds,_)
        ->tst_bds bds
      | Include (_,fname) when O.expand -> tst_ast fname
      | _ -> ()

    and tst_bds bds =
      List.iter
        (fun (loc,p,d) ->
          match ok_def p d with
          | Some id ->
             eprintf "%a: Translating definition %s\n%!" TxtLoc.pp loc id ;
             tr_def loc id d ;
             printf "\n%!"
          | None -> ())
        bds

    let tr_ast name =
      (if O.testmode then tst_ast else tr_ast) name ;
      let defs = get_nodefs () in
      if not (StringSet.is_empty defs) then begin
        eprintf "Warning: the following commands are undefined:\n" ;
        StringSet.iter
          (fun name -> eprintf "\\%s\n" name)
          defs
      end

  end

(****************************************)
(* Parse command line arguments proceed *)
(****************************************)

let verbose = ref 0
let libdir = ref (Filename.concat Version.libdir "herd")
let includes = ref []
let names = ref StringSet.empty
let testmode = ref false
let texfile = ref None
let expand = ref true
let flatten = ref true
let std = ref false

let options =
  [
(* Basic *)
    ("-version", Arg.Unit
     (fun () -> printf "%s, Rev: %s\n" Version.version Version.rev ; exit 0),
   " show version number and exit") ;
    ("-libdir", Arg.Unit (fun () -> print_endline !libdir; exit 0),
    " show installation directory and exit");
    ("-set-libdir", Arg.String (fun s -> libdir := s),
    "<path> set installation directory to <path>");
    ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
    ("-q", Arg.Unit (fun _ -> verbose := -1 ),
   "<default> do not show diagnostics");
    ("-I", Arg.String (fun s -> includes := !includes @ [s]),
   "<dir> add <dir> to search path");
    ArgUtils.parse_stringset "-show" names "show those names definitions";
    ArgUtils.parse_bool "-test" testmode "translate as many names as possible";
    ArgUtils.parse_bool "-expand" expand "expand include statements";
    ArgUtils.parse_bool "-flatten" flatten "flatten associative operators";
    ArgUtils.parse_bool "-stdlatex" std "output for standard latex";
    ArgUtils.parse_string_opt "-tex" texfile "file of LaTeX definitions";
  ]

(* Parse command line *)
let args = ref []
let get_cmd_arg s = args := s :: !args

let () =
  try
    Arg.parse options
      get_cmd_arg
      (sprintf "Usage %s [options] [files]+, translate cat definition into English." prog)
  with
  | Misc.Fatal msg -> eprintf "%s: %s\n" prog msg ; exit 2

let cats = List.rev !args

let () =
  let module Zyva =
    Make
      (struct
        let verbose = !verbose
        let includes = !includes
        let libdir = !libdir
        let names = !names
        let expand = !expand
        let flatten = !flatten
        let std = !std
        let testmode = !testmode
        let texfile = !texfile
      end) in
  let zyva name =
    try Zyva.tr_ast name
    with
    | Misc.Fatal msg ->
       Warn.warn_always "%a: %s" Pos.pp_pos0 name msg ;
       exit 2
    | Misc.UserError msg ->
       Warn.warn_always "%s (User error)" msg ;
       exit 2
    | Misc.Exit -> ()
    | e ->
       Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
       raise e in
    List.iter zyva cats
