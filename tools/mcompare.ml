(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Misc
open Printf

(*****************************)
(* Option and argument parse *)
(*****************************)

let logs = ref []
let verb = ref 0

type runopts =
    {mode:OutMode.t;
     do_show:string;
     show_litmus_summary:bool;
     do_sum:bool;
     outputfile:string option;
     restrict_by_first_column:bool;
     rename:string option;
     forcekind: string option;
     kinds:string list;
     show_kinds: bool;
     orders:string list;
     conds:string list;
     show_litmus : bool ;
     show_model : bool ;
     show_fst : bool ;
     show_empty_rows : bool ;
     filter : string option ;
     kmg : bool ;
     quiet : bool ;
     select : string list ;
     names : string list ;
     check_hash : bool ;
     texmacro : bool ;
     dump_eq : string option ;
     dump_pos : string option ;
     dump_neg : string option ;
     cond_pos : string option ;
     cond_neg : string option ;
     opt_cond : bool ;
   }

let default_runopts =
  {mode = OutMode.Txt;
   do_show = "";
   show_litmus_summary = false;
   do_sum = false; 
   outputfile = None;
   restrict_by_first_column = false;
   rename = None ;
   forcekind = None ;
   kinds = [] ; conds = [] ; orders=[];
   show_litmus = true ;
   show_model = true ;
   show_fst = true ;
   show_empty_rows = true ;
   show_kinds = false ;
   filter = None ;
   kmg = false ;
   quiet = false ;
   names = [];
   select = [];
   check_hash = true ;
   texmacro = false ;
   dump_eq = None;
   dump_pos = None;
   dump_neg = None;
   cond_pos = None;
   cond_neg = None;
   opt_cond = false;
 }

let runopts = default_runopts

let delayed_options = ref (fun ro -> ro)

let delay_ro f x = 
  let prev = !delayed_options in
  delayed_options := 
    (fun ro -> prev (f x ro)) 

let options =
  [
  ("-v", Arg.Unit (fun _ -> incr verb),
   "<non-default> show various diagnostics, repeat to increase verbosity");
  ("-show", Arg.String
     (delay_ro
        (fun s ro -> {ro with do_show = ro.do_show ^ s})),
   "<"^ runopts.do_show ^
   "> show control, argument is a string of
       * v (validation), V (verbose validation),
       * r (revalidation against conditions found in first file),
       * w (witnesses), W (kind-oriented witnesses),
       * s (states),
       * d (state differences),
       * t (timing),
       * p (productivity),
       * X special, for nice, informative, tables");
   ("-show_litmus_summary", Arg.Bool (delay_ro (fun b ro -> {ro with show_litmus_summary = b})),
    "<"^ string_of_bool runopts.show_litmus_summary ^ 
    "> show a summary of litmus logs passed in (roughly show V+W), and optionally any memevents log show V");
   ("-restrict", Arg.Bool (delay_ro (fun b ro -> {ro with restrict_by_first_column = b})),
    "<"^ string_of_bool runopts.restrict_by_first_column ^ 
    "> restrict to rows for which there is an entry in the first column");
   ("-nolitmus",
       Arg.Unit (delay_ro (fun () ro -> {ro with show_litmus = false})),
     " do not show hardware experiments") ;
   ("-nofst",
       Arg.Unit (delay_ro (fun () ro -> {ro with show_fst = false})),
     " do not show models") ;
   ("-noempty",
       Arg.Unit (delay_ro (fun () ro -> {ro with show_empty_rows = false})),
     " do not show empty rows") ;
  begin let module P = ParseTag.Make(OutMode) in
  P.parse_withfun "-mode"
    (delay_ro (fun m ro -> { ro with mode = m; }))
    (sprintf
       "output mode, %s" (OutMode.pp default_runopts.mode))
    None
  end ;  
  ("-macro",
    Arg.Unit
      (delay_ro
         (fun () ro -> { ro with texmacro = true; })),
      " macros in latex/hevea output");
   ("-names",
    Arg.String
      (delay_ro (fun s ro -> { ro with names = s :: ro.names})),
    "<name> specify  selected name file, can be repeated") ;       
   ("-select",
    Arg.String
      (delay_ro (fun s ro -> { ro with select = s :: ro.select})),
    "<name> specify selected test file (or index file), can be repeated") ;       
   ("-rename",
    Arg.String
      (delay_ro (fun s ro -> { ro with rename = Some s })),
    "<name> specify a rename mapping, for changing test names in output") ;    
   ("-nohash",
    Arg.Unit
      (delay_ro (fun () ro -> { ro with check_hash = false; })),
    " do not check hashes");

   ("-forcekind",
    Arg.String
      (delay_ro (fun s ro -> { ro with forcekind= Some s; })),
        "<Allow|Forbid|Require> force kind of all tets") ;    

   ("-kinds",
    Arg.String
      (delay_ro (fun s ro -> { ro with kinds= ro.kinds @ [s] })),
        "<name> specify kinds of tests") ;    
   ("-showkinds",
    Arg.Bool
      (delay_ro (fun s ro -> { ro with show_kinds= s })),
        "<bool> show kinds in table") ;
   ("-conds",
    Arg.String
      (delay_ro (fun s ro -> { ro with conds= ro.conds @ [s] })),
        "<name> specify conditions of tests") ;    
   ("-cost",
    Arg.String
      (delay_ro (fun s ro -> { ro with orders= ro.orders @ [s] })),
        "<name> specify order of tests") ;    
   ("-e",
    Arg.String
     (delay_ro (fun s ro -> { ro with filter = Some s; })),
    "<regexp> filter test names with <regexp>") ;
   ("-kmg",
    Arg.Unit
     (delay_ro (fun () ro -> { ro with kmg = true; })),
    "show numbers as kilo/mega/giga") ;
   ("-terse",
    Arg.Unit
     (delay_ro (fun () ro -> { ro with quiet = true; })),
    "produce terser tables") ;
   ("-o",
    Arg.String
      (delay_ro
	 (fun s ro -> {ro with outputfile = Some s})),
    "<"^ (match runopts.outputfile with None -> "" | Some s -> s) ^ ">");
   ("-pos",
     Arg.String
      (delay_ro
	 (fun s ro -> {ro with dump_pos = Some s})),
    " <file> dump positive differences, default "^ (match runopts.dump_pos with None -> "don't dump" | Some s -> s));
   ("-eq",
     Arg.String
      (delay_ro
	 (fun s ro -> {ro with dump_eq = Some s})),
    " <file> dump perfect fits, default "^ (match runopts.dump_eq with None -> "don't dump" | Some s -> s));
   ("-neg",
     Arg.String
      (delay_ro
	 (fun s ro -> {ro with dump_neg = Some s})),
    "<file> dump negative differences, default "^ (match runopts.dump_neg with None -> "don't dump" | Some s -> s));
   ("-cpos",
     Arg.String
      (delay_ro
	 (fun s ro -> {ro with cond_pos = Some s})),
    " <file> dump positive conditions, default "^ (match runopts.cond_pos with None -> "don't dump" | Some s -> s));
   ("-cneg",
     Arg.String
      (delay_ro
	 (fun s ro -> {ro with cond_neg = Some s})),
    "<file> dump negative conditions, default "^ (match runopts.cond_neg with None -> "don't dump" | Some s -> s));
   ("-optcond",
      Arg.Bool
        (delay_ro
           (fun b ro -> { ro with opt_cond = b; })),
    (sprintf "<bool> optimise dumped conditions, default %b" runopts.opt_cond));
 ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "compare"

let () =
  Arg.parse options
    (fun s -> logs := s :: !logs)
    (sprintf "Usage %s [options]* [logs]*
  - logs are log file names from memevents or litmus
  - options are:" prog)

let runopts = (!delayed_options) runopts


(************************)
(* Settle configuration *)
(************************)

module type Config = sig
  val mode : OutMode.t
  val verbose : int
  val show_litmus_summary : bool
  val chan : out_channel 
  val restrict : bool
  val forcekind :  LogState.kind option
  val rename : string TblRename.t
  val kinds : LogState.kind TblRename.t
  val conds : LogConstr.constr TblRename.t
  val orders : unit TblRename.t
  val show_litmus : bool
  val show_model : bool
  val show_fst : bool
  val show_empty_rows : bool
  val show_kinds : bool
  val filter : Str.regexp option
  val kmg : bool
  val quiet : bool
  val names : StringSet.t option
  val check_hash : bool
  val macro : bool
  val dump_eq : string option
  val dump_pos : string option
  val dump_neg : string option
  val cond_pos : string option
  val cond_neg : string option
  val opt_cond : bool
end

module Verbose = struct let verbose = !verb end
module LR = LexRename.Make(Verbose)
module LS = LogState.Make(Verbose)


module Config = struct 
  let mode = runopts.mode 
  let verbose = !verb
  let show_litmus_summary = runopts.show_litmus_summary
  let restrict = runopts.restrict_by_first_column

  let chan =  match runopts.outputfile with
    | Some s -> open_out s
    | None -> stdout

  let forcekind = match runopts.forcekind with
  | None -> None
  | Some s ->
      begin match  LS.parse_kind s with
      | None -> failwith (sprintf "'%s' is not a kind" s)
      | Some _ as k -> k
      end

  let kinds  = LR.read_from_files runopts.kinds LS.parse_kind

  let conds = LR.read_from_files runopts.conds LogConstr.parse

  let orders = LR.read_from_files runopts.orders (fun _ -> Some ())

  let rename = match runopts.rename with
  | None -> TblRename.empty
  | Some fname -> LR.read_from_file fname (fun s -> Some s)

  let do_rename name =
    try TblRename.find_value rename name
    with Not_found -> name

  let show_litmus = runopts.show_litmus
  let show_model = runopts.show_model
  let show_fst = runopts.show_fst
  let show_empty_rows =  runopts.show_empty_rows
  let show_kinds = runopts.show_kinds
  let filter = match runopts.filter with
  | None -> None
  | Some s -> Some (Str.regexp s)
  let kmg = runopts.kmg
  let quiet = runopts.quiet


  let names1 = match runopts.names with
  | [] -> None
  | args ->
      let add t = StringSet.add (do_rename t) in
      Some
        (List.fold_left
           (fun r name -> ReadNames.from_file name add r)
           StringSet.empty args)

  let names2 = match runopts.select with
  | [] -> None
  | args ->
      let names = Names.from_fnames (Misc.expand_argv args) in
      let names = List.rev_map do_rename names in
      Some (StringSet.of_list names)

  let names = match names1,names2 with
  | (None,ns)|(ns,None) -> ns
  | Some ns1,Some ns2 -> Some (StringSet.union ns1 ns2)

  let check_hash = runopts.check_hash
  let macro = runopts.texmacro
  let dump_eq = runopts.dump_eq
  let dump_pos = runopts.dump_pos
  let dump_neg = runopts.dump_neg
  let cond_pos = runopts.cond_pos
  let cond_neg = runopts.cond_neg
  let opt_cond = runopts.opt_cond
end

(************)
(* Let's go *)
(************)

module Make (Opt:Config) = struct
  open Opt
  open LogState

  module D = Matrix.Dump(Opt)

  let do_rename name =
    try TblRename.find_value Opt.rename name
    with Not_found -> name

(* Filter test by names while reading logs *)
  let ok_match = match Opt.filter with
  | None -> (fun _ -> true)
  | Some e ->  (fun name -> Str.string_match e name 0)

  let ok_select = match  Opt.names with
  | None -> fun _ -> true
  | Some set -> fun name -> StringSet.mem name set


  let ok_name name = ok_select name && ok_match name

(* Erase some columns at the very last moment *)
  let erase_cols logs xs = match xs with
  | [] -> []
  | _ ->
      List.fold_right2 
        (fun log x k ->
          let ok =
            if log.is_litmus then show_litmus else show_model in
          if ok then x::k else k)
        logs xs []

  let erase_fst xs = match xs with
  | [] -> []
  | _::xs -> xs

  let dump =
    if show_litmus && show_model && show_fst then (fun _ -> D.dump)
    else
      let erase_fst =
        if show_fst then  (fun xs -> xs)
        else erase_fst in
      let erase logs xs = erase_fst (erase_cols logs xs) in
      (fun logs legend horiz row1 rown col1 ?col2:col2 m ->
        D.dump legend horiz
          (erase logs row1)
          (erase logs rown)
          col1 ?col2:col2
          (erase logs m))

  module K = Key.Make(Opt)

(******************)
(* Read log files *)
(******************)

  module LL =
    LexLog.Make
      (struct
        let verbose = Opt.verbose
        let rename = do_rename
        let ok = ok_select
      end)


  type hash_t =
      { h : string ; orig : string ; }

  let read_logs =

    let hash_ok = ref true in

    let table  = Hashtbl.create 101 in

    let check_tests fname ts =
      Array.iter
        (fun t -> match t.hash with
        | None -> ()
        | Some h ->
            if h <> "NOHASH"  then
              begin try              
                let oh = Hashtbl.find table t.tname  in
                if  oh.h <> h then begin
                  hash_ok := false ;
                  Warn.warn_always
                    "Hash mismatch, test %s, files %s<>%s"
                    t.tname oh.orig fname
                end
              with Not_found ->
                Hashtbl.replace table t.tname {h=h; orig=fname;}
                end)
        ts in

    let check_hashes ts =
      List.iter (fun t -> check_tests t.name t.tests) ts in

    fun  logs -> 
      let ts = LL.read_names logs in
      if Opt.check_hash then begin
        check_hashes ts ;
        if not !hash_ok then Warn.fatal "Hash mismatch occured"
      end ;
      ts

(* utilities *)
  open OutMode
  let my_error s = match mode with 
  | Txt -> "ERROR:" ^ s
  | LaTeX|HeVeA|HeVeANew -> "\\myerror{" ^ s ^ "}"


  let my_warn s =  match mode with 
  | Txt -> "WARN:" ^ s
  | LaTeX|HeVeA|HeVeANew -> "\\mywarn{" ^ s ^ "}"


(**********************)
(* Get all test names *)
(**********************)

  let rec union compare xs ys = match xs,ys with
  | ([],zs)|(zs,[]) -> zs
  | x::xs,y::ys ->
      let c = compare x y in
      if c < 0 then x::union compare xs (y::ys)
      else if c > 0 then y::union compare (x::xs) ys
      else y::union compare xs ys

  let rec union2 compare = function
    | []|[_] as xss -> xss
    | xs::ys::rem -> union compare xs ys::union2 compare rem

  let rec unions compare xss = match xss with
  | [] -> []
  | [xs] -> xs
  | _ -> unions compare (union2 compare xss)


  let from_tests ts =
    let r = ref StringSet.empty in
    Array.iter
      (fun t -> r := StringSet.add t.tname !r)
      ts ;
    !r

  let build_tnames ts =
    let names = 
      if restrict then
        match ts with
        | [] -> StringSet.empty
        | t1::_ -> from_tests t1.tests
      else match Opt.names with
      | None ->
        let names =
          List.map (fun {tests=ts;_} -> from_tests ts) ts in
        StringSet.unions names
      |  Some set -> set in
    let names = 
      match Opt.filter with
      | None -> names
      | Some e ->  
          StringSet.filter
            (fun name -> Str.string_match e name 0)
            names in
    Array.of_list (StringSet.elements names)



  let full_pp_mem_log t =
    let k = LS.pp_kind t.kind in
    let v = (if t.loop then "Loop " else "") ^ LS.pp_validation t.validation in
    [k;v]

  let full_pp_mem_logs = Array.map full_pp_mem_log


(**************)
(* Validation *)
(**************)

  let pp_name n = Filename.basename n

  module ValidateAdd = struct

    type v = LogState.validation

    let add v t = match v,t.validation with
    | Ok,Ok -> Ok
    | (No,Ok)|(Ok,No)|(No,No) -> No
    | _,_ -> DontKnow
  end

  let show_validate tnames ts =
    let keys = K.Kind.add tnames ts in
    let module B =
      Matrix.Build
        (struct

          type info = K.Kind.info

          let fmt_cell _col i t = match i.K.Kind.kind with
          | ErrorKind -> assert false
          | NoKind    ->
              [sprintf "%s (%s)" 
                 (LS.pp_validation t.validation)
                 (LS.pp_kind t.kind)]
          | _ -> [LS.pp_validation t.validation]

          include ValidateAdd

        end) in
    let m = B.build keys ts in
    if quiet then
      dump ts "Validation" true
        (List.map (fun t -> 1,pp_name t.name) ts) []
        keys
        m
    else
      let sum = B.sum keys (List.map (fun _ -> Ok) ts) ts in
      let sum = List.map LS.pp_validation sum in    
      dump ts "Validation" true
        (List.map (fun t -> 1,pp_name t.name) ts) sum
        keys
        ~col2:(K.Kind.pps keys)
        m ;
    output_char chan '\n' 

  module FmtValidate = struct

    type info = K.Kind.info
          
    let fmt_cell col i t =
      let k = i.K.Kind.kind in
      [if col.is_litmus then begin
	match k,t.validation with
	| Allow,Ok -> "Allow val."
	| Allow,No -> my_warn "Allow not val."
	| _,Ok -> LS.pp_kind t.kind ^ " val."
	| _,_ -> my_error (LS.pp_kind t.kind ^ " NOT val.")
      end else begin 
	if t.validation = Ok then LS.pp_kind k
        else my_error (LS.pp_kind k ^ " violated")
      end]         

    include ValidateAdd
  end

  let show_validate_verbose tnames ts =
    let keys = K.Kind.add tnames ts in
    let module B = Matrix.Build (FmtValidate) in
    let m = B.build keys ts in
    dump ts "Validation" false
      (List.map (fun t -> 1,pp_name t.name) ts) []
      keys ~col2:(K.Kind.pps keys) m ;
    output_char chan '\n' 

(****************)
(* Revalidation *)
(****************)

  let kind_of c = match c with
  | None -> NoKind
  | Some c ->  match c with
    | ConstrGen.NotExistsState _ -> Forbid
    | ConstrGen.ExistsState _ -> Allow
    | ConstrGen.ForallStates _ -> Require



  let highlight_bad s =  match mode with
  | Txt -> s
  | LaTeX|HeVeA|HeVeANew ->
      if Config.macro then sprintf "\\colorinvalid{%s}" s
      else sprintf "{\\color{red}%s}" s

  let highlight_moderate s =  match mode with
  | Txt -> s
  | LaTeX|HeVeA|HeVeANew ->
      if Config.macro then sprintf "\\colorunseen{%s}" s
      else sprintf "{\\color{blue}%s}" s



  let add_comment eqallowed _name is_litmus unsure k v v_pp =
    if quiet then [v_pp]
    else if is_litmus then
      v_pp::
      if unsure && verbose=0 then
        []
      else
        match Opt.forcekind,k,v with
        | (None,(Forbid|Require as k),No)
        | (Some (Forbid as k),Allow,Ok) ->
            [highlight_bad (sprintf "%s invalidated" (LS.pp_kind k))]
        | (None|Some Allow),Allow,No ->
            [highlight_moderate (sprintf "Allow unseen")]
        | _,_,_ -> []
(* Model log *)
    else
      match k,v with
      | k,Ok ->
          if (unsure && verbose=0) || not eqallowed then [LS.pp_kind k]
          else ["="]
      | Allow,No ->
          let pp = LS.pp_kind Forbid in
          let pp = if unsure then pp else highlight_bad pp in
          [pp]
      | Forbid,No ->
          let pp = LS.pp_kind Allow in
          let pp = if unsure then pp else highlight_moderate pp in
          [pp]
      | Require,No -> ["Not Require"]
      | _ -> ["??"]

  let add_short_comment _name is_litmus unsure k v v_pp =
    if quiet then [v_pp]
    else if is_litmus then
      if unsure && verbose=0 then
        [v_pp]
      else
        match k,v with
        | (Forbid|Require),No ->
            [highlight_bad v_pp]
        | Allow,No ->
            [highlight_moderate v_pp]
        | _,_ -> [v_pp]
(* Model log *)
    else
      match k,v with
      | k,Ok ->
          if unsure && verbose=0 then [LS.pp_kind k]
          else ["="]
      | Allow,No ->
          let pp = LS.pp_kind Forbid in
          let pp = if unsure then pp else highlight_bad pp in
          [pp]
      | Forbid,No ->
          let pp = LS.pp_kind Allow in
          let pp = if unsure then pp else highlight_moderate pp in
          [pp]
      | Require,No -> ["Not Require"]
      | _ -> ["??"]

  let show_revalidate ts = match ts with
  | [] -> ()
  | t1::_ ->
      let keys = K.Cond.add t1 in
      let module B =
        Matrix.Build
          (struct
            type info = K.Cond.info

            let fmt_cell col
                { K.Cond.cond=cond; K.Cond.unsure=unsure; kind=k;} t =
              let v = LS.revalidate cond t.states in
              add_comment true t.tname
                col.is_litmus unsure k v                
                (LS.pp_validation v)
            include Matrix.NoAdd
          end) in
      let m = B.build keys ts in
      dump ts "Revalidation" true
        (List.map (fun t -> 1,pp_name t.name) ts) []
        keys
        ~col2:
        (Misc.array_map2
           (fun t k ->
             let i =  k.Key.info in
             let k = i.K.Cond.kind
             and unsure = i.K.Cond.unsure in
             [LS.pp_kind k ^
              (if unsure then "?" else "") ^
              (if unsure && t.loop then " (Loop)" else "")])
           t1.tests keys)
        m ;
      output_char chan '\n' 


(************)
(* Outcomes *)
(************)

  module FmtState = struct
    type info = unit

    let fmt_cell col _key t =
      LS.pretty_states "" mode col.is_litmus t.states

    include Matrix.NoAdd
  end

  let show_states tnames ts =
    let module B = Matrix.Build (FmtState) in
    let keys = K.None.add_names tnames in
    let r = B.build keys ts in
    dump ts "Outcomes" true
      (List.map (fun t -> 1,pp_name t.name) ts) []
      keys r ;
    output_char chan '\n' 

(* Idem but use fst column as reference and omit it *)
  let show_states_fst ts = match ts with
  | [] -> ()
  | t1::ts ->
      let keys = K.None.add_log t1 in
      let module B = Matrix.Build (FmtState) in
      let r = B.build keys ts in
      dump ts "Outcomes" true
        (List.map (fun t -> 1,pp_name t.name) ts) []
        keys r ;
      output_char chan '\n' 

(*************)
(* Witnesses *)
(*************)

let format_int_string s = 
  let char_list_of_string s = 
    let n = String.length s in
    let rec f i = if i=n then [] else String.get s i :: f (i+1) in
    f 0 in
  let string_of_char_list ts =
    let n = List.length ts in
    let buf = Buffer.create n in
    let rec f ts =
      match ts with 
      | [] -> ()
      | t::ts -> Buffer.add_char buf t; f ts
    in 
    f ts;
    Buffer.contents buf in
  let string_rev s = string_of_char_list (List.rev_append (char_list_of_string s) []) in
  let rec split_in_threes cs = match cs with
  | c1::c2::c3::cs' -> (String.make 1 c1 ^ String.make 1 c2 ^ String.make 1 c3) :: split_in_threes cs'
  | [] -> [] 
  | _ -> [string_of_char_list cs] in
  (string_rev (String.concat " " (split_in_threes (List.rev (char_list_of_string s)))))

  let format_int64_as_int x =
    format_int_string (Int64.to_string x)
      
  let format_int64 x =
    let xs =
      if Int64.compare x 1000L < 0 then format_int64_as_int x
      else
        Str.global_replace ((Str.regexp_string "e+1")) "e1"
          (Str.global_replace ((Str.regexp_string "e+0")) "e" 
             (sprintf "%1.1g" (Int64.to_float x))) in
    sprintf "%3s" xs

  let fmt_approx x u =
    let y = Int64.div (Int64.add x (Int64.div u 2L)) u in
    if Int64.compare y 10L < 0 then
      let v = Int64.div u 10L in
      let y = Int64.div (Int64.add x (Int64.div v 2L)) v in
      let y = Int64.to_string y in
      assert (String.length y = 2) ;
      sprintf "%c.%c" y.[0] y.[1]
    else Int64.to_string y


  let format_int64_alt x =
    if Int64.compare x 1000L < 0 then
      Int64.to_string x
    else if Int64.compare x 1000000L < 0 then 
      fmt_approx x 1000L ^ "k"
    else if Int64.compare x 1000000000L < 0 then 
      fmt_approx x 1000000L ^ "M"
    else if Int64.compare x 1000000000000L < 0 then 
      fmt_approx x 1000000000L ^ "G"
    else if Int64.compare x 1000000000000000L < 0 then 
      fmt_approx x 1000000000000L ^ "T"
    else failwith "Improbable :)"

  let digit2 f = 
    if f < 1.0 then sprintf "%0.2f" f
    else if f < 10.0 then sprintf "%0.1f" f
    else sprintf "%02.0f" f

  let fmt_float_approx x u =  digit2 (x /. u)

  let fmt_float_alt x =
    if x < 1.0 then
      sprintf "%0.1f" x
    else if x < 1000.0 then
      fmt_float_approx x 1.0
    else if x < 1000000.0 then
      fmt_float_approx x 1000.0 ^ "k"
    else if x < 1000000000.0 then
      fmt_float_approx x 1000000.0 ^ "M"
    else 
      fmt_float_approx x 1000000000.0 ^ "G"

  let pp_wits p n = 
    let all = Int64.add p n in
    match mode with
    | LaTeX|HeVeA|HeVeANew when not kmg -> 
        sprintf "\\mydata{%s}{%s}{%s}{%s}{%s}" 
          (format_int64_as_int p)
          (format_int64_as_int n)
          (format_int64_as_int all)
          (format_int64 p)
          (format_int64 all)
    | Txt|LaTeX|HeVeA|HeVeANew ->
        let ps = format_int64_alt p
        and alls = format_int64_alt all in
        sprintf "%s/%s" ps alls



  module FmtWitness(O : sig val oriented : bool end) = struct

    type info = K.Kind.info

    let fmt_cell _col i t =
      let p,n = t.witnesses in
      let wits = 
	let p,n = if O.oriented && t.kind = Forbid then n,p else p,n in
        if Int64.compare (Int64.add p n) 0L = 0 then "??"
	else pp_wits p n in      
      let wits = match i.K.Kind.kind with
      | ErrorKind -> assert false
      | NoKind -> sprintf "%s (%s)" wits (LS.pp_kind t.kind)
      | _ -> wits in
      [wits]

    include Matrix.NoAdd
  end  

  let show_wits kind_oriented tnames ts =
    let keys = K.Kind.add tnames ts in
    let module B =
      Matrix.Build 
        (FmtWitness (struct let oriented = kind_oriented end)) in
    let r = B.build keys ts in
    dump ts "Witnesses" true
      (List.map (fun t -> 1,pp_name t.name) ts) []
      keys ~col2:(K.Kind.pps keys) r ;
    output_char chan '\n' 

(***********************)
(* Recompute witnesses *)
(***********************)

  type xyz = X | Y | Z

  let show_XYZ xyz =
    let asY = match xyz with Y -> true | X|Z -> false in
    let asZ = match xyz with Z -> true | X|Y -> false in
    fun ts -> match ts with
    | [] -> ()
    | _::_ ->
        let keys = K.Cond.adds ts in
        let module B =
          Matrix.Build
            (struct
              type info = K.Cond.info

              let fmt_cell col
                  { K.Cond.cond = cond; K.Cond.unsure = unsure; kind=k } t =
                if verbose > 1 then
                  begin match cond with
                  | None -> eprintf "No cond for %s\n" t.tname
                  | Some c ->
                      eprintf "Cond for %s: <%a>[kind=%s]\n"
                        t.tname LogConstr.dump c (LS.pp_kind k)
                  end ;
                let v = LS.revalidate cond t.states in
                let v_pp =
                  if col.is_litmus then
                    let p,n = LS.witness_again cond t.states in
                    match v with
                    | Ok|No ->
                        if asY || (unsure && verbose = 0) then
                          pp_wits p n
                        else
                          sprintf "%s, %s" (LS.pp_validation v) (pp_wits p n)
                    | _ -> LS.pp_validation v
                  else
                    "X" ^ LS.pp_validation v in
                (if asY then add_short_comment else add_comment (not asZ))
                  t.tname col.is_litmus unsure k v v_pp

              include Matrix.NoAdd
            end) in
        let m = B.build keys ts in
        if quiet || ( asZ && not show_kinds ) then
          dump ts "WitnessesRecomputed" true
            (List.map (fun t -> 1,pp_name t.name) ts) [] keys m
        else
          dump ts "WitnessesRecomputed" true
            (List.map (fun t -> 1,pp_name t.name) ts) [] keys
            ~col2:
            (Array.map
               (fun k ->
                 let i =  k.Key.info in
                 let kind = i.K.Cond.kind
                 and unsure = i.K.Cond.unsure in
                 let ppk = 
                   if unsure then
                     if verbose > 0 then LS.pp_kind kind ^ "?"
                     else "---"
                   else LS.pp_kind kind in
                 [ppk])
               keys)
            m ;
        output_char chan '\n'

(**************)
(* Efficiency *)
(**************)

  let show_E ts = match ts with
  | [] -> ()
  | t1::_ ->
      let keys = K.Cond.add t1 in
      let module B =
        Matrix.Build
          (struct
            type info = K.Cond.info

            let fmt_cell _col { K.Cond.cond = cond; _ } t =
              let v_pp =
                match t.time with
                | None -> "--"
                | Some time -> 
                    let p,_ = LS.witness_again cond t.states in
                    if Int64.compare p Int64.zero = 0 then
                      if verbose > 0 then
                        sprintf "-- [%0.2f] " time
                      else
                        ""
                    else
                      let e = Int64.to_float p /. time in
                      if verbose > 0 then
                        sprintf "%0.1f [%s/%0.2f]"
                          e (format_int64_alt p) time
                      else
                        fmt_float_alt e in (* sprintf "%0.1f" e in *)
              [v_pp]

            include Matrix.NoAdd
                
          end) in
      let m = B.build keys ts in
      dump ts "Efficiency" true
        (List.map (fun t -> 1,pp_name t.name) ts) [] keys m ;
      output_char chan '\n'

(**********)
(* Timimg *)
(**********)

(* Pure time *)
  module FmtTime = struct
    type info = unit

    let fmt_cell _col _key t = match t.time with
    | None -> []
    | Some ti -> [sprintf "%0.2f" ti]

    type v = float
    let add f t = match t.time with
    | None -> f
    | Some g -> f +. g

  end

  let show_time tnames ts =
    let module B = Matrix.Build (FmtTime) in
    let keys = K.None.add_names tnames in
    let r = B.build keys ts in
    dump ts "Time" true
      (List.map (fun t -> 1,pp_name t.name) ts) []
      keys r ;
    output_char chan '\n' 

(* Productivity (ie outcomes/sec) *)
  module FmtProd = struct
    type info = unit

    let fmt_cell _col _key t = match t.time with
    | None -> []
    | Some ti ->
        let os = Int64.to_float (LS.get_nouts t.states) in
        let x = os /. ti in
        [fmt_float_alt x]

    type v = Int64.t * float

    let add (occs,f as acc) t = match t.time with
    | None -> acc
    | Some ti -> Int64.add occs (LS.get_nouts t.states), f +. ti

  end

  let show_prod tnames ts =
    let module B = Matrix.Build (FmtProd) in
    let keys = K.None.add_names tnames in
    let r = B.build keys ts in
    let sum = B.sum keys (List.map (fun  _ -> 0L,0.0) ts) ts in
    let sum =
      List.map
        (fun (os,ti) ->
          let x = Int64.to_float os /. ti in
          fmt_float_alt x)
        sum in          
    dump ts "Productivity" true
      (List.map (fun t -> 1,pp_name t.name) ts) sum
      keys r ;
    output_char chan '\n' 

      
(*****************************************)
(* Diff matrix, first colum is reference *)
(*****************************************)

  let show_diffs ts = match ts with
  | [] -> ()
  | t1::ts ->
      let keys = K.Full.add t1 in
      let pos = ref StringSet.empty
      and neg = ref StringSet.empty
      and eq = ref StringSet.empty
      and pos_cond = ref []
      and neg_cond = ref [] in
      let module B =
        Matrix.Build
          (struct

(*
            let pp_bd (loc,v) = sprintf "%s=%s" loc v

            let pp_cond_simple bdss =
              let pp =
                List.map
                  (fun bds ->
                    String.concat " /\\ "
                      (List.map pp_bd bds))
                  bdss in
              let pp = List.map (sprintf "(%s)") pp in
              let pp = String.concat " \\/ "  pp in
              pp

            let compare_bd (loc1,v1) (loc2,v2) =
              match String.compare loc1 loc2 with
              | 0 -> String.compare v1 v2
              | r -> r

            module Env =
              MyMap.Make
                (struct
                  type t = string * string
                  let compare = compare_bd
                end)


            let pp_cond_opt =
              let build_env =
                  List.fold_left
                    (List.fold_left
                       (fun env bd ->
                         let old =
                           try Env.find bd env
                           with Not_found -> 0 in
                         Env.add bd (old+1) env))
                    Env.empty in
              let find_max env =
                Env.fold
                  (fun bd n (_,n_max as max) ->
                    if n > n_max then (bd,n) else max)
                  env (("",""),0) in

              let split bd bdss =
                let rec remove = function
                  | [] -> raise Not_found
                  | bd0::rem ->
                      if compare_bd bd bd0 = 0 then rem
                      else bd0::remove rem in
                List.fold_left
                  (fun (ok,no) bds ->
                    try
                      let bds = remove bds in
                      (bds::ok,no)
                    with
                      Not_found -> (ok,bds::no))
                  ([],[]) bdss in
              fun bdss ->
                let rec do_rec bdss = match bdss with
                | [] -> assert false
                | [bds] ->
                    sprintf "(%s)"
                      (String.concat " /\\ "
                         (List.map pp_bd bds))
                | []::_ ->
                    List.iter
                      (function
                        | [] -> ()
                        | _ -> assert false)
                      bdss ;
                    ""
                | [_]::_ ->
                    let bds =
                      List.map
                        (function
                          | [bd] -> pp_bd bd
                          | _ -> assert false)
                        bdss in
                    sprintf "(%s)"
                      (String.concat " \\/ " bds)
                | _ ->
                    let (bd_max,_) = find_max (build_env bdss) in
                    let ok,no = split bd_max bdss in
                    let pp_ok = do_rec ok in
                    let pp_no = match no with
                    | [] -> ""
                    | _ -> do_rec no in
                    match pp_ok,pp_no with
                    | "","" -> pp_bd bd_max
                    | "",_ -> sprintf "%s \\/ (%s)" (pp_bd bd_max) pp_no
                    | _,"" -> sprintf "%s /\\ (%s)" (pp_bd bd_max) pp_ok
                    | _,_  -> sprintf "%s /\\ (%s) \\/ (%s)"
                          (pp_bd bd_max) pp_ok pp_no in
                do_rec bdss
*)                
                       
              
              
            let pp_cond =
              if Opt.opt_cond then CondPP.pp_opt
              else CondPP.pp_simple

            let pp_prop name bdss =
              sprintf "%s \"exists %s\"" name (pp_cond bdss)

            type info = K.Full.info

            let fmt_cell  _col r t = try 
              let name = t.tname in
              let more = LS.diff_states t.states r.states
              and less = LS.diff_states r.states t.states in
              if not (LS.no_states more) then begin
                pos := StringSet.add name !pos ;
                pos_cond :=
                  pp_prop name (LS.get_bindings more) :: !pos_cond
              end ;
              if not (LS.no_states less) then begin
                neg := StringSet.add name !neg ;
                neg_cond :=
                  pp_prop name (LS.get_bindings less) :: !neg_cond
              end ;
              if LS.no_states more && LS.no_states less then begin
                eq := StringSet.add name !eq ;
                ["=="]
              end else
                LS.pretty_states "+" mode false more @
                LS.pretty_states "-" mode false less
            with StateMismatch loc ->
              Warn.fatal "State mismatch, test=%s, loc=%s" r.tname loc

            include Matrix.NoAdd

          end) in
      let m = B.build keys ts in
      let m1 =
        Array.map
          (fun t -> LS.pretty_states "" mode false t.states)
          t1.tests in
      dump ts "Diffs" true
        (List.map (fun t -> 1,pp_name t.name) (t1::ts)) []
        keys
        ~col2:(full_pp_mem_logs t1.tests) (m1::m) ;
      output_char chan '\n' ;
      let dump_set msg c s =
        match StringSet.elements s with
        | [] -> ()
        | xs ->
            fprintf chan "!!! Warning %s differences in:" msg ;
            List.iter
              (fun x -> fprintf chan " %c%s" c x)
              xs ;
            output_char chan '\n' in      
      dump_set "positive" '+' !pos ;
      dump_set "negative" '-' !neg ;
      let dump_chan s chan =
        StringSet.iter
          (fprintf chan "%s\n") s in
      let dump_file s name = Misc.output_protect (dump_chan s) name in
      begin match dump_eq with
      | None -> ()
      | Some file -> dump_file !eq file
      end ;
      begin match dump_pos with
      | None -> ()
      | Some file ->  dump_file !pos file
      end ;
      begin match dump_neg with
      | None -> ()
      | Some file ->  dump_file !neg file
      end ;
      let dump_cond_chan txt chan = List.iter (fprintf chan "%s\n") txt in
      let dump_cond txt o = match o with
      | None -> ()
      | Some name -> output_protect (dump_cond_chan txt) name in
      dump_cond !pos_cond Config.cond_pos ;
      dump_cond !neg_cond Config.cond_neg ;
      ()

(*************************************)
(* Old summary stuff, still useful ? *)
(*************************************)

  let show_summary tnames tests =
    let keys = K.Kind.add tnames tests in
    let module V = Matrix.Build(FmtValidate) in
    let module W =
      Matrix.Build(FmtWitness(struct let oriented = true end)) in
    let r = 
      List.fold_right
	(fun ({is_litmus=il;tests=ts;_} as t) k -> 
          try              
	    let valids = V.extract t keys ts in
	    if il then
	      let witnesses = W.extract t keys ts in
              valids :: witnesses :: k
            else
              valids :: k
          with Misc.Fatal msg ->
            Warn.fatal "Summary %s: %s" t.name msg
	)
	tests [] in

    dump tests "Summary" false 
      (List.map (fun t ->
	let basename = pp_name t.name in
	let name = try Filename.chop_extension basename with Invalid_argument _ -> basename in
	(if t.is_litmus then 2 else 1),name) tests) []
      keys
      ~col2:(K.Kind.pps keys) r


  let show do_show logs =    
    let do_show = if do_show = "" then "d" else do_show in
    let tests = read_logs logs in

    let tnames = build_tnames tests in
(* Get rid of this inconsistency, some day ? *)
    if show_litmus_summary then show_summary tnames tests ;

    let t = Hashtbl.create 17 in
    let add c f = Hashtbl.add t c f in
    add 'v' (show_validate tnames) ;
    add 'V' (show_validate_verbose tnames) ;
    add 'r' show_revalidate ;
    add 'X' (show_XYZ X) ;
    add 'Y' (show_XYZ Y) ;
    add 'Z' (show_XYZ Z) ;
    add 'E' show_E ;
    add 'w' (show_wits false tnames) ;
    add 'W' (show_wits true tnames) ;
    add 's' (show_states tnames) ;
    add 'S' show_states_fst ;
    add 'd' show_diffs ;
    add 't' (show_time tnames) ;
    add 'p' (show_prod tnames) ;
    for k = 0 to String.length do_show - 1 do
      try Hashtbl.find t do_show.[k] tests
      with Not_found ->
        Warn.warn_always "cannot show '%c'" do_show.[k]
    done ;
    ()

end


let () =
try
    let module S = Make(Config) in
    S.show runopts.do_show (List.rev !logs) ; exit 0
  with Misc.Fatal msg ->
    eprintf "Fatal error: %s\n%!" msg ; exit 2
