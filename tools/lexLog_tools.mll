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

{

let mask = Int64.sub (Int64.shift_left Int64.one 32) Int64.one
     
module type Config = sig
  val verbose : int
  val rename : string -> string
  val ok : string -> bool
  val hexa : bool
  val int32 : bool
  val acceptBig : bool
end

module Make(O:Config) = struct
open Lexing
open LexMisc
open Printf
open LogState
module LS = LogState.Make(O)

let c_init = 100
let count = ref 0
let poolize loc v = HashedPair.as_hashed loc v
let no_wits = Int64.zero,Int64.zero
let no_topos = []
let no_cond = None
let no_loop = false
let no_hash = None
let no_time = None

let to_dec32 num =
  try Int32.to_string (Int32.of_string num) with _ -> num

let to_hex32 num =
  try
    sprintf "0x%lx" (Int32.of_string num)
  with _ -> try (* Big unsigned... *)
    let n = Int64.of_string num in
    let n32 = Int64.logand n mask in
    if Int64.compare n32 n = 0 then
      sprintf "0x%Lx" n32
    else num
  with _ -> num

let to_dec =
  if O.int32 then to_dec32
  else fun num -> try Int64.to_string (Int64.of_string num) with _ -> num

let to_hex =
  if O.int32 then to_hex32
  else fun num -> try sprintf "0x%Lx" (Int64.of_string num) with _ -> num

let to_xxx = if O.hexa then to_hex else to_dec

let nstates_limit = 1024
}

let digit = [ '0'-'9' ]
let num = digit+
let hexa = ['0'-'9' 'a'-'f' 'A'-'F' ]
let hexanum = "0x" hexa+
let set = '{' (' '|','|('-'?(num|hexanum)))* '}'
let alpha = [ 'a'-'z' 'A'-'Z']
let name = alpha (alpha|digit| '.')*
let loc = name | ('$' (alpha+|digit+))
let blank = [' ' '\t']
let testname  = (alpha|digit|'_' | '/' | '.' | '-' | '+' | '[' | ']')+
let nl = '\n'|"\r\n"
let validation = "Undef"|"Succeeded"|"Failed"|"Ok"|"No"|"??"

rule main  mk islitmus rem = parse
 | "Test" blank+ (testname as t)
    ((blank+ (name as  kind))| ("" as kind)) nl
    { incr_lineno lexbuf ;
      let t = Misc.clean_name t in
      if O.verbose > 0 then begin
        decr count ;
        if !count <= 0 then begin
          eprintf "+%!" ;
          count := c_init
        end
      end ;
      let kind = match LS.parse_kind kind with
      | Some k -> k
      | None -> Warn.fatal "Lexing kind: %s" kind in
      begin match  pstate lexbuf with
      | None -> main  mk islitmus rem lexbuf
      | Some (islitmusst,(nk,st)) ->
          let t = O.rename t in
          if O.ok t && (O.acceptBig || nk < nstates_limit) then
            main  mk (islitmusst || islitmus) (mk (t,kind,st)::rem) lexbuf
          else begin
            if O.verbose > 0 && O.ok t && nk >= nstates_limit then
              eprintf "Test %s ignored for size\n%!" t ;
            main mk islitmus rem lexbuf
          end
      end }
| [^'\r''\n']*  nl  { incr_lineno lexbuf ; main  mk islitmus rem lexbuf }
| eof { islitmus,rem }
| "" { error "main" lexbuf }

and pstate = parse
| "Generated assembler\n"
    { incr_lineno lexbuf ;
      match pstate lexbuf with
      | None -> None
      | Some (_,rem) -> Some (true,rem) }
| ("States" as key) (blank+ (digit+ as _x))?  nl
| ("Histogram" as key)
     (blank+  '(' (digit+ as _x) blank+ "states" blank* ')')?  nl
    { incr_lineno lexbuf ; Some (key="Histogram",plines 0 [] lexbuf ) }
| "Fatal" [':'' ']  [^'\r''\n']*  nl
    { incr_lineno lexbuf ; None }
| [^'\r''\n']*  nl  { incr_lineno lexbuf ; pstate  lexbuf }
| eof { Some (false,(0,([],Run,no_wits,no_cond,no_loop,no_hash,no_topos,no_time))) }
| "" { error "pstate" lexbuf }


and plines nk k = parse
| ((num as c) blank* (":>"|"*>"))?
    { if O.acceptBig || nk <= nstates_limit then
        let line = pline [] lexbuf in
        let st =
          { p_noccs =
            begin match c with
            | None -> Int64.one
            | Some s -> Int64.of_string s
            end ;
            p_st = LS.as_st_concrete line } in
        plines (nk+1) (st::k) lexbuf
    else begin
      skip_pline lexbuf ;
      plines (nk+1) k lexbuf
    end }
|  ("Loop" blank+ as loop)?
   (((validation as ok) ([^'\r''\n']*))
|("" as ok))  nl  (* missing validation result, from some litmus logs *)
    { incr_lineno lexbuf ;
      let ok = match ok with
      | "Succeeded"|"Ok" -> Ok
      | "Failed"|"No" -> No
      | "Undefined" -> Undef
      | _ -> DontKnow
      and loop = match loop with Some _ -> true | None -> false in
      let wits = pwitnesses lexbuf in
      let cond = pcond lexbuf in
      skip_empty_lines lexbuf ;
      let hash,topos,time = phash (no_hash,no_topos,no_time) lexbuf in
      (nk,(k,ok,wits,cond,loop,hash,topos,time)) }
| eof { nk,(k,Run,no_wits,no_cond,no_loop,no_hash,no_topos,no_time) }

and skip_empty_lines = parse
| blank*  nl  { incr_lineno lexbuf ; skip_empty_lines lexbuf }
| "" { () }

and pline k = parse
| blank*
 ((num ':' loc as loc)|(('['?) (loc as loc) ( ']'?))|(loc '[' num ']' as loc))
    blank* '=' blank* (('-' ? (num|hexanum))|name|set as v)
    blank* ';'
    {
     let v = to_xxx v in  (* Translate to decimal *)
     let p = poolize loc v in
     pline (p::k) lexbuf }
| blank* ('#' [^'\n']*)?  nl  { incr_lineno lexbuf ; k }
| "" { error "pline" lexbuf }

and skip_pline = parse
| blank*
 ((num ':' loc)|(('['?) (loc) ( ']'?))|(loc '[' num ']'))
    blank* '=' blank* (('-' ? (num|hexanum))|name|set)
    blank* ';'
    { skip_pline lexbuf }
| blank* ('#' [^'\n']*)?  nl  { incr_lineno lexbuf }
| "" { error "skip_pline" lexbuf }

and pwitnesses = parse
 blank*  nl  { incr_lineno lexbuf ; pwitnesses lexbuf }
| "Witnesses" blank*  nl
 "Positive:" blank* (num as pos) ','? blank*
 "Negative:" blank* (num as neg) blank*
 nl
{ incr_lineno lexbuf ; incr_lineno lexbuf ;
  Int64.of_string pos, Int64.of_string neg }
| "" { no_wits }

and pcond = parse
| blank*  nl
| "Bad executions" [^'\n''\r']* nl
  { incr_lineno lexbuf ; pcond lexbuf }
| "Condition" blank+
  ([^'\r''\n']+ as c)  nl
 {  incr_lineno lexbuf ;
    strip_end_cond (Buffer.create 10) (Lexing.from_string c) }
| "" { no_cond }

and strip_end_cond buff = parse
  blank+ "is" _* "validated"
| eof
  { let lxm = Buffer.contents buff in
    let c = LogConstr.parse lxm in
    c }
| _ as c { Buffer.add_char buff c ; strip_end_cond buff lexbuf }

and phash st = parse
| "Hash" blank* '=' blank* ([^' ''\t''\n''\r']+ as hash) blank*  nl
  { let _,p_topos,p_time = st in
   incr_lineno lexbuf ; phash (Some hash,p_topos,p_time) lexbuf }
| "Time" blank+ testname blank+ (num '.' num as t) blank*  nl
  { let p_hash,p_topos,_ = st in
    incr_lineno lexbuf ; phash (p_hash,p_topos,Some (float_of_string t)) lexbuf }
| "Topology" blank+ (num as n) blank* ":>" blank*
  ([^' ''\t''\n''\r']+ as topo) blank* nl
  { let p_hash,p_topo,p_time = st in
    let n = try Int64.of_string n with _ -> assert false in
    let topo = HashedString.as_hashed topo in
    phash (p_hash,(topo,n)::p_topo,p_time) lexbuf  }
| (alpha+ as key) blank* '=' [^'\r''\n']*  nl
| (alpha+ as key)  blank+ testname blank+ [^'\r''\n']*  nl
  { ignore(key) ; incr_lineno lexbuf ; phash  st lexbuf }
| ""
  { st }

and main_simple  mk islitmus rem = parse
| "Test" blank+ (testname as t)
    ((blank+ name)| "") nl
    { incr_lineno lexbuf ;
      if O.verbose > 0 then begin
        decr count ;
        if !count <= 0 then begin
          eprintf "+%!" ;
          count := c_init
        end
      end ;
      let t = Misc.clean_name t in
      begin match sstate lexbuf with
      | None -> main_simple  mk islitmus rem lexbuf
      | Some (islitmusst,st) ->
          let t = O.rename t in
          if O.ok t then
            main_simple  mk (islitmusst || islitmus) (mk (t,st)::rem) lexbuf
          else begin
            main_simple  mk islitmus rem lexbuf
          end
      end }
| [^'\r''\n']*  nl  { incr_lineno lexbuf ; main_simple  mk islitmus rem lexbuf }
| eof { islitmus,rem }
| "" { error "main_simple" lexbuf }

and sstate = parse
| "Generated assembler\n"
    { incr_lineno lexbuf ;
      match sstate lexbuf with
      | None -> None
      | Some (_,rem) -> Some (true,rem) }
| ("States" as key) (blank+ (digit+ as _x))?  nl
| ("Histogram" as key)
     (blank+  '(' (digit+ as _x) blank+ "states" blank* ')')?  nl
    { incr_lineno lexbuf ; Some (key="Histogram",slines [] lexbuf ) }
| "Fatal" [':'' ']  [^'\r''\n']*  nl
    { incr_lineno lexbuf ; None }
| [^'\r''\n']*  nl  { incr_lineno lexbuf ; sstate  lexbuf }
| eof { Some (false,([],no_hash)) }
| "" { error "pstate" lexbuf }


and slines k = parse
| ((num) blank* (":>"|"*>"))?
    { let line = pline [] lexbuf in
      let st = LS.as_st_concrete line in
      slines (st::k) lexbuf }
|  ("Loop" blank+ )?
   ((validation ([^'\r''\n']*))
   |("")  nl ) (* missing validation result, from some litmus logs *)
    { incr_lineno lexbuf ;
      let hash = shash lexbuf in
      (k,hash) }
| eof { (k,no_hash) }

and shash = parse
| "Hash" blank* '=' blank* ([^' ''\t''\r''\n']+ as hash) blank*  nl
  { incr_lineno lexbuf ; Some hash }
| [^'\r''\n']*  nl  {  incr_lineno lexbuf ; shash lexbuf }
| eof { no_hash }

{

let zyva main mk name lexbuf =
  try
    lexbuf.lex_curr_p <-
      {pos_fname = name; pos_lnum = 1;
       pos_bol = 0; pos_cnum = 0};
    count := c_init ;
    main mk false [] lexbuf
  with
  | LexMisc.Error (msg,loc) ->
        Printf.eprintf "%a: Lex error %s\n"
          Pos.pp_pos loc msg ;
        raise Misc.Exit

let do_read_chan main mk name chan =
  zyva main mk name (Lexing.from_channel chan)

let read_name main normalize mk name k =
  if O.verbose > 0 then
    eprintf "Reading file: %s\n%!" name ;
  try
    let (is_litmus,r) =
      Misc.input_protect
        (do_read_chan main mk name)
        name in
    if O.verbose > 0 then
      eprintf
        "Found %i tests in log (which is of type %s)\n%!"
        (List.length r)
        (if is_litmus then "litmus log" else "memevents log");
    match r with
    | [] -> k
    | _::_ -> normalize name is_litmus r::k
  with
  | Misc.Fatal msg ->
      eprintf "Fatal error will not be fatal after all: %s\n" msg ;
      k
  | Misc.Exit -> k

let do_read_names main norm mk names =
  List.fold_right (read_name main norm mk) names []

let full_log log = log

let read_chan name chan =
  let normalize = LS.normalize and mk = full_log in
  let is_litmus,r =  do_read_chan main mk name chan in
  if O.verbose > 0 then
    eprintf
      "Found %i tests in log (which is of type %s)\n%!"
      (List.length r)
      (if is_litmus then "litmus log" else "memevents log");
  normalize name is_litmus r

let read_name name = Misc.input_protect (read_chan name) name

let read_names names =  do_read_names main LS.normalize full_log names

let simplify (t,(sts,hash)) = (t,sts,hash)

let read_names_simple names =
  do_read_names main_simple LS.normalize_simple simplify names
end
}
