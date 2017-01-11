(****************************************************************************)
(*                          the diy7 toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(***********************************************)
(* Protect memory accesses with locks, linux-C *)
(***********************************************)

open Printf
open Archs

module Action = struct
  type t = Lock | Expand

  let tags = ["lock"; "expand";]

  let parse s = match Misc.lowercase s with
  | "lock" -> Some Lock
  | "expand" -> Some Expand
  | _ -> None

  let pp = function
    | Lock -> "lock"
    | Expand -> "expand"
end

module type Config = sig
  val verbose : int
  val action : Action.t
end

module Top(O:Config)(Out:OutTests.S) = struct

  open Action
  open MiscParser
  open MemOrderOrAnnot
  open CBase

  module D = CDumper.Make(Out)

(**********)
(* Expand *)
(**********)

  let expand_param =
    let open CAst in let open CType in
    fun p -> match p.param_ty with
    | Pointer  (Base "spinlock_t") ->
        { p with param_ty=Pointer (Base "int");}
    | _ -> p

  let expand_params = List.map expand_param


(********)
(* Lock *)
(********)

(* Code *)
  let lock_id x ="lock_" ^ x

  let lock_param  =
    let open CAst in let open CType in
    fun p k ->
      let x = p.param_name in
      let plock =  { param_name=lock_id x ;
                     param_ty=Pointer (Base "spinlock_t"); } in
      p::plock::k

  let lock_params =
    List.map (fun ps -> List.fold_right lock_param ps [])


  let rec expr_read e = match e with
  | Const _|LoadReg _ -> StringSet.empty
  | ECall ("READ_ONCE", [LoadMem (LoadReg x,AN [])]) -> StringSet.singleton x
  | ECall (_,es) -> exprs_read es
  | LoadMem (e,_) -> expr_read e
  | Op (_,e1,e2)
  | Exchange (e1,e2,_)
  | Fetch (e1,_,e2,_)
    -> StringSet.union (expr_read e1) (expr_read e2)
  | ECas (e1,e2,e3,_,_,_) ->
      StringSet.union
        (StringSet.union (expr_read e1) (expr_read e2))
        (expr_read e2)

  and exprs_read es = StringSet.unions (List.map expr_read es)

  let seq = function
    | [i] -> i
    | is -> Seq (is,false)

  let add_locks s i =
    let is =
      StringSet.fold
        (fun x k ->
          let x = lock_id x in
          let addr = LoadMem (LoadReg x,AN []) in
          CBase.Lock (addr,MutexLinux)::k@[Unlock (addr,MutexLinux)])
        s [i] in
    seq is

  let lock_ins =
    let rec tr_ins i = match i with
      | Fence _
      | DeclReg _
      | Symb _
        -> i
      | Lock _
      | Unlock _
        -> Warn.fatal "locking locked test"
      | Seq (is,b) -> Seq (List.map tr_ins is,b)
      | If (ec,it,o) ->
          let s = expr_read ec in
          let i = If (ec,tr_ins it,Misc.app_opt tr_ins o) in
          add_locks s i
      | StoreReg (_,_,e) ->
          let s = expr_read e in
          add_locks s i
      | StoreMem (e1,e2,_) ->
          let s1 = expr_read e1 and s2 = expr_read e2 in
          let s = StringSet.union s1 s2 in
          add_locks s i
      | PCall ("WRITE_ONCE",[LoadMem (LoadReg r,AN []);e]) ->
          let s = expr_read e in
          add_locks (StringSet.add r s) i
      | PCall (_,es) ->
          let s = exprs_read es in
          add_locks s i in
      tr_ins


  let tr_parsed t = match t.extra_data with
  | CExtra pss ->
      let prog =
        List.map
          (fun (i,code) -> i,List.map (CBase.pseudo_map lock_ins) code)
          t.prog in
      let extra_data = CExtra (lock_params pss) in
      { t with extra_data; prog;}
  | NoExtra|BellExtra _ -> assert false

(* Name *)

  let tr_name = match O.action with
  | Action.Lock ->
      fun name -> { name with Name.name = name.Name.name ^ "+locked" ; }
  | Action.Expand ->
      fun name -> name

  let tr_test idx_out name parsed =
    let fname = name.Name.file in
    let base = Filename.basename fname in
    let out = Out.open_file base in
    Misc.output_protect_close Out.close
      (fun out ->
        let name = tr_name name in
        let parsed = tr_parsed parsed in
        D.dump out name parsed ;
        Out.fprintf idx_out "%s\n" base)
      out ;
    ()

  module LexConf  = struct
    let debug = O.verbose > 2
    let check_rename _ = None
  end

  let from_chan idx_out chan splitted =
    match splitted.Splitter.arch with
    | `C ->
        let module C = CBase in
        let module L = struct
          type pseudo = C.pseudo
          type token = CParser.token
          module Lexer = CLexer.Make(LexConf)
          let shallow_lexer = Lexer.token false
          let deep_lexer = Lexer.token true
          let shallow_parser = CParser.shallow_main
          let deep_parser = CParser.deep_main
(* No macros *)
          type macro = unit
          let macros_parser _ _ = assert false
          let macros_expand _ i = i
        end in
        let module P =
          CGenParser_lib.Make(CGenParser_lib.DefaultConfig)(C)(L) in
        let name =  splitted.Splitter.name in
        let parsed = P.parse chan splitted in
        tr_test idx_out name parsed
    | _ -> Warn.fatal "not a C litmus test"

  module SP = Splitter.Make(LexConf)

  let from_arg idx_out name =
    Misc.input_protect
      (fun chan ->
        let (splitted:Splitter.result) = SP.split name chan in
        from_chan idx_out chan splitted)
      name

  let zyva args =
    let idx_out = Out.open_all () in
    Misc.output_protect_close Out.close
      (fun idx_out ->
        Misc.iter_argv
          (fun fname ->
            try from_arg idx_out fname with
            | Misc.Exit -> ()
            | Misc.Fatal msg ->
                Warn.warn_always "%a %s" Pos.pp_pos0 fname msg ;
                ()
            | e ->
                Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 fname ;
                raise e)
          args) idx_out ;
    Out.tar ()

end

let verbose = ref 0
let outputdir = ref None
let action = ref Action.Lock
let args = ref []


let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
   "-o", Arg.String (fun s -> outputdir := Some s),
   "<name>  all output in directory <name>";
   begin let module P = ParseTag.Make(Action) in
   P.parse "-action" action "action performed" end ;
 ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mprog"

let () =
  Arg.parse opts
    (fun s -> args := !args @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)

module X =
  Top
    (struct
      let verbose = !verbose
      let action = !action
    end)

let zyva = match !outputdir with
| None ->
    let module Y = X(OutStd) in
    Y.zyva
| Some _ as d ->
    let module T =
      OutTar.Make
        (struct
	  let verbose = !verbose
	  let outname = d
	end) in
    let module Y = X(T) in
    Y.zyva

let () = zyva !args
