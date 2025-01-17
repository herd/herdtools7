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

module Action = struct
  type t = Lock | Expand | Once

  let tags = ["lock"; "expand"; "once";]

  let parse s = match Misc.lowercase s with
  | "lock" -> Some Lock
  | "expand" -> Some Expand
  | "once" -> Some Once
  | _ -> None

  let pp = function
    | Lock -> "lock"
    | Expand -> "expand"
    | Once -> "once"
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

  let dump = match O.action with
  | Expand -> D.dump_withhash
  | Action.Lock | Once -> D.dump

  let changed = ref false

  exception NotChanged

  let not_changed name =
    if O.verbose > 0 then
      Warn.fatal "test %s unchanged, no output" name.Name.name
    else
      raise NotChanged

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

  let const_one = ParsedConstant.intToV 1
  and const_zero = ParsedConstant.intToV 0

  let expand_pseudo_code =

    let rec tr_expr e = match e with
    | Const _|LoadReg _ -> e
    | ECall("spin_trylock",[e])
    | TryLock (e,_) ->
        let e = tr_expr e in
        Op
          (Op.Eq,ECall ("xchg_acquire",[e;Const const_one]),Const const_zero)
    | IsLocked (e,_) ->
        let e = tr_expr e in
        LoadMem (e,AN [])
    | LoadMem (e,a) -> LoadMem(tr_expr e,a)
    | Op (op,e1,e2) -> Op (op,tr_expr e1,tr_expr e2)
    | Exchange (e1,e2,a) ->  Exchange (tr_expr e1,tr_expr e2,a)
    | Fetch (e1,op,e2,a) -> Fetch (tr_expr e1,op,tr_expr e2,a)
    | ECall (f,es) -> ECall (f,List.map tr_expr es)
    | ECas (e1,e2,e3,m1,m2,b) ->
        ECas (tr_expr e1,tr_expr e2,tr_expr e3,m1,m2,b)
    | CmpExchange (loc,o,n,a) ->
        CmpExchange (tr_expr loc,tr_expr o,tr_expr n,a)
    | AtomicOpReturn (loc,op,u,ret,a) ->
        AtomicOpReturn (tr_expr loc,op,tr_expr u,ret,a)
    | AtomicAddUnless (loc,e,u,retbool,a) ->
        AtomicAddUnless (tr_expr loc,tr_expr e,tr_expr u,retbool,a)
    | ExpSRCU(eloc,a) ->
        ExpSRCU(tr_expr eloc,a) in

  let rec tr_ins nxt i = match i with
    | Lock (e,_)|PCall ("spin_lock",[e]) ->
        let e = tr_expr e in
        let v = sprintf "d%i" nxt in
        nxt+1,StringSet.singleton v,
        StoreReg
          (None,Some v,ECall ("xchg_acquire",[e;Const const_one]))
    | Unlock (e,_)|PCall ("spin_unlock",[e]) ->
        let e = tr_expr e in
        nxt,StringSet.empty,
        PCall ("smp_store_release",[e;Const const_zero]);
    | Fence _
    | DeclReg _
    | Symb _
      -> nxt,StringSet.empty,i
    | CastExpr e ->
        nxt,StringSet.empty,CastExpr (tr_expr e)
    | StoreReg (t,r,e) ->
        nxt,StringSet.empty,StoreReg (t,r,tr_expr e)
    | StoreMem (e1,e2,m) ->
         nxt,StringSet.empty,StoreMem(tr_expr e1,tr_expr e2,m)
    | PCall (f,es) ->
        nxt,StringSet.empty,PCall (f,List.map tr_expr es)
    | Seq (is,b) ->
        let nxt,vs,is = tr_code nxt is in
        nxt,vs,Seq (is,b)
    | If (ec,it,o) ->
        let ec = tr_expr ec in
        let nxt,vs,it = tr_ins nxt it in
        let nxt,vs,o = match o with
        | None ->  nxt,vs,o
        | Some f ->
            let nxt,vs,f = tr_ins nxt f in
            nxt,vs,Some f in
        nxt,vs,If (ec,it,o)
    | While (ec,it,n) ->
        let ec = tr_expr ec in
        let nxt,vs,it = tr_ins nxt it in
        nxt,vs,While (ec,it,n)
    | AtomicOp (loc,op,e,a) ->
        let loc = tr_expr loc
        and e = tr_expr e in
        nxt,StringSet.empty,AtomicOp(loc,op,e,a)
    | InstrSRCU(eloc,a,oe) ->
        let eloc = tr_expr eloc in
        let oe = Misc.app_opt tr_expr oe in
        nxt,StringSet.empty,InstrSRCU(eloc,a,oe)

  and tr_code nxt = function
      | [] -> nxt,StringSet.empty,[]
      | i::is ->
          let nxt,vs,i = tr_ins nxt i in
          let nxt,ws,is = tr_code nxt is in
          nxt,StringSet.union vs ws,i::is in

    let rec tr_pseudo nxt p = match p with
    | Nop|Symbolic _|Macro _ -> nxt,StringSet.empty,p
    | Instruction i ->
        let nxt,vs,i = tr_ins nxt i in
        nxt,vs,Instruction i
    | Label (l,i) ->
        let nxt,vs,i = tr_pseudo nxt i in
        nxt,vs,Label (l,i)
    | Pagealign | Skip _ -> assert false

    and tr_pseudo_code nxt = function
      | [] -> StringSet.empty,[]
      | p::ps ->
          let nxt,vs,p =  tr_pseudo nxt p in
          let ws,ps = tr_pseudo_code nxt ps in
          StringSet.union vs ws,p::ps in

    fun code ->
      let vs,code = tr_pseudo_code 0 code in
      if not (StringSet.is_empty vs) then begin
        changed := true ;
        let open CType in
        let code =
          Instruction
            (Seq (StringSet.fold (fun v k -> DeclReg (Base "int",v)::k) vs [],false))::
          code in
        vs,code
      end else
        vs,code


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
  | ECall ("READ_ONCE", [LoadMem (LoadReg x,AN [])]) ->
      changed := true ; StringSet.singleton x
  | ECall (_,es) -> exprs_read es
  | LoadMem (e,_)|TryLock (e,_)|IsLocked (e,_)|ExpSRCU(e,_) -> expr_read e
  | Op (_,e1,e2)
  | Exchange (e1,e2,_)
  | Fetch (e1,_,e2,_)
  | AtomicOpReturn (e1,_,e2,_,_)
    -> StringSet.union (expr_read e1) (expr_read e2)
  | ECas (e1,e2,e3,_,_,_)
  | CmpExchange(e1,e2,e3,_)
  | AtomicAddUnless (e1,e2,e3,_,_) ->
      StringSet.union3 (expr_read e1) (expr_read e2) (expr_read e3)

  and exprs_read es = StringSet.unions (List.map expr_read es)

  let seq = function
    | [i] -> i
    | is -> Seq (is,false)

  let add_locks s i =
    let is =
      StringSet.fold
        (fun x k ->
          let x = lock_id x in
          let addr = LoadReg x in
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
    | PCall (("spin_lock"|"spin_unlock"),_)
      -> Warn.fatal "locking locked test"
    | Seq (is,b) -> Seq (List.map tr_ins is,b)
    | If (ec,it,o) ->
        let s = expr_read ec in
        let i = If (ec,tr_ins it,Misc.app_opt tr_ins o) in
        add_locks s i
    | While (ec,it,n) ->
        let s = expr_read ec in
        let i = While (ec,tr_ins it,n) in
        add_locks s i
    | CastExpr e
    | StoreReg (_,_,e) ->
        let s = expr_read e in
        add_locks s i
    |InstrSRCU(eloc,_,oe) ->
        let s1 = expr_read eloc
        and s2 = Misc.app_opt_def StringSet.empty expr_read oe in
        let s = StringSet.union s1 s2 in
        add_locks s i
    | StoreMem (e1,e2,_) ->
        let s1 = expr_read e1 and s2 = expr_read e2 in
        let s = StringSet.union s1 s2 in
        add_locks s i
    | PCall ("WRITE_ONCE",[LoadMem (LoadReg r,AN []);e]) ->
        changed := true ;
        let s = expr_read e in
        add_locks (StringSet.add r s) i
    | PCall (_,es) ->
        let s = exprs_read es in
        add_locks s i
    | AtomicOp (loc,_,e,_) ->
        let s1 = expr_read loc and s2 = expr_read e in
        let s = StringSet.union s1 s2 in
        add_locks s i in
    tr_ins

  let opt_locks =

    let rec no_seq i k = match i with
    | Seq (is,false) ->
        List.fold_right no_seq is k
    | _ -> Instruction i::k in

    let rec upcode = function
      | [] -> []
      | Instruction i::rem ->
          no_seq i (upcode rem)
      | p::rem -> p::upcode rem in

    let rec peep = function
      | [] -> []
      | Instruction (CBase.Unlock (LoadReg r1,MutexLinux))::
        Instruction (CBase.Lock (LoadReg r2,MutexLinux))::k when r1=r2 ->
          peep k
      | i::k -> i::peep k in


    fun p ->
      let prog = List.map (fun (i,ps) -> i,peep (upcode ps)) p.prog in
      { p with prog; }



(********)
(* Once *)
(********)

  let once_ins =

    let rec tr_expr e = match e with
    | Const _ | LoadReg _ -> e
    | LoadMem (LoadReg _,AN []) -> changed := true ; ECall ("READ_ONCE",[e])
    | LoadMem (e,a) -> LoadMem (tr_expr e,a)
    | Op (op,e1,e2) -> Op (op,tr_expr e1,tr_expr e2)
    | Exchange  (e1,e2,a) -> Exchange (tr_expr e1,tr_expr e2,a)
    | Fetch (e1,op,e2,a) -> Fetch (tr_expr e1,op,tr_expr e2,a)
    | ECall (f,es) -> ECall (f,tr_exprs es)
    | ECas (e1,e2,e3,a1,a2,b) ->
        ECas (tr_expr e1,tr_expr e2,tr_expr e3,a1,a2,b)
    | TryLock (e,m) -> TryLock (tr_expr e,m)
    | IsLocked (e,m) -> IsLocked (tr_expr e,m)
    | CmpExchange (loc,o,n,a) ->
        CmpExchange (tr_expr loc,tr_expr o,tr_expr n,a)
    | AtomicOpReturn (loc,op,e,ret,a) ->
        AtomicOpReturn (tr_expr loc,op,tr_expr e,ret,a)
    | AtomicAddUnless (loc,e,u,retbool,a) ->
        AtomicAddUnless (tr_expr loc,tr_expr e,tr_expr u,retbool,a)
    | ExpSRCU(e,a) ->
        ExpSRCU(tr_expr e,a)

    and tr_exprs es =  List.map tr_expr es in

    let rec tr_ins i = match i with
    | Fence _
    | DeclReg _
    | Symb _
      -> i
    | Lock (e,a) -> Lock (tr_expr e,a)
    | Unlock (e,a) -> Unlock (tr_expr e,a)
    | Seq (is,b) -> Seq (List.map tr_ins is,b)
    | If (ec,it,o) ->
        let ec = tr_expr ec
        and it = tr_ins it
        and o = Misc.app_opt tr_ins o in
        If (ec,it,o)
    | While (ec,it,n) ->
        let ec = tr_expr ec
        and it = tr_ins it in
        While (ec,it,n)
    | CastExpr e ->
        CastExpr (tr_expr e)
    | StoreReg (t,r,e) ->
        StoreReg (t,r,tr_expr e)
    | StoreMem (e1,e2,a) ->
        let e1 = tr_expr e1 and e2 = tr_expr e2 in
        begin match e1,a with
        | LoadReg _,AN [] ->
            changed := true ;
            PCall ("WRITE_ONCE",[LoadMem (e1,AN []);e2])
        | _ -> StoreMem (e1,e2,a)
        end
    | PCall (f,es) -> PCall (f,tr_exprs es)
    | AtomicOp (loc,op,e,a) -> AtomicOp(tr_expr loc,op,tr_expr e,a)
    | InstrSRCU(eloc,a,oe) -> InstrSRCU(tr_expr eloc,a,Misc.app_opt tr_expr oe) in
    tr_ins

(* Parsed *)

  let tr_params = match O.action with
  | Action.Lock  -> lock_params
  | Expand -> List.map expand_params
  | Once -> fun pss -> pss

  let tr_extra_data =
    List.map
      (function
        | CExtra pss -> CExtra (tr_params pss)
        | BellExtra _ as data -> data)

  let tr_parsed tr_ins name t =
    let tr_prog prog =
     changed := false ;
     let prog =
       List.map
        (fun (i,code) -> i,List.map (CBase.pseudo_map tr_ins) code)
        prog in
     if not !changed then not_changed name ;
    prog in
    { t with prog = tr_prog t.prog; extra_data = tr_extra_data t.extra_data; }

(* Name *)
  let tr_name0 = match O.action with
  | Action.Lock ->
       fun name -> name ^ "+locked"
  | Action.Expand ->
      fun name -> name
  | Action.Once ->
       fun name -> name ^ "+onces"

  let tr_name name = { name with Name.name = tr_name0 name.Name.name ; }

(***************)
(*    Test     *)
(***************)

  let tr_test idx_out name parsed =
    let fname = name.Name.file in
    let base = Filename.basename fname in
    let base =
      try Filename.chop_extension base
      with Invalid_argument _ -> assert false in
    let base = sprintf "%s.litmus" (tr_name0 base) in
    let out = Out.open_file base in
    try
      Misc.output_protect_close Out.close
        (fun out ->
          let name = tr_name name in
          let parsed = match O.action with
            | Action.Lock ->
               opt_locks (tr_parsed lock_ins name parsed)
            | Once ->
               tr_parsed once_ins name parsed
            | Expand ->
               let extra_data = tr_extra_data parsed.extra_data in
               changed := false ;
               let prog =
                 List.map
                   (fun ((i,_,_) as proc,ps) ->
                     let vs,ps = expand_pseudo_code ps in
                     StringSet.fold (fun v k -> Location_reg (i,v)::k) vs [],
                     (proc,ps))
                   parsed.prog in
               if not !changed then not_changed name ;
               let locss,prog = List.split prog in
               let filter =
                 let open ConstrGen in
                 let old =
                   match parsed.filter with
                   | Some p -> fun q -> And [q;p]
                   | None -> Misc.identity
                 and q =
                   And
                     (List.map
                        (fun vs ->
                          And
                            (List.map
                               (fun loc -> Atom (LV (Loc loc,const_zero)))
                               vs))
                        locss) in
                 old q in
               { parsed with prog;
                             extra_data;
                             filter=Some filter;} in
          dump out name parsed ;
          Out.fprintf idx_out "%s\n" base)
        out ;
      ()
    with
    | NotChanged ->
       Out.remove base
    | e ->
       Out.remove base ;
       raise e

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
        Misc.iter_argv_or_stdin
          (fun fname ->
            try from_arg idx_out fname with
            | Misc.Exit -> ()
            | Misc.Fatal msg|Misc.UserError msg ->
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
  else "mlock"

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
