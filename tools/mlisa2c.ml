(****************************************************************************)
(*                          the diy7 toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                         *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(******************************************)
(* Translate specific LISA to C11/Linux   *)
(******************************************)

(*
  Equivalence of constructs as suggested in
  <http://open-std.org/JTC1/SC22/WG21/docs/papers/2016/p0124r2.html#So%20You%20Want%20Your%20Arch%20To%20Use%20C11%20Atomics>
*)
open Printf

module Action = struct
  type t = C11 | Linux

  let tags = ["c11"; "linux";]

  let parse s = match Misc.lowercase s with
  | "linux" -> Some Linux
  | "c11" -> Some C11
  | _ -> None

  let pp = function
    | Linux -> "linux"
    | C11 -> "c11"
end

module type Config = sig
  val verbose : int
  val action : Action.t
end

module Top(O:Config)(Out:OutTests.S) = struct
  open Action
  open MiscParser
  open MemOrderOrAnnot

  module D = CDumper.Make(Out)

  let dump =  D.dump_withhash

  open BellBase
  open CBase

(* Collect locations *)
  let collect_ra xs = function
    | Abs (Constant.Symbolic ((s,_),_)) -> StringSet.add s xs
    | _ -> xs

  let collect_addr xs = function
    | Addr_op_atom ra
    | Addr_op_add (ra,_) -> collect_ra xs ra

  let collect_ins xs ins = match ins with
  | Pld (_,a,_)
  | Pst (a,_,_) -> collect_addr xs a
  | _ -> xs

  let collect_pseudo xs ins = BellBase.pseudo_fold  collect_ins xs ins

  let collect_code code = List.fold_left collect_pseudo StringSet.empty code

(* Translate to C *)
  let tr_reg r = BellBase.pp_reg r

  let tr_ra = function
    | Rega r -> LoadReg (tr_reg r)
    | Abs (Constant.Symbolic ((s,_),_)) -> LoadReg s
    | _ -> assert false

  let do_tr_addr = function
    | Addr_op_atom ra -> tr_ra ra
    | _ -> assert false

  let tr_addr a = LoadMem(do_tr_addr a,AN [])

  let tr_reg_or_imm = function
    | Imm k -> Const (ParsedConstant.intToV k)
    | Regi r -> LoadReg (tr_reg r)


  let tr_fence = match O.action with
  | C11 ->
      fun f ->
        let open MemOrder in
        let f = match f with
        | "mb" -> SC
        | "wmb" -> Acq_Rel
        | "rmb" -> Acq_Rel
        | _ -> assert false in
        CBase.Fence (MO f)
  | Linux -> fun f -> PCall ("smp_" ^ f,[])

  let tr_load = match O.action with
  | C11 ->
      fun an a ->
        let mo = match an with
        | "once" -> MemOrder.Rlx
        | "acquire" -> MemOrder.Acq
        | _ -> assert false in
        LoadMem (do_tr_addr a,MO mo)
  | Linux ->
      fun an a -> match an with
      | "once" ->
          ECall ("READ_ONCE",[tr_addr a])
      | "acquire" ->
          ECall ("smp_load_acquire",[do_tr_addr a])
      | _ -> assert false

  and tr_store = match O.action with
  |  C11 ->
      fun an a k ->
        let mo = match an with
        | "once" -> MemOrder.Rlx
        | "release" -> MemOrder.Rel
        | _ -> assert false in
        StoreMem (do_tr_addr a,tr_reg_or_imm k,MO mo)
  | Linux ->
      fun an a k ->
        match an with
        | "once" ->
            PCall ("WRITE_ONCE",[tr_addr a;tr_reg_or_imm k;])
        | "release" ->
            PCall ("smp_store_release",[do_tr_addr a;tr_reg_or_imm k;])
        | _ -> assert false

  let tr_an = function
    | [] -> "once"
    | a::_ -> a

  let  tr_ins = function
    | Pld (r,a,(["once"]|[]|["acquire"] as an)) ->
        StoreReg (Some CType.word,tr_reg r,tr_load (tr_an an) a)
    | Pst (a,k,(["once"]|[]|["release"] as an)) -> tr_store (tr_an an) a k
    | Pfence (BellBase.Fence (["mb"|"rmb"|"wmb" as f],_)) -> tr_fence f
    | Pfence (BellBase.Fence (["sync"],None)) when O.action = Linux ->
         PCall ("synchronize_rcu",[])
    | Pfence (BellBase.Fence ([("rcu_read_lock"|"rcu_read_unlock" as f)],None)) when O.action = Linux ->
         PCall (f,[])
    | ins ->
        Warn.user_error
          "Instruction inconnue: %s" (BellBase.dump_instruction ins)



  let rec tr_pseudo tr = function
    | BellBase.Nop -> CBase.Nop
    | BellBase.Instruction i -> CBase.Instruction (tr i)
    | BellBase.Label (lab,i) -> CBase.Label (lab,tr_pseudo tr i)
    | BellBase.Macro (f,es) -> CBase.Macro (f,List.map tr_reg es)
    | BellBase.Symbolic s -> CBase.Symbolic s

  let ptr_type =
    let open CType in
    Pointer
      (match O.action with | C11 -> Base "atomic_int" | Linux -> CType.word)

  let tr_extra prog =
    List.map
      (fun (_,code) ->
        let xs = collect_code code in
        let xs = StringSet.elements xs in
        let open CAst in
        List.map
          (fun x ->  { param_ty=ptr_type; param_name=x;})
          xs)
      prog

  let tr_prog prog =
    List.map
      (fun (i,code) -> i,List.map (tr_pseudo tr_ins) code)
      prog


  let do_tr p =
    { p with prog = tr_prog p.prog; extra_data=CExtra (tr_extra p.prog);}

  let tr_test idx_out name parsed =
    let fname = name.Name.file in
    if O.verbose > 0 then
      eprintf "translating %s\n" fname ;
    let base = Filename.basename fname in
    let out = Out.open_file base in
    Misc.output_protect_close Out.close
      (fun out ->
        let ot = do_tr parsed in
        dump out name ot ;
        Out.fprintf idx_out "%s\n" base)
      out ;
    ()

(***************)
(* Parsing.... *)
(***************)

  module LexConf  = struct
    let debug = O.verbose > 2
    let check_rename _ = None
  end

  let from_chan idx_out chan splitted =
    match splitted.Splitter.arch with
    | `LISA ->
        let module Bell = BellBase in
        let module BellLexParse = struct
          type instruction = Bell.parsedPseudo
          type token = LISAParser.token

          module L = BellLexer.Make(LexConf)
          let lexer = L.token
          let parser = LISAParser.main
        end in
        let module P = GenParser.Make(GenParser.DefaultConfig)(Bell)(BellLexParse) in
        let parsed = P.parse chan splitted in
        let name = splitted.Splitter.name in
        tr_test idx_out name parsed
    | _ -> Warn.fatal "not a LISA litmus test"

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
                Printf.eprintf "Fatal: %a Adios\n" Pos.pp_pos0 fname ;
                raise e)
          args) idx_out ;
    Out.tar ()
end





let verbose = ref 0
let outputdir = ref None
let action = ref Action.Linux
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
