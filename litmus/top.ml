(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(***********************************************)
(* Parse a source file for the needs of litmus *)
(* (And then compile test)                     *)
(***********************************************)

open Answer

module type CommonConfig = sig
  val verbose : int
  val limit : bool
  val timeloop : int
  val stride : int option
  val avail : int option
  val runs : int
  val size : int
  val noccs : int
  val timelimit : float option
  val isync : bool
  val speedcheck : Speedcheck.t
  val safer : Safer.t
  val cautious : bool
  val preload : Preload.t
  val memory : Memory.t
  val alloc : Alloc.t
  val doublealloc : bool
  val launch : Launch.t
  val barrier : Barrier.t
  val linkopt : string
  val logicalprocs : int list option
  val affinity : Affinity.t
  val targetos : TargetOS.t
  val is_out : bool
  val sleep : int
  val driver : Driver.t
  val crossrun : Crossrun.t
  val gcc : string
  val c11 : bool
  val c11_fence : bool
  val ascall : bool
  val stdio : bool
  val xy : bool
  val pldw : bool
  val morearch : MoreArch.t
  val carch : Archs.System.t option
  val syncconst : int
  val numeric_labels : bool
  val kind : bool
  val force_affinity : bool
  val smtmode : Smt.t
  val smt : int
  val nsockets : int
  val contiguous : bool
  val syncmacro : int option
  val collect : Collect.t
  val hexa : bool
  val verbose_barrier : bool
  val verbose_prelude : bool
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
  val check_nstates : string -> int option
  val cross : bool
  val tarname : string
  val hint : string option
  val no : string option
  val index : string option
end

module type TopConfig = sig
  include CommonConfig
  val platform : string
  val check_name : string -> bool
  val check_rename : string -> string option
(* Arch dependent options *)
  val mkopt : Option.opt -> Option.opt
(* Mode *)
  val mode : Mode.t
(* usearch *)
  val usearch : UseArch.t
end

module type Config = sig
  include GenParser.Config
  include Compile.Config
(* Additions for Presi *)
  val line : int
  val noccs : int
  val timelimit : float option
  val check_nstates : string -> int option
(* End of additions *)
  include Skel.Config
  include Run.Config
  val limit : bool
  val sysarch : Archs.System.t
end

module Top (OT:TopConfig) (Tar:Tar.S) : sig
  val from_files : string list -> unit
end = struct

(************************************************************)
(* Some configuration dependent stuff, to be performed once *)
(************************************************************)

(* Avoid cycles *)
  let read_no fname =
    Misc.input_protect
      (fun chan -> MySys.read_list chan (fun s -> Some s))
      fname

  let avoid_cycle =
    let xs = match OT.no with
    | None -> []
    | Some fname -> read_no fname in
    let set = StringSet.of_list xs in
    fun cy -> StringSet.mem cy set

(* hints *)
  let hint = match OT.hint with
  | None -> Hint.empty
  | Some fname -> Hint.read fname

  module W = Warn.Make(OT)


  module Utils (O:Config) (A':Arch.Base)
      (Lang:Language.S
      with type arch_reg = A'.Out.arch_reg
      and type t = A'.Out.t)
      (Pseudo:PseudoAbstract.S) =
    struct
      module T = Test.Make(O)(A')(Pseudo)
      module R = Run.Make(O)(Tar)(T.D)

      let get_cycle t =
        let info = t.MiscParser.info in
        List.assoc "Cycle" info

      let cycle_ok avoid t =
        try
          let cy = get_cycle t in
          not (avoid cy)
        with Not_found -> true


      let hash_ok env tname hash =
        try
          let ohash = StringMap.find tname env in
          if String.compare hash.hash ohash.hash <> 0 then begin
            Warn.user_error "Unconsistent hashes for test %s, previous file %s"
              tname ohash.filename
          end else begin
            if  ohash.filename <> hash.filename then
              W.warn  "Duplicate occurrence of test %s (%s,%s)"
                tname ohash.filename hash.filename
            else
              W.warn "File %s is referenced more then once"
                ohash.filename
          end ;
          false
        with Not_found ->  true

      let change_hint hint name t =
        try
          let more_info = Hint.get hint name in
          let info =
            more_info @
            List.filter
              (fun (k,_) ->
                try
                  let _ = List.assoc k more_info in
                  false
                with Not_found -> true)
              t.MiscParser.info in
          { t with MiscParser.info = info; }
        with Not_found -> t

      let dump source doc compiled =
        Misc.output_protect
          (fun chan ->
            let module Out =
              Indent.Make(struct let hexa = O.hexa let out = chan end) in
            let dump =
              match OT.mode with
              | Mode.Std ->
                  let module S = Skel.Make(O)(Pseudo)(A')(T)(Out)(Lang) in
                  S.dump
              | Mode.PreSi ->
                  let module S = PreSi.Make(O)(Pseudo)(A')(T)(Out)(Lang) in
                  S.dump in
            dump doc compiled)
          (Tar.outname source)

      let limit_ok nprocs = match O.avail with
      | None|Some 0 -> true
      | Some navail -> nprocs <= navail

      let warn_limit name nprocs = match O.avail with
      | None|Some 0 -> ()
      | Some navail ->
          if nprocs > navail then
            Warn.warn_always
              "%stest with more threads (%i) than available (%i) is compiled"
              (Pos.str_pos0 name.Name.file) nprocs navail

      let hash name parsed =
        try
          let hash = List.assoc "Hash" parsed.MiscParser.info in
          { filename=name; hash=hash;}
        with Not_found -> assert false

      let compile
          parse count_procs compile allocate
          cycles hash_env
          name in_chan out_chan splitted =
        try begin
          let parsed = parse in_chan splitted in
          let doc = splitted.Splitter.name in
          let tname = doc.Name.name in
          close_in in_chan ;
          let nprocs = count_procs parsed.MiscParser.prog in
          let hash = hash name parsed in
          let cycle_ok = cycle_ok avoid_cycle parsed
          and hash_ok = hash_ok hash_env tname hash
          and limit_ok = limit_ok nprocs in
          if
            cycle_ok && hash_ok && limit_ok
          then begin
            warn_limit doc nprocs ;
            let hash_env = StringMap.add tname hash hash_env in
            let parsed = change_hint hint doc.Name.name parsed in
            let allocated = allocate parsed in
            let compiled = compile allocated in
            let source = MyName.outname name ".c" in
            dump source doc compiled;
            if not OT.is_out then begin
              let _utils =
                let module O = struct
                  include OT
                  let arch = A'.arch
                end in
                let module Obj = ObjUtil.Make(O)(Tar) in
                Obj.dump () in
              ()
            end ;
            R.run name out_chan doc allocated source ;
            Completed (A'.arch,doc,source,cycles,hash_env)
          end else begin
            W.warn "%stest not compiled" (Pos.str_pos0 doc.Name.file) ;
            Absent A'.arch
          end
        end with e -> Interrupted (A'.arch,e)
    end


  module Make
      (O:Config)
      (A:Arch.S)
      (L:GenParser.LexParse with type instruction = A.pseudo)
      (XXXComp : XXXCompile.S with module A = A) =
    struct
      module Pseudo = struct
        type code = int * A.pseudo list
        let rec fmt_io io = match io with
        | A.Nop -> ""
        | A.Instruction ins -> A.dump_instruction ins
        | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
        | A.Macro (f,regs) ->
            Printf.sprintf
              "%s(%s)"
              f
              (String.concat "," (List.map A.pp_reg regs))

        let dump_prog (p,is) = Printf.sprintf "P%i" p::List.map fmt_io is

        let dump_prog_lines prog =
          let pp = List.map dump_prog prog in
          let pp = Misc.lines_of_prog pp in
          List.map (Printf.sprintf "%s;") pp

        let print_prog chan prog =
          let pp = List.map dump_prog prog in
          Misc.pp_prog chan pp
      end

      module Lang = ASMLang.Make(O)(A.I)(A.Out)
      module Utils = Utils(O)(A)(Lang)(Pseudo)
      module P = GenParser.Make(O)(A) (L)
      module Comp = Compile.Make (O)(A)(Utils.T)(XXXComp)

      module AllocArch = struct
        include A 
        type v = A.V.v
        let maybevToV = V.maybevToV
        type global = string
        let maybevToGlobal = A.vToName
      end
      let compile =
        let allocate parsed =
          let module Alloc = SymbReg.Make(AllocArch) in
          Alloc.allocate_regs parsed
        in
        Utils.compile P.parse List.length Comp.compile allocate
    end


  module Make'
      (O:Config)
      (A:sig val comment : char end) =
    struct
      module L = struct
        type token = CParser.token
        module CL = CLexer.Make(struct let debug = false end)
        let lexer = CL.token false
        let parser = CParser.shallow_main
      end
      module A' = struct
        module V =
          struct
            include SymbConstant
            let maybevToV c = c
          end
        type reg = string

        let vToName = function
            | Constant.Concrete i -> "addr_" ^ string_of_int i
            | Constant.Symbolic s -> s

        module Internal = struct
          type arch_reg = reg
          let pp_reg x = x
          let reg_compare = String.compare

          type arch_global = string
          let pp_global x = x
          let global_compare = String.compare

          let arch = `C
        end

        include Location.Make(Internal)

        let parse_reg x = Some x
        let reg_compare = Internal.reg_compare

        type state = (location * V.v) list
        type fullstate = (location * (MiscParser.run_type * V.v)) list

        module Out = struct
          include CTarget
          include OutUtils.Make(O)
        end

        let arch = Internal.arch

        let rec find_in_state loc = function
          | [] -> V.intToV 0
          | (loc2,v)::rem ->
              if location_compare loc loc2 = 0 then v
              else find_in_state loc rem
        let pp_reg x = x
      end

      module Pseudo = DumpCAst

      module Lang =
        CLang.Make
          (struct
            let comment = A.comment
            let memory = O.memory
            let mode = O.mode
          end)
      module Utils = Utils(O)(A')(Lang)(Pseudo)
      module P = CGenParser.Make(O)(Pseudo)(A')(L)
      module Comp = CCompile.Make(O)(Utils.T)

      let rec count_procs = function
        | CAst.Test _::xs -> 1 + count_procs xs
        | CAst.Global _::xs -> count_procs xs
        | [] -> 0

      let compile =
        let allocate parsed =
          let module Alloc = CSymbReg.Make(A') in
          let allocated = Alloc.allocate_regs parsed in
          { allocated with MiscParser.prog = allocated.MiscParser.prog; }
        in
        Utils.compile P.parse count_procs Comp.compile allocate
    end


  let debuglexer =  OT.verbose > 2

  module LexConfig =
    struct
      let debug = debuglexer
      let check_rename = OT.check_rename
    end


  module SP = Splitter.Make(LexConfig)

  let from_chan cycles hash_env name in_chan out_chan =
(* First split the input file in sections *)
    let { Splitter.arch=arch ; _ } as splitted =
      SP.split name in_chan in
    let tname = splitted.Splitter.name.Name.name in
    if OT.check_name tname then begin
(* Then call appropriate compiler, depending upon arch *)
      let module V = SymbConstant in
      let opt = OT.mkopt (Option.get_default arch) in
      let word = Option.get_word opt in
      let module ODep = struct
        let word = word
        let line = Option.get_line opt
        let delay = Option.get_delay opt
        let gccopts = Option.get_gccopts opt
      end in
(* Compile configuration, must also be used to configure arch modules *)
      let module OC = struct
        let word = word
        let syncmacro =OT.syncmacro
        let syncconst = OT.syncconst
        let memory = OT.memory
        let morearch = OT.morearch
        let cautious = OT.cautious
      end in
      let module OX = struct
        include OT
        include ODep
        let debuglexer = debuglexer
        let sysarch = match arch with
        | #Archs.System.t as a -> a
        | `C -> begin match OT.carch with
          | Some a -> a
          | None  -> Warn.fatal "Test %s not performed because -carch is not given but required while using C arch" tname
        end
      end in
      let module Cfg = OX in
      let do_ppc () =             
        let module Arch' = PPCArch.Make(OC)(V) in
        let module LexParse = struct
          type instruction = Arch'.pseudo
          type token = PPCParser.token
          module Lexer = PPCLexer.Make(LexConfig)
          let lexer = Lexer.token
          let parser = PPCParser.main
        end in
        let module Compile = PPCCompile.Make(V)(OC) in
        let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
        X.compile cycles hash_env name in_chan out_chan splitted
      in
      let do_ppcgen () =
        let module Arch' = PPCGenArch.Make(OC)(V) in
        let module LexParse = struct
          type instruction = Arch'.pseudo
          type token = PPCGenParser.token
          module Lexer = PPCGenLexer.Make(LexConfig)
          let lexer = Lexer.token
          let parser = PPCGenParser.main
        end in
            let module Compile = PPCGenCompile.Make(V)(OC) in
            let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
            X.compile cycles hash_env
              name in_chan out_chan splitted
      in
      let aux = function
        | `PPC ->
            begin match OT.usearch with
            | UseArch.Trad -> do_ppc ()
            | UseArch.Gen -> do_ppcgen ()
            end
        | `PPCGen -> do_ppcgen ()
        | `X86 ->
            let module Arch' = X86Arch.Make(OC)(V) in
            let module LexParse = struct
              type instruction = Arch'.pseudo
              type token = X86Parser.token
              module Lexer = X86Lexer.Make(LexConfig)
              let lexer = Lexer.token
              let parser = X86Parser.main
            end in
            let module Compile = X86Compile.Make(V)(OC) in
            let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
            X.compile cycles hash_env
              name in_chan out_chan splitted
        | `ARM ->
            let module Arch' = ARMArch.Make(OC)(V) in
            let module LexParse = struct
              type instruction = Arch'.pseudo
              type token = ARMParser.token
              module Lexer = ARMLexer.Make(LexConfig)
              let lexer = Lexer.token
              let parser = ARMParser.main
            end in
            let module Compile = ARMCompile.Make(V)(OC) in
            let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
            X.compile cycles hash_env
              name in_chan out_chan splitted
        | `MIPS ->
            let module Arch' = MIPSArch.Make(OC)(V) in
            let module LexParse = struct
              type instruction = Arch'.pseudo
              type token = MIPSParser.token
              module Lexer = MIPSLexer.Make(LexConfig)
              let lexer = Lexer.token
              let parser = MIPSParser.main
            end in
            let module Compile = MIPSCompile.Make(V)(OC) in
            let module X = Make(Cfg)(Arch')(LexParse)(Compile) in
            X.compile cycles hash_env
              name in_chan out_chan splitted
        | `C ->
            let module Arch' = struct
              let comment = match OX.sysarch with
              | `PPC -> PPCArch.comment
              | `PPCGen -> PPCGenArch.comment
              | `X86 -> X86Arch.comment
              | `ARM -> ARMArch.comment
              | `MIPS -> MIPSArch.comment
            end in
            let module X = Make'(Cfg)(Arch') in
            X.compile cycles hash_env
              name in_chan out_chan splitted
      in
      aux arch
    end else begin (* Excluded explicitely, (check_tname), do not warn *)
      Absent arch
    end

  let from_file
      cycles hash_env
      name out_chan =
    Misc.input_protect
      (fun in_chan ->
        from_chan cycles hash_env
          name in_chan out_chan) name

(* Call generic tar builder/runner *)
  module DF = DumpRun.Make (OT)(Tar) (struct let from_file = from_file end)

  let from_files = DF.from_files
end
