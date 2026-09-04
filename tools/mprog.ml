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

(* Pretty print tests *)

open Printf

type input =
  | File of string
  | Stdin

let stdin_name = "stdin.litmus"

let input_name = function
  | File name -> name
  | Stdin -> stdin_name


module Top
    (O:
       sig
         val verbose : int
         val withindex : bool
         val ascommands : bool
         val texmacros : bool
         val hexa : bool
         val compat : bool
         val outputdir : string option
         val mode : OutMode.t
         val transpose : bool
         val alloc : bool
         val set_hash : bool
       end) =
  struct

    let () = ignore O.verbose

    module T = struct
      type t = unit
    end

    (* Transpose dump *)
    module Transpose(A:ArchBase.S)(Pte:PteVal.S)(AddrReg:AddrReg.S) = struct

      module D =
        TransposeDumper.Make
          (struct
            module A = A
            type prog =  (MiscParser.proc * A.pseudo list) list

            type v = ParsedConstant.v
            let dump_v = ParsedConstant.pp_v

            let dump_loc = MiscParser.dump_location
            let dump_reg r = r

            let dump_state_atom dump_loc a =
              MiscParser.dump_state_atom MiscParser.is_global dump_loc dump_v a

            type state = MiscParser.state

            type fault_type = MiscParser.fault_type
            let dump_fault_type = MiscParser.dump_fault_type

            let add_loc v k =
              MiscParser.LocSet.add (MiscParser.Location_global v) k

            let rec get_addrs_ins k = function
              | A.Nop -> k
              | A.Label (_,i) -> get_addrs_ins k i
              | A.Instruction i ->
                  A.fold_addrs add_loc k i
              | A.Symbolic _
              | A.Macro _ -> assert false
              | A.Pagealign | A.Skip _ -> assert false


            let dump_global_state prog st =
              let global_st =
                List.filter (fun (loc,_) -> MiscParser.is_global loc) st in
(* Compute global location referenced from code and init *)
              let gs =
                List.fold_left
                  (fun k (_,code) ->
                    List.fold_left get_addrs_ins k code)
                  MiscParser.LocSet.empty prog in
              let gs =
                List.fold_left
                  (fun k (_,(_,v)) -> match v with
                  | Constant.Symbolic _ as loc -> add_loc loc k
                  | _ -> k) gs st in

              let zeros =
                MiscParser.LocSet.fold
                  (fun loc k ->
                    if
                      List.exists
                        (fun (loc0,_) ->
                          MiscParser.location_compare loc loc0 = 0)
                        global_st
                    then k
                    else (loc,(TestType.TyDef,ParsedConstant.intToV 0))::k)
                  gs [] in
              let st = global_st @ zeros in
              String.concat " "
                (List.map
                   (fun a ->
                     sprintf "%s;"
                       (dump_state_atom dump_loc a))
                   st)

            let ignore_reg _r () = ()
            let collect_sym = StringSet.add
            let collect_regs = ignore_reg,collect_sym

            let collect_ins syms i =
              let _,syms =
                A.pseudo_fold
                  (A.fold_regs collect_regs)
                  ((),syms) i in
              syms

            let collect_code code =
              List.fold_left collect_ins StringSet.empty code

            let dump_proc_state p code st =
              let syms = collect_code code in
              let st =
                List.fold_right
                  (fun (loc,v) k ->
                    match MiscParser.as_local_proc p syms loc with
                    | Some reg -> (reg,v)::k
                    | None -> k)
                  st [] in
              match st with
              | [] -> None
              | _ ->
                 let pp =
                   String.concat " "
                     (List.map
                        (fun a ->
                          MiscParser.dump_state_atom
                            (fun _ -> false)
                            dump_reg dump_v a)
                        st) in
                 Some pp

            type prop = MiscParser.prop

            let dump_atom a =
              ConstrGen.dump_atom
                dump_loc MiscParser.dump_location_brk ParsedConstant.pp_v MiscParser.dump_fault_type
                a

            let dump_prop = ConstrGen.prop_to_string dump_atom
            let dump_constr = ConstrGen.constraints_to_string dump_atom

            type location = MiscParser.location
            let dump_location = dump_loc
          end)

      let dump = D.dump (* Or D.dump_info *)
      let zyva = match O.outputdir with
      | None -> dump stdout
      | Some d ->
          fun name parsed ->
            let fname = name.Name.file in
            let fname = Filename.basename fname in
            let fname = Filename.concat d fname in
            Misc.output_protect
              (fun chan -> dump chan name parsed)
              fname

    end

    (*************)
    (* Text dump *)
    (*************)

    (* No alloc *)
    module Text(A:ArchBase.S)(Pte:PteVal.S)(AddrReg:AddrReg.S) = struct
      module D = DumperMiscParser.Make(O)(A)

      let zyva = match O.outputdir with
      | None -> D.dump_info stdout
      | Some d ->
          fun name parsed ->
            let fname = name.Name.file in
            let fname = Filename.basename fname in
            let fname = Filename.concat d fname in
            Misc.output_protect
              (fun chan -> D.dump_info chan name parsed)
              fname

    end

    (* Some alloc *)
    module TextAlloc(A:ArchBase.S)(Pte:PteVal.S)(AddrReg:AddrReg.S) = struct
      module Arch = ArchExtra_tools.Make(O)(A)(Pte)(AddrReg)
      module Alloc = SymbReg.Make(Arch)
      module D = Dumper.Make(Arch)

      let zyva = match O.outputdir with
      | None ->
          fun name parsed ->
            D.dump_info stdout name (Alloc.allocate_regs parsed)
      | Some d ->
          fun name parsed ->
            let fname = name.Name.file in
            let fname = Filename.basename fname in
            let fname = Filename.concat d fname in
            Misc.output_protect
              (fun chan ->
                D.dump_info chan name (Alloc.allocate_regs parsed))
              fname

    end

    module Latex(A:ArchBase.S)(Pte:PteVal.S)(AddrReg:AddrReg.S) = struct
      module Arch = ArchExtra_tools.Make(O)(A)(Pte)(AddrReg)
      module M = PrettyProg.Make(O)(Arch)
      module Alloc = SymbReg.Make(Arch)

      let zyva name (parsed : A.pseudo MiscParser.t) =
        let parsed = Alloc.allocate_regs parsed in
        M.dump_prog name parsed

    end

    module TPT =
      ToolParse.Top
        (struct
          include ToolParse.DefaultConfig

          let verbose = O.verbose

          let hash =
            let open HashInfo in
            if O.set_hash then Std else NoOp
        end)
        (T)

    open OutMode

    let from_input from_file from_string = function
      | File name -> from_file name
      | Stdin ->
          from_string
            ~filename:stdin_name ~contents:(In_channel.input_all stdin)

    let zyva =
      if O.transpose then
        let module Z = TPT(Transpose) in
        from_input Z.from_file Z.from_string
      else match O.mode with
      | Txt ->
          if O.alloc then
            let module Z =  TPT(TextAlloc) in
            from_input Z.from_file Z.from_string
          else
            let module Z =  TPT(Text) in
            from_input Z.from_file Z.from_string
      | LaTeX|HeVeA|HeVeANew ->
          let module Z =  TPT(Latex) in
          from_input Z.from_file Z.from_string

  end

(***********************)
let args = ref []
let verbose = ref 0
let texmacros = ref false
let hexa = ref false
let compat = ref false
let outputdir = ref None
let mode = ref OutMode.LaTeX
let transpose = ref false
let alloc = ref false
let set_hash = ref false
let stdin_seen = ref false

let add_input input = args := input :: !args

let add_stdin () =
  if !stdin_seen then
    raise (Arg.Bad "standard input ('-') may be specified at most once")
  else begin
    stdin_seen := true;
    add_input Stdin
  end

(* Util for creating boolean arguments. *)
let arg_set_bool arg_ref = Arg.Bool (fun b -> arg_ref := b)

let opts =
  [
   "-",Arg.Unit add_stdin, " read one litmus test from standard input";
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
   "-texmacros", arg_set_bool texmacros,
   (sprintf "<bool> use latex macros in output, default %b" !texmacros);
   "-hexa", arg_set_bool hexa,
   (sprintf "<bool> hexadecimal output, default %b" !hexa);
   "-compat", arg_set_bool compat,
   (sprintf "<bool> backward compatible output (used for hashes), default %b" !hexa);
   begin let module P = ParseTag.Make(OutMode) in
   P.parse "-mode" mode "output mode" end ;
   "-transpose", arg_set_bool transpose,
   (sprintf "<bool> show code proc by proc, default %b" !transpose);
   "-alloc", arg_set_bool alloc,
   (sprintf "<bool> alloc symbolic registers (text mode only), default %b" !alloc);
   ("-o", Arg.String (fun s -> outputdir := Some s),
   "<name>  all output in directory <name>");
   ("-set-hash", arg_set_bool set_hash, (sprintf "<bool> add hashes to litmus tests, default %b" !set_hash));
 ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mprog"

let () =
  Arg.parse opts
    (fun s -> add_input (File s))
    (sprintf "Usage: %s [options]* [test|-]*" prog)


module X =
  Top
    (struct
      let verbose = !verbose
      let withindex = false
      let ascommands = false
      let texmacros = !texmacros
      let hexa = !hexa
      let compat = !compat
      let outputdir = !outputdir
      let mode = !mode
      let transpose = !transpose
      let alloc = !alloc
      let set_hash = !set_hash
    end)

(* Iterate over input, taking care of expanding filename lists from stdin and
   @list files. *)
let iter_inputs args : input Iter.t =
 fun k ->
  let do_file fname = k (File fname) in
  match args with
  (* With no positional arguments, stdin is interpreted as a list of files. *)
  | [] -> Misc.iter_stdin do_file
  | args ->
      args
      |> List.iter (function
        | File name -> Misc.iter_argv do_file [ name ]
        | Stdin -> k Stdin)

let iter_check_all (f : 'a -> bool) (iter : 'a Iter.t) : bool =
  Iter.fold (fun acc x -> let ok = f x in ok && acc) true iter

let do_input input =
  let name = input_name input in
  try
    X.zyva input;
    true
  with
  | Misc.Fatal msg ->
      eprintf "Fatal error: %a %s\n%!" Pos.pp_pos0 name msg ;
      false
  | Misc.UserError msg ->
      (* TODO: Consider changing `msg` to be some structured diagnostics value
         that can optionally carry source context. *)
      if String.starts_with ~prefix:"File " msg then
        eprintf "User error: %s\n%!" msg
      else
        eprintf "User error: %a %s\n%!" Pos.pp_pos0 name msg ;
      false

let () =
  let no_errors = iter_inputs !args |> iter_check_all do_input in
  if not no_errors then exit 1
