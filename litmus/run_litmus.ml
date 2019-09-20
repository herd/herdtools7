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

(* Run a test and collect statistics *)

open Printf
open MySys

module type Config = sig
  val crossrun : Crossrun.t
  val driver : Driver.t
  val sleep : int
  include RunUtils.Config
end

module type Test = sig
  type test
  val dump : out_channel -> Name.t -> test -> unit
  val lines : Name.t -> test -> string list
end

module Make(O:Config)(Tar:Tar.S)(D:Test) =
  struct
    module RU = RunUtils.Make(O)

    let gcc =
      let gcc_opts = RU.get_gcc_opts in
      let link_opts = RU.get_link_opts in
      sprintf "%s %s%s" O.gcc  gcc_opts link_opts

    let gcc2as =
      let gcc_opts = RU.get_gcc_opts in
      sprintf "%s %s" O.gcc gcc_opts

    let compile_test name source =
      let sX = MyName.outname name (Mode.exe O.mode) in
      if not O.is_out then begin
        let sX = Tar.outname sX in
        let source = Tar.outname source in
        let utils =
          let k = match O.mode with
          | Mode.Std -> ["litmus_rand.c"; "outs.c";]
          | Mode.PreSi -> ["litmus_rand.c"; "litmus_io.c";]
          | Mode.Kvm -> []  in
          let utils =
            match O.affinity with
            | Affinity.No -> "utils.c"::k
            | _ ->  "utils.c"::"affinity.c"::k in
          String.concat " " (List.map Tar.outname utils) in
        let com = sprintf "%s -o %s %s %s" gcc sX utils source in
        exec_stdout com
      end ;
      sX

    let do_show out_chan in_chan =
      read_by_line in_chan
        (fun line prev ->
          let open Showcode in
          begin match prev with
          | Next -> fprintf out_chan "%s\n" line
          | No|Now -> ()
          end ;
          let here = Showcode.see line in
          begin match here with
          | Now -> fprintf out_chan "%s\n" line
          | No|Next -> ()
          end ;
          here)
        Showcode.No

    let showcode chan name =
      ignore (Misc.input_protect (do_show chan) name)

    let assemble_test chan name source =
      let sS = MyName.outname name ".s" in
      if O.is_out then
        let sS = MyName.outname name ".t" in
        let com =
          match O.mode with
          | Mode.Kvm -> sprintf "cat ${TDIR}/%s" sS
          | Mode.Std|Mode.PreSi -> sprintf "cat %s" sS in
        output_line chan com
      else begin
        let source = Tar.outname source in
        let sS = Tar.outname sS in
        let com = sprintf "%s -S -o %s %s"
            gcc2as sS source in
        exec_stdout com ;
        showcode chan sS  ;
        output_line chan ""
      end ;
      sS

    open Speedcheck

    let do_dont = match O.speedcheck with
    | AllSpeed -> true
    | NoSpeed|SomeSpeed -> false

    let run_test chan _t name sX =
      let opts =
        sprintf "%s $LITMUSOPTS" (if O.verbose > 0 then "-v" else "-q") in
      if O.is_out then begin
        let exe = Filename.concat "." sX in
        let com =
          match O.crossrun with
          | Crossrun.No -> sprintf "%s %s" exe opts
          | Crossrun.Adb|Crossrun.Kvm _ ->  sprintf "dorun %s %s" exe opts
          | Crossrun.Qemu _ ->  sprintf "$QEMU %s %s" exe opts
          | Crossrun.Host _h ->
              let scp = sprintf "doscp %s" exe in
              let run = sprintf "%s %s" exe opts in
              let rm = sprintf "rm %s" exe in
              sprintf "%s && ssh -q -n -p $RPORT $RHOST \"%s 2>/dev/null && %s\""
                scp  run rm in
        let out_com chan com =
          if do_dont then
            fprintf chan "%s | tee $LOG\n" com
          else output_line chan com in
        out_com chan com ;
        if do_dont then begin
          output_line chan
            "if grep -e 'Sometimes' $LOG >/dev/null 2>/dev/null" ;
          output_line chan "then" ;
          fprintf chan "  touch %s.no\n" (RU.file_base name) ;
          output_line chan "fi" ;
          output_line chan "rm -f $LOG"
        end
      end else begin
        let com = sprintf "%s %s" (Tar.outname sX) opts in
        exec_stdout com ;
        ()
      end



    let do_c = match O.driver with
    | Driver.C|Driver.XCode -> O.is_out
    | Driver.Shell -> false

    let report name doc test source chan =
(* Static information *)
      let title = sprintf "%% Results for %s %%" name in
      let nice = String.make (String.length title) '%' in
      RU.open_norun doc chan ;
      RU.open_quote chan ;
      output_line chan nice ;
      output_line chan title ;
      output_line chan nice ;
      D.dump chan doc test ;
(* Actual assembler *)
      output_string chan "Generated assembler\n" ;
      RU.close_quote chan ;
      flush chan ;
(* End of static information *)
      let _sS = assemble_test chan name source in
(* Compile *)
      let sX = compile_test name source in
(* Run test *)
      flush chan ;
      run_test chan test doc sX ;
      RU.close_norun chan ;
      if O.is_out && O.sleep >= 0 then output_line chan "sleep $SLEEP" ;
      output_line chan "" ;
      flush chan

    let run name chan doc test source =
      if do_c then begin
        fprintf chan "extern int %s(int argc,char **argv,FILE *out);\n" (MyName.as_symbol doc)
      end else
        report name doc test source chan

  end
