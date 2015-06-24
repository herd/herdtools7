(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(*********************************)
(* Dump or run a series of tests *)
(*********************************)

open Answer
open Printf

module type Config = sig
  val carch : Archs.System.t option
  val platform : string
  val gcc : string
  val stdio : bool
  val index : string option
  val crossrun : Crossrun.t
  val adbdir : string
  val sleep : int
  val tarname : string
  val driver : Driver.t
  val cross : bool
  val hexa : bool
  val asmcommentaslabel : bool
  include RunUtils.CommonConfig
  val mkopt : Option.opt -> Option.opt
end

module type OneTest = sig
  val from_file :
      StringSet.t -> Answer.info StringMap.t -> string -> out_channel -> answer
end


module Make (Cfg:Config) (Tar:Tar.S) (CT:OneTest) : sig
  val from_files : string list -> unit
end = struct
  open MySys

  module GD = GenerateCrossDoc.Make(Cfg)

  let report_failure name msg chan =
    if not Cfg.is_out then begin
      let title = sprintf "!!! Test %s failed !!!" name in
      let warn = String.make (String.length title) '!' in
      output_line chan  warn ;
      output_line chan  title ;
      output_line chan  warn ;
      fprintf chan "Reported: %s\n" msg ;
      output_line chan ""
    end

  open Speedcheck
  let do_dont =
    match Cfg.speedcheck with
    | AllSpeed -> true
    | NoSpeed|SomeSpeed -> false


  module type ArchConf = sig
    val word : Word.t
    val delay : int
    val gccopts :  string
  end

  let get_arch arch =
    let arch = match arch with
    | `C ->
        begin match Cfg.carch with
        | Some a -> a
        | None -> assert false
        end
    | `OpenCL -> assert false
    | #Archs.System.t as a -> a in
    let opt = Option.get_default arch in
    let opt = Cfg.mkopt opt in
    let module M = struct
      let word = Option.get_word opt
      let gccopts = Option.get_gccopts opt
      let delay = Option.get_delay opt
    end in
    (module M : ArchConf)

(* Makefile utilities *)

  let makefile_vars chan arch sources =
    let module O = struct
      include Cfg
      include (val (get_arch arch) : ArchConf)
    end in
    let module RU = RunUtils.Make(O) in
    let gcc_opts = RU.get_gcc_opts in
    fprintf chan "GCC=%s\n" Cfg.gcc ;
    fprintf chan "GCCOPTS=%s\n" gcc_opts ;
    let link_opts = RU.get_link_opts in
    fprintf chan "LINKOPTS=%s\n" link_opts ;
    fprintf chan "SRC=\\\n" ;
    List.iter
      (fun src -> fprintf chan " %s\\\n" src)
      (List.rev sources) ;
    fprintf chan "\n" ;
    ()

  let makefile_clean chan extra =
    fprintf chan "clean:\n" ;
    fprintf chan "\t/bin/rm -f *.o *.s *.t *.exe *~%s\n" extra ;
    fprintf chan "\n" ;
    fprintf chan "cleansource:\n" ;
    fprintf chan "\t/bin/rm -f *.o *.c *.h *.s *~\n" ;
    fprintf chan "\n" ;
    ()

  let makefile_utils chan utils =
    let utils =
      List.fold_right
        (fun s k ->
          if Filename.check_suffix s ".c" then
            let b = Filename.chop_suffix (Filename.basename s) ".c" in
            b :: k
          else k) utils [] in
    List.iter
      (fun u ->
        let src = u ^ ".c" and obj = u ^ ".o" in
        fprintf chan "%s: %s\n" obj src ;
        fprintf chan "\t$(GCC) $(GCCOPTS) -O2 -c %s\n" src ;
        fprintf chan "\n")
      utils ;
(* UTIL objs *)
    let utils_objs =
      String.concat " " (List.map (fun s -> s ^ ".o") utils) in
    fprintf chan "UTILS=%s\n\n" utils_objs ;
    ()
;;

(* Compile (and run in command line mode) tests *)
let run_tests names out_chan =
  let exp = match Cfg.index with
  | None -> None
  | Some exp -> Some (open_out exp) in
  let  arch,docs,sources,_,_ =
    Misc.fold_argv
      (fun name (_,docs,srcs,cycles,hash_env) ->
        match CT.from_file cycles hash_env
            name out_chan with
        | Completed (a,doc,src,cycles,hash_env) ->
            begin match exp with
            | None -> ()
            | Some exp -> fprintf exp "%s\n" name
            end ;
            a,(doc::docs),(src::srcs),cycles,hash_env
        | Absent a -> a,docs,srcs,cycles,hash_env
        | Interrupted (a,e) ->
            let msg =  match e with
            | Misc.Exit -> "None"
            | Misc.Fatal msg
            | Misc.UserError msg ->
                eprintf "%a %s\n%!" Pos.pp_pos0 name msg ;
                msg
            | e -> 
                let msg = sprintf "exception %s"  (Printexc.to_string e) in
                eprintf "%a %s\n%!" Pos.pp_pos0 name msg ;
                msg in
            report_failure name msg out_chan ;
            a,docs,srcs,cycles,hash_env)
      names (`X86,[],[],StringSet.empty,StringMap.empty) in
  begin match exp with
  | None -> ()
  | Some exp -> close_out exp
  end ;
  let utils =
    let module O = struct
      include Cfg
      let arch = arch
    end in
    let module Obj = ObjUtil.Make(O)(Tar) in
    Obj.dump () in
  arch,docs,sources,utils

(* Run tests (command line mode) *)
let dump_command names =
  let out_chan = stdout in
  let arch,_,_,utils = run_tests names out_chan in
  let module O = struct
      include Cfg
    include (val (get_arch arch) : ArchConf)
  end in
  let module RU = RunUtils.Make(O) in
  RU.report_machine out_chan ;
  List.iter MySys.remove utils ;
  flush out_chan ;
  ()

let dump_shell names =
  Misc.output_protect
    (fun out_chan ->
      output_line out_chan "date" ;
      output_line out_chan "LITMUSOPTS=\"${@:-$LITMUSOPTS}\"" ;
      begin match Cfg.crossrun with
      | Crossrun.No -> ()
      | Crossrun.Adb  ->
          fprintf out_chan "RDIR=%s\n" Cfg.adbdir ;
          fprintf out_chan "adb shell mkdir $RDIR >/dev/null 2>&1\n" ;
          fprintf out_chan "dorun () {\n" ;
          fprintf out_chan "  EXE=$1\n" ;
          fprintf out_chan "  shift\n" ;
          fprintf out_chan "  OPTS=\"$@\"\n" ;
          fprintf out_chan "  BASE=$(basename $EXE)\n" ;
          fprintf out_chan "  adb push $EXE $RDIR >/dev/null 2>&1\n" ;
          fprintf out_chan "  adb shell $RDIR/$BASE $OPTS | tr -d '\\r' \n" ;
          fprintf out_chan "  adb shell rm $RDIR/$BASE >/dev/null 2>&1\n" ;
          fprintf out_chan "}\n" ;
      | Crossrun.Host h ->
          fprintf out_chan "RHOST=%s\n" h.Crossrun.host ;
          fprintf out_chan "RPORT=%i\n"
            (match h.Crossrun.port with| None -> 22| Some p -> p) ;
          fprintf out_chan "doscp () {\n" ;
          fprintf out_chan "  EXE=$1\n" ;
          fprintf out_chan "  scp -q -P $RPORT $EXE $RHOST: 2> /dev/null\n" ;
          fprintf out_chan "  while [ $? -ne 0 ]\n" ;
          fprintf out_chan "  do\n" ;
          fprintf out_chan "    sleep 30\n" ;
          fprintf out_chan "    scp -q -P $RPORT $EXE $RHOST: 2> /dev/null\n" ;
          fprintf out_chan "  done\n" ;
          fprintf out_chan "  true\n" ;
          fprintf out_chan "}\n" ;
          ()
      end ;
      let sleep = Cfg.sleep in
      if sleep >= 0 then fprintf out_chan "SLEEP=%i\n" sleep ;
      if do_dont then fprintf out_chan "LOG=./$$.txt\n" ;
      let arch,_,sources,utils = run_tests names out_chan in

      if do_dont then output_line out_chan "rm -f $LOG" ;
      let module O = struct
        include Cfg
        include (val (get_arch arch) : ArchConf)
      end in
      let module RU = RunUtils.Make(O) in
      RU.report_machine out_chan ;
      output_line out_chan "head -1 comp.sh" ;
      output_line out_chan "echo \"LITMUSOPTS=$LITMUSOPTS\"" ;
      output_line out_chan "date" ;
      arch,sources,utils)
    (Tar.outname (MyName.outname "run" ".sh"))


let dump_shell_cont arch sources utils =
  let sources = List.map Filename.basename  sources in
(* Shell script for sequential compilation *)
  let module O = struct
    include Cfg
    include (val (get_arch arch) : ArchConf)
  end in
  let module RU = RunUtils.Make(O) in
  Misc.output_protect
    (fun chan ->
      let gcc_opts = RU.get_gcc_opts in
      fprintf chan "GCC=%s\n" Cfg.gcc ;
      fprintf chan "GCCOPTS=\"%s\"\n" gcc_opts ;
      let link_opts = RU.get_link_opts  in
      fprintf chan "LINKOPTS=\"%s\"\n" link_opts ;
      fprintf chan "/bin/rm -f *.exe *.s\n" ;
      List.iter
        (fun s ->
          if Filename.check_suffix s ".c" then
            fprintf chan "$GCC $GCCOPTS -O2 -c %s\n"
              (Filename.basename s))
        utils ;
      let utils_objs =
        List.fold_right
          (fun s k ->
            if Filename.check_suffix s ".c" then
              let b = Filename.chop_suffix (Filename.basename s) ".c" in
              (b ^ ".o") :: k
            else k) utils [] in
      let utils_objs = String.concat " " utils_objs in
      List.iter
        (fun src ->
          let exe = Filename.chop_extension src ^ ".exe" in
(* No more moderate parallelism [blocks on abducens, sh bug ?] *)
          fprintf chan
            "$GCC $GCCOPTS $LINKOPTS -o %s %s %s\n" exe utils_objs src ;
          let srcS = Filename.chop_extension src ^ ".s" in
          let srcT = Filename.chop_extension src ^ ".t" in
          fprintf chan "$GCC $GCCOPTS -S %s && awk -f show.awk %s > %s && /bin/rm %s\n"
            src srcS srcT srcS ;
          ())
        (List.rev sources))
    (Tar.outname (MyName.outname "comp" ".sh")) ;
(* Add a small README file *)
  Misc.output_protect
    (fun chan ->
      GD.gen_readme_src chan arch sources)
    (Tar.outname (MyName.outname "README" ".txt")) ;
(* Makefile for parallel compilation *)
  Misc.output_protect
    (fun chan ->
(* Variables *)
      makefile_vars chan arch sources ;
      fprintf chan "EXE=$(SRC:.c=.exe)\n" ;
      fprintf chan "T=$(SRC:.c=.t)\n" ;
      fprintf chan "\n" ;
(* Entry points *)
      fprintf chan "all: $(EXE) $(T)\n" ;
      fprintf chan "\n" ;
      makefile_clean chan "";
      makefile_utils chan utils ;
(* Cannot use %.s here for Mac that preprocesses assembly files,
   and because #comments makes the preprocessor crash ! *)
      let src_ext = match Cfg.targetos with
      | TargetOS.Mac -> 'c'
      | TargetOS.Linux|TargetOS.AIX -> 's' in
      fprintf chan "%%.exe:%%.%c $(UTILS)\n" src_ext ;
      fprintf chan
        "\t$(GCC) $(GCCOPTS) $(LINKOPTS) -o $@ $(UTILS) $<\n" ;
      fprintf chan "\n" ;
(* .s pattern rule *)
      fprintf chan "%%.s:%%.c\n" ;
      fprintf chan "\t$(GCC) $(GCCOPTS) -S $<\n" ;
      fprintf chan "\n" ;
(* .t pattern rule *)
      fprintf chan "%%.t:%%.s\n" ;
      fprintf chan "\tawk -f show.awk $< > $@\n" ;
      fprintf chan "\n")
    (Tar.outname (MyName.outname "Makefile" "")) ;
  Tar.tar  () ;
  ()

let dump_c xcode names =
  Misc.output_protect
    (fun out_chan ->
      let module O = Indent.Make(struct let hexa = Cfg.hexa let out = out_chan end) in
      O.o "#include <stdio.h>" ;
      O.o "#include <stdlib.h>" ;
      if Cfg.sleep > 0 then  O.o "#include <unistd.h>" ;
      O.o "" ;
      O.o "/* Declarations of tests entry points */" ;
      let arch,docs,srcs,utils = run_tests names out_chan in
      let module C = struct
        include Cfg
        include (val (get_arch arch) : ArchConf)
      end in
      let module RU = RunUtils.Make(C) in

      O.o "" ;
      O.o "/* Date function */" ;
      O.o "#include <time.h>" ;
      O.o "static void my_date(FILE *out) {" ;
      O.oi "time_t t = time(NULL);" ;
      O.oi "fprintf(out,\"%s\",ctime(&t));";
      O.o "}" ;
      O.o "" ;
      O.o "/* Postlude */" ;
      O.o "static void end_report(int argc,char **argv,FILE *out) {" ;
      let dstring s =
        O.fi "fprintf(out,\"%%s\\n\",\"%s\");" (String.escaped s) in
      RU.report_parameters dstring ;
      O.o "/* Command line options */" ;
      O.oi "fprintf(out,\"Command:\");" ;
      O.oi "for ( ; *argv ; argv++) {" ;
      O.oii "fprintf(out,\" %s\",*argv);" ;
      O.oi "}" ;
      O.oi "putc('\\n',out);" ;
      O.o "}" ;
      O.o"" ;
      O.o"/* Run all tests */" ;
      if xcode then begin
        O.o "#import \"run.h\"";
        O.o "" ;
        O.o "@implementation Run" ;
        O.o "+ (void) runWithArgc:(int) argc argv: (char **) argv out: (FILE *) out tick: (id <Ticker>)tick {" ;
        O.oi "my_date(out);" ;
        List.iteri
          (fun k doc ->
            if k > 0 && Cfg.sleep > 0 then  O.fi "sleep(%i);" Cfg.sleep ;
            O.fi "%s(argc,argv,out);" (MyName.as_symbol doc) ;
            O.oi "[tick tick];")
          (List.rev docs) ;
        O.oi "end_report(argc,argv,out);" ;
        O.oi "my_date(out);" ;
        O.o "}" ;
      end else begin
        O.o "static void run(int argc,char **argv,FILE *out) {" ;
        O.oi "my_date(out);" ;
        List.iteri
        (fun k doc ->
          if k > 0 && Cfg.sleep > 0 then  O.fi "sleep(%i);" Cfg.sleep ;
          O.fi "%s(argc,argv,out);" (MyName.as_symbol doc))
          (List.rev docs) ;
        O.oi "end_report(argc,argv,out);" ;
        O.oi "my_date(out);" ;
        O.o "}"
      end ;
      O.o"" ;
      if xcode then begin
        O.o "+ (int) getNTests {" ;
        O.fi "return %i;" (List.length docs) ;
        O.o "}" ;
        O.o "" ;
        O.o "@end"
      end else  begin
        O.o"int main(int argc,char **argv) {" ;
        O.oi "run(argc,argv,stdout);" ;
        O.oi "return 0;" ;
        O.o"}"
      end ;
      arch,srcs,utils)
    (Tar.outname (MyName.outname "run" (if xcode then ".m" else ".c")))


let dump_c_cont xcode arch sources utils =
  let sources = List.map Filename.basename  sources in
(* Makefile *)
  Misc.output_protect
    (fun chan ->
      makefile_vars chan arch sources ;
(* Various intermediate targets *)
      fprintf chan "T=$(SRC:.c=.t)\n" ;
      fprintf chan "H=$(SRC:.c=.h)\n" ;
      if not xcode then begin
        fprintf chan "OBJ=$(SRC:.c=.o)\n" ;
        fprintf chan "EXE=run.exe\n" ;
        fprintf chan "\n" ;
      end ;
(* Entry point *)
      if xcode then begin
        fprintf chan "all: $(H)\n" ;
      end else begin
        fprintf chan "all: $(EXE)\n" ;
      end ;
      fprintf chan "\n" ;
      makefile_clean chan " $(H)";
      if not xcode then makefile_utils chan utils ;
(* Rules *)
      if not xcode then begin
        let objs = "$(UTILS) $(OBJ) run.o" in
        fprintf chan "$(EXE): %s\n" objs ;
        fprintf chan "\t$(GCC)  $(GCCOPTS) $(LINKOPTS) -o $@ %s\n" objs ;
        fprintf chan "\n" ;
(* .o pattern rule *)
        fprintf chan "%%.o:%%.c\n" ;
        fprintf chan
        "\t$(GCC) $(GCCOPTS) $(LINKOPTS) -c -o $@ $<\n" ;
        fprintf chan "\n"
      end ;
(* .s pattern rule *)
      fprintf chan "%%.s:%%.c\n" ;
      fprintf chan "\t$(GCC) -DASS $(GCCOPTS) -S $<\n" ;
      fprintf chan "\n" ;
(* .t pattern rule *)
      fprintf chan "%%.t:%%.s\n" ;
      fprintf chan "\tawk -f show.awk $< > $@\n" ;
      fprintf chan "\n" ;
 (* .h pattern rule *)
      fprintf chan "%%.h:%%.t\n" ;
      fprintf chan "\tsh toh.sh $< > $@\n" ;
      fprintf chan "\n" ;
(* Dependencies *)
      if not xcode then begin
        List.iter
          (fun src ->
            let base = Filename.chop_extension src in
            fprintf chan "%s.o: %s.h %s.c\n" base base base)
          sources ;
        fprintf chan "\n"
      end ;
      ())
    (Tar.outname (MyName.outname "Makefile" "")) ;
(* XCode interface file *)
  if xcode then begin
    Misc.output_protect
      (fun chan ->
        let module O = Indent.Make(struct let hexa = Cfg.hexa let out = chan end) in
        O.o "#import <Foundation/Foundation.h>" ;
        O.o "#import \"ticker.h\"" ;
        O.o "" ;
        O.o "@interface Run : NSObject" ;
        O.o
          "+ (void) runWithArgc:(int) argc argv: (char **) argv out: (FILE *) out tick: (id <Ticker>)tick ;" ;
        O.o "+ (int) getNTests;" ;
        O.o "@end")
       (Tar.outname (MyName.outname "run" ".h")) ;
  end ;
  Tar.tar  () ;
  ()

let dump_cross _arch =
  (* Now build new directory structure *)
  let top = Filename.temp_file "dir" ".tmp" in
  MySys.mkdir top ;
  let dname = "litmus_tests" in
  let dir = Filename.concat top dname in
  MySys.mkdir dir ;
  (* Put this in dir *)
  Misc.output_protect
    (fun chan -> output_line chan (GD.gen_makefile ()))
    (Filename.concat dir "Makefile") ;
  Misc.output_protect
    (fun chan -> GD.gen_readme chan)
    (Filename.concat dir "README.txt") ;
(* Untar sources in src sub-directory *)
  if Tar.is_archive then begin
    let src = Filename.concat dir "src" in
    MySys.mkdir src ;
    let cmd =
      let tf = Filename.basename Cfg.tarname in
      sprintf "mv %s %s && cd %s && tar x%sf %s && /bin/rm -f %s"
        Cfg.tarname src src (Tar.tarz ()) tf tf in
    MySys.exec_stdout cmd ;
    Tar.tar_dir dir
  end else begin
    let tgt = Cfg.tarname in
    let tmp = sprintf "%s.tmp" tgt in
    let src = Filename.concat tgt "src" in
    let com =
      sprintf
        "mv %s %s && mv %s %s && mv %s %s && /bin/rm -rf %s"
        tgt tmp dir tgt tmp src top in
    MySys.exec_stdout com
  end

let from_files =
  if not Cfg.is_out then begin
    dump_command
  end else
    fun names ->
      let arch =
        match Cfg.driver with
        | Driver.Shell ->
            let arch,sources,utils = dump_shell names in
            dump_shell_cont arch sources utils ;
            arch
        | Driver.C|Driver.XCode as d ->
            let xcode = match d with
            | Driver.XCode -> true
            | _ -> false in
            let arch,sources,utils = dump_c xcode names in
            dump_c_cont xcode arch sources utils ;
            arch in
      if Cfg.cross then dump_cross arch
end
