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

(* Copy files from litmus own files *)

open Printf

type status = Before | Inside | After

let is_com s =
  let len = String.length s in
  len >= 3 && s.[0] = '/' && s.[1] = '*' &&
  s.[len-2] = '*' && s.[len-1] = '/'

let no_header out =
  let st = ref Before in
  fun s -> match !st with
  | Before ->
      if String.length s > 0 then begin
        if is_com s then st := Inside
        else begin
          st := After ;
          out s        
        end
      end
  | Inside ->
      if not (is_com s) then begin
        st := After ;
        out s
      end
  | After -> out s

let add_no_header fname out =
  if
    Filename.check_suffix fname ".c" ||
    Filename.check_suffix fname ".h"
  then no_header out
  else out

let insert_lib_file o src =
  let _,in_chan = MyName.open_lib src in
  let o = add_no_header src o in
  MySys.cat_chan in_chan o ;
  close_in in_chan

module type InsertConfig = sig
  val sysarch : Archs.System.t
end

module Insert (O:InsertConfig) :
    sig
      val insert : (string -> unit) -> string -> unit
      val exists : string -> bool
      val copy : string -> (string -> string) -> unit
    end =
  struct
    open Word

    let dir = match O.sysarch with
    | `X86 -> "_x86"
    | `X86_64 -> "_x86_64"
    | `PPC -> "_ppc"
    | `ARM -> "_arm"
    | `MIPS -> "_mips"
    | `AArch64 -> "_aarch64"
    | `RISCV -> "_riscv"

    let find_lib src =
      let n1 = Filename.concat dir src in
      try MyName.open_lib n1
      with Misc.Fatal _ ->
        Warn.fatal "Cannot find lib file %s" src

    let insert out src =
      let _,in_chan = find_lib src in
      let out = add_no_header src out  in
      MySys.cat_chan in_chan out ;
      close_in in_chan

    let exists src =
      try
        let _,in_chan = find_lib src in
        begin try close_in in_chan with _ -> () end ;
        true
      with Misc.Fatal _ -> false

    let copy fname outname =
      let _,in_chan = find_lib fname in
      begin try
        MySys.cp in_chan (outname fname)
      with e ->
        begin
          close_in in_chan  ;
          raise e
        end
      end ;
      close_in in_chan
  end

module type Config = sig
  val targetos : TargetOS.t
  val driver : Driver.t
  val affinity : Affinity.t
  val arch : Archs.t
  val carch   : Archs.System.t option
  val mode : Mode.t
  val stdio : bool
  val platform : string
  val asmcommentaslabel : bool
  val cached : bool
end

module Make(O:Config)(Tar:Tar.S) =
  struct
    open TargetOS

    let actual_name name ext = Tar.outname (name ^ ext)

    let do_cpy ?prf fnames src tgt ext =
      let _,in_chan = MyName.open_lib (src ^ ext) in
      let fnames =
        begin try
          let fname = actual_name tgt ext in
          MySys.cp ?prf in_chan fname ;
          fname::fnames
        with e -> close_in in_chan  ; raise e end in
      close_in in_chan ;
      fnames

(* Copy lib file *)
    let cpy ?prf fnames name ext = do_cpy ?prf fnames ("_" ^ name) name ext

(* Copy lib file, changing its name *)
    let cpy' ?prf fnames src dst ext = do_cpy ?prf fnames ("_" ^ src) dst ext

(* Copy from platform subdirectory *)
    let cpy_platform fnames name ext =
      let name = sprintf "platform_%s" name in
      do_cpy fnames (Filename.concat O.platform name) name ext

    let affinity_base () = match O.targetos with
    | Linux|FreeBsd -> "_linux_affinity"
    | AIX -> "_aix_affinity"
    | Android8 -> "_android8_affinity"
    | Mac as os ->
        Warn.fatal "Affinity not implemented for %s" (TargetOS.pp os)

    let dump () =
      let fnames = [] in
      let fnames = match O.driver with
      | Driver.Shell -> fnames
      | Driver.C|Driver.XCode -> cpy fnames "toh" ".sh" in
      let fnames = match O.arch with
        | `C ->
            if O.asmcommentaslabel then
              cpy' fnames "showLabel" "show" ".awk"
            else
              cpy' fnames "showC" "show" ".awk"
        | `X86 |`X86_64 | `ARM | `PPC | `MIPS | `AArch64 |` RISCV->
            if O.asmcommentaslabel then
              cpy' fnames "showLabel" "show" ".awk"
            else
              cpy fnames "show" ".awk"
        | `CPP|`LISA -> Warn.fatal "no support for arch '%s'" (Archs.pp O.arch) 
      in
      let fnames = match O.mode with
      | Mode.Kvm -> fnames
      | Mode.PreSi|Mode.Std ->
          let fnames = cpy fnames "litmus_rand" ".c" in
          let fnames = cpy fnames "litmus_rand" ".h" in
          fnames in
      let fnames =
        if O.stdio then fnames
        else
          let fnames = cpy_platform fnames "io" ".c" in
          let fnames = cpy_platform fnames "io" ".h" in
          let fnames = cpy fnames "litmus_io" ".c" in
          let fnames = cpy fnames "litmus_io" ".h" in
          fnames in
      let fnames = match O.mode with
      | Mode.Std ->
          let fnames = cpy fnames "utils" ".c" in
(* Select cached conditional variables, disabled.
          if O.cached then
            cpy ~prf:"#define CACHE 1" fnames "utils" ".h"
          else *)
            cpy fnames "utils" ".h"
      | Mode.PreSi ->
          let fnames = cpy' fnames "presi" "utils" ".c" in
          cpy' fnames "presi" "utils" ".h"
      |  Mode.Kvm ->
          let fnames = cpy' ~prf:"#define KVM 1" fnames "presi" "utils" ".c" in
          let fnames = cpy' ~prf:"#define KVM 1" fnames "presi" "utils" ".h" in
          let fnames = cpy fnames "kvm_timeofday" ".h" in
          let sysarch = match Archs.get_sysarch O.arch O.carch with
          | Some a -> a
          | None -> assert false in
          let module I = Insert(struct let sysarch = sysarch end) in
          I.copy "kvm_timeofday.c" Tar.outname ;
          I.copy "kvm-headers.h" Tar.outname ;
          fnames in
      let fnames =
        match O.mode with
        | Mode.Std ->
            let fnames = cpy fnames "outs" ".c" in
            let fnames = cpy fnames "outs" ".h" in
            fnames
        | Mode.PreSi|Mode.Kvm ->
            fnames in
      let fnames =
        match O.affinity with
        | Affinity.No -> fnames
        | _ ->
            let affi = affinity_base () in
            let fnames = do_cpy fnames affi "affinity" ".c" in
            let fnames = cpy fnames "affinity" ".h" in
            fnames in
      fnames

  end
