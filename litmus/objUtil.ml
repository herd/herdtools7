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

(* Copy files from litmus own files *)

open Printf

let cp_lib_file src dst =
  let _,in_chan = MyName.open_lib src in
  begin try MySys.cp in_chan dst
  with e -> close_in in_chan  ; raise e end ;
  close_in in_chan

let insert_lib_file o src =
  let _,in_chan = MyName.open_lib src in
  MySys.cat_chan in_chan o ;
  close_in in_chan

module type InsertConfig = sig
  val sysarch : Archs.System.t
  val word : Word.t
end

module Insert (O:InsertConfig) :
    sig
      val insert : (string -> unit) -> string -> unit
      val exists : string -> bool
    end =
  struct
    open Word

    let dir = match O.sysarch with
    | `X86 -> "_x86"
    | `PPC -> "_ppc"
    | `ARM -> "_arm"
    | `MIPS -> "_mips"
    | `AArch64 -> "_aarch64"

    let sz = match O.word with
    | W32|WXX -> "32" (* our "default" word size, used for PPC only *)
    | W64 -> "64"


    let find_lib src =
      let len = String.length src in
      let base =
        try Filename.chop_extension src
        with Invalid_argument _ -> src in
      let baselen = String.length base in
      let ext = String.sub src baselen (len-baselen) in
      let n1 = Filename.concat dir src in
      try MyName.open_lib n1
      with Misc.Fatal _ ->
        try
          let n2 = Filename.concat dir (sprintf "%s%s%s" base sz ext) in
          MyName.open_lib n2
        with Misc.Fatal _ ->
          Warn.fatal "Cannot insert lib file %s" src

    let insert out src =
      let _,in_chan = find_lib src in
      MySys.cat_chan in_chan out ;
      close_in in_chan

    let exists src =
      try
        let _,in_chan = find_lib src in
        begin try close_in in_chan with _ -> () end ;
        true
      with Misc.Fatal _ -> false
  end

module type Config = sig
  val targetos : TargetOS.t
  val driver : Driver.t
  val affinity : Affinity.t
  val arch : Archs.t
  val mode : Mode.t
  val stdio : bool
  val platform : string
end

module Make(O:Config)(Tar:Tar.S) =
  struct
    open TargetOS

    let actual_name name ext = Tar.outname (name ^ ext)

    let do_cpy fnames src tgt ext =
      let _,in_chan = MyName.open_lib (src ^ ext) in
      let fnames =
        begin try
          let fname = actual_name tgt ext in
          MySys.cp in_chan fname ;
          fname::fnames
        with e -> close_in in_chan  ; raise e end in
      close_in in_chan ;
      fnames

(* Copy lib file *)
    let cpy fnames name ext = do_cpy fnames ("_" ^ name) name ext

(* Copy lib file, changing its name *)
    let cpy' fnames src dst ext = do_cpy fnames ("_" ^ src) dst ext
(* Copy from platform subdirectory *)
    let cpy_platform fnames name ext =
      let name = sprintf "platform_%s" name in
      do_cpy fnames (Filename.concat O.platform name) name ext

    let affinity_base () = match O.targetos with
    | Linux -> "_linux_affinity"
    | AIX -> "_aix_affinity"
    | os -> Warn.fatal "Affinity not implemented for %s" (TargetOS.pp os)

    let dump () =
      let fnames = [] in
      let fnames = match O.driver with
      | Driver.Shell -> fnames
      | Driver.C|Driver.XCode -> cpy fnames "toh" ".sh" in
      let fnames = match O.arch with
        | `C ->
            cpy' fnames "showC" "show" ".awk"
        | `X86 | `ARM | `PPC | `MIPS | `AArch64 ->
            cpy fnames "show" ".awk"
      in
      let fnames = cpy fnames "litmus_rand" ".c" in
      let fnames = cpy fnames "litmus_rand" ".h" in
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
          cpy fnames "utils" ".h"
      | Mode.PreSi ->
          let fnames = cpy' fnames "presi" "utils" ".c" in
          cpy' fnames "presi" "utils" ".h" in
      let fnames =
        match O.mode with
        | Mode.Std ->
            let fnames = cpy fnames "outs" ".c" in
            let fnames = cpy fnames "outs" ".h" in
            fnames
        | Mode.PreSi ->
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
