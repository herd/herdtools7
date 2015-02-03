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

(* Beware, this file is litmus/gen.new common, change with caution *)

open Printf

module type Option = sig
  val verbose : int
(* Output name *)
  val outname : string option
end

module type S = sig
(* Gives actual output name, ie add path to directory where the file is created *)
  val outname : string -> string

(* Build archive or not *)
  val is_archive : bool

(* Returns z if O.outname is *.tgz or empty string otherwise *)
  val tarz : unit -> string

(* Produce final tar archive (and remove temporary directory) *)
  val tar : unit -> unit

(* 'tar_dir dir' Similar, but archive contains top directory 'dir' *)
  val tar_dir : (*dir*) string -> unit
end

module Make(O:Option) : S =
  struct

    type style = Dir | Tar | TarGz

    let arg = match O.outname with
    | None -> Filename.current_dir_name
    | Some n -> n

    let style = 
      if Filename.check_suffix arg ".tgz" then TarGz
      else if Filename.check_suffix arg ".tar" then Tar
      else Dir

    let is_archive = match style with
    | Dir -> false
    | Tar|TarGz -> true

(* Limited system interface *)

    let command cmd =
      if O.verbose > 1 then begin
        eprintf "Exec: %s -> %!" cmd
      end ;
      let r = Sys.command cmd in
      if O.verbose > 1 then begin
        eprintf "%i\n%!" r
      end ;
      r
        
    let exec cmd = match command cmd with
    | 0 -> ()
    | _ -> Warn.fatal "Exec of '%s' failed" cmd

    let rmdir name = exec (sprintf "/bin/rm -rf %s" name)
    let mkdir name = exec (sprintf "/bin/rm -rf %s && mkdir %s" name name)
    let direxists name = Sys.file_exists name && Sys.is_directory name

(************)
(* Let's go *)
(************)

    let mk_temp_dir () =
      let name = Filename.temp_file  "dir" ".tmp" in
      mkdir name ;
      name

    let out_dir = match style with
    | Dir ->
        if direxists arg then
          arg
        else
          Warn.fatal "directory %s does not exist"  arg
    | Tar|TarGz -> mk_temp_dir ()
          

    let outname name = Filename.concat out_dir name

    let tarz () =  match style with
    | TarGz -> "z"
    | Tar|Dir -> ""


    let exec_tar2 dir1 dir2 tar =
      let z = tarz () in
      exec
        (sprintf "( cd %s && tar c%sf - %s ) > %s" dir1 z dir2 tar)

    let exec_tar dir tar = exec_tar2 dir "." tar  

    let tar () = match style with
    | Tar|TarGz ->
        let dir = out_dir in
        exec_tar dir arg ;
        rmdir dir
    | Dir -> ()
          
          
    let tar_dir dir =
      let tar = arg in
      let up = Filename.dirname dir
      and name =  Filename.basename dir in
      exec_tar2 up name tar ;
      rmdir up
  end
    
