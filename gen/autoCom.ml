open Printf

type mach = Local | Distant of string

module type Config = sig
  val addpath : string list
  val target : mach
end

module type S =  sig
  val dist_sh : string -> unit
  val dist_sh_save : string -> string -> unit
  val dist_upload : string -> string -> unit
  val dist_download : string -> string -> unit
end


module Make(C:Config) = struct

  let path = match C.addpath with
  | [] -> ""
  | ds ->
      "PATH=" ^
      List.fold_right
        (fun d k -> sprintf "%s:%s" d k)
        ds "$PATH && "

  module LocalCom = struct

    let dist_sh com = MySys.exec_stdout (sprintf "sh -c '%s'" com)

    let dist_sh_save com name =
      MySys.exec_stdout (sprintf "sh -c '%s' > %s" com name)

    let dist_upload f t = MySys.exec_stdout (sprintf "cp %s %s" f t)

    let dist_download _ _ = assert false
  end


  module DistCom (C:sig val addr : string end) =
    struct
      let ssh = "ssh -q -T -x -n"

      let dist_sh com =
        let com = sprintf "%s %s '%s%s'" ssh C.addr path com in
        MySys.exec_stdout com

      let dist_sh_save com name =
        let com = sprintf "%s %s '%s%s' > %s" ssh C.addr path com name in
        MySys.exec_stdout com


      let dist_upload f t =
        let com = sprintf "scp -q %s %s:%s > /dev/null" f C.addr t in
        MySys.exec_stdout com

      let dist_download f t =
        let com = sprintf "scp -q %s:%s %s > /dev/null" C.addr f t in
        MySys.exec_stdout com
    end


  let upload,download,sh,sh_save =
    match C.target with
    | Local ->
        let open LocalCom in
        dist_upload,dist_download,dist_sh,dist_sh_save
    | Distant addr ->
        let module M = DistCom(struct let addr = addr end) in
        let open M in
        dist_upload,dist_download,dist_sh,dist_sh_save

  let dist_upload = upload
  let dist_download = download
  let dist_sh = sh
  let dist_sh_save = sh_save
end
