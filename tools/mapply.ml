open Printf

type mode = Buff | File
(* Task engine *)

module type TArg = sig
  val com : string
  val comargs : string list
  val verbose : int
  val mode : mode
end

module Task(A:TArg) = struct
  open Unix
  module W = Warn.Make(A)

  type task =
      { idx : int ; com : string ; chan : in_channel ;
        oname : string ; buff : Buffer.t; }
(* Fork utility *)
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (sprintf "mapply.%i" (getpid()))

  let rmrf dir = ignore (Sys.command (sprintf "/bin/rm -rf %s" dir))

  let _ =
    match A.mode with
    | File ->
        let doit signum =
          Sys.set_signal signum
            (Sys.Signal_handle
               (fun _ -> rmrf dir ; exit 2)) in
        doit Sys.sigint ;
        doit Sys.sigquit ;
        doit Sys.sigterm ;
        doit Sys.sighup ;
        ()
    | Buff -> ()


  let nobuff = Buffer.create 0

  let popen idx cmd args name =
    try
      let base =
        try
          Filename.chop_extension (Filename.basename name)
        with Invalid_argument _ ->
          Warn.warn_always "Ignoring file %s, since it has no extension" name ;
          raise Exit in
      let oname = Filename.concat dir (sprintf "%s-%02i.txt" base idx) in
      let com =
        let opts = match args with
        | [] -> ""
        | _::_ -> " " ^ String.concat " " args in
        match A.mode with
        | File -> sprintf "%s%s %s>%s" cmd opts name oname
        | Buff -> sprintf "%s%s %s" cmd opts name in
      if A.verbose > 2 then eprintf "Starting: '%s' on %02i\n" com idx ;
      let chan = Unix.open_process_in com in
      begin match A.mode with
      | File -> ()
      | Buff -> set_nonblock (descr_of_in_channel chan)
      end ;
      let buff = match A.mode with  Buff -> Buffer.create 128 | File -> nobuff in
      Some { com; idx; buff; chan; oname;}
    with Exit -> None

  let table = Hashtbl.create 17

  let get_waiting () = Hashtbl.fold (fun fd _ r -> fd::r) table []

  let rec start_task idx (nrun,iter as k) = match iter with
  | None -> k
  | Some iter -> match Misc.next_iter iter with
    | Some (name,iter) ->
        let task = popen idx A.com A.comargs name in
        begin match task with
        | Some task ->
            let fd = descr_of_in_channel task.chan in
            Hashtbl.add table fd task ;
            if A.verbose > 1 then eprintf "Start %02i\n%!" idx ;
            nrun+1,Some iter
        | None ->  start_task idx (nrun,Some iter)
      end
    | None -> nrun,None

  let sz = match A.mode with File -> 1024 | Buff -> 1024

  let warn_status st =
    Warn.warn_always "task ended with %s"
      (match st with
      | WEXITED i -> sprintf "exit %i" i
      | WSIGNALED i -> sprintf "signaled %i" i
      | WSTOPPED i -> sprintf "stopped %i" i)

  let to_stdout oname =
    Misc.input_protect
      (fun chan ->
        let buff = Bytes.create sz in
        try
          while true do
            match input chan buff 0 sz with
            | 0 -> raise Exit
            | n -> output Pervasives.stdout buff 0 n
          done
        with Exit ->())
      oname ;
    flush Pervasives.stdout ;
    Sys.remove oname

  let task_file (nrun,files) fd =
    let task =
      try Hashtbl.find table fd
      with Not_found -> assert false in
    Hashtbl.remove table fd ;
    begin match close_process_in task.chan with
    | WEXITED 0 ->
        to_stdout task.oname ;
        start_task task.idx (nrun-1,files)
    | st ->
        warn_status st ;
        start_task task.idx (nrun-1,files)
    end


  let to_buff fd t =
    let buff = Bytes.create sz in
    let rec to_rec () =
      try
        if A.verbose > 2 then eprintf "Read %02i\n%!" t.idx ;
        let nread = read fd buff 0 sz in
        if A.verbose > 1 then eprintf "Got %i from %02i\n%!" nread t.idx ;
        match nread with
        | 0 -> true
        | n ->
            Buffer.add_string t.buff (Bytes.sub_string buff 0 n) ;
            to_rec ()
      with
      | Unix_error ((EWOULDBLOCK|EAGAIN),_,_) -> false
      | e -> raise e in
    to_rec ()

  let task_buff (nrun,files as k) fd =
    let task =
      try Hashtbl.find table fd
      with Not_found -> assert false in
    let is_over = to_buff fd task in
    if is_over then begin
      if A.verbose > 1 then eprintf "Over %02i\n%!" task.idx ;
      Hashtbl.remove table fd ;
      begin match close_process_in task.chan with
      | WEXITED 0 ->
          Buffer.output_buffer Pervasives.stdout task.buff ;
          flush Pervasives.stdout
      | st ->
          warn_status st
      end ;
      start_task task.idx (nrun-1,files)
    end else begin
      if A.verbose > 2 then eprintf "Again %02i\n%!" task.idx ;
      k
    end

  let process_task = match A.mode with
  | File -> task_file
  | Buff -> task_buff

  let ppok ok =
     List.iter
      (fun fd ->
        let {idx=idx;_} =
          try Hashtbl.find table fd
          with Not_found -> assert false in
        eprintf " %02i" idx)
      ok ;
    eprintf "\n%!"  

  let rec loop (nrun,_ as k) =
    if nrun > 0 then begin
      let fds = get_waiting () in
      assert (List.length fds = nrun) ;
      let ok,_,_ = select fds [] [] (-1.0) in
      if A.verbose > 0 then begin match ok with
      | []|[_] ->
          if A.verbose > 1 then begin
            eprintf "Select" ;
            ppok ok
          end
      | _ ->
          eprintf "Multiple select:" ;
          ppok ok
      end ;
      let k = List.fold_left process_task k ok in
      loop k
    end

  let run j names =
    let names = Misc.mk_iter names in
    begin match A.mode with
    | File -> mkdir dir 0o700
    | Buff -> ()
    end ;
    let rec start_rec k = function
      | 0 -> k
      | j -> start_rec (start_task j k) (j-1) in
    loop (start_rec (0,Some names) j) ;
    begin match A.mode with
    | File ->
        begin try rmdir dir
        with _ -> W.warn "Cannot delete directory %s" dir end
    | Buff -> ()
    end
end


let args = ref []
let com = ref "echo"
let verbose = ref 0
let j = ref 1
let mode = ref Buff
let comargs = ref []

let parse_mode tag = match tag with
| "buff" -> Buff
| "file" -> File
| _ ->
    raise
      (Arg.Bad (sprintf "%s: bad rag for option -mode" tag))

let pp_mode = function
  | Buff -> "buff"
  | File -> "file"

let set_mode tag = mode := parse_mode tag

let () =
  Arg.parse
    ["-v", Arg.Unit (fun () -> incr verbose)," be verbose";
     "-j", Arg.Int (fun i -> j := i),"<n> manage <n> simultaneaous tasks" ;
     "-com", Arg.String (fun c -> com := c),"<com> set command (default echo)";
     "-comargs",
     Arg.String (fun args -> comargs := !comargs @ Misc.split_comma args),
     "<args> initial arguments for command (comma separated)";
     "-mode", Arg.String set_mode,
     sprintf
       "(buff|file) use either internal buffers or files for comunication, default %s" (pp_mode !mode);]
    (fun arg -> args := arg :: !args)
    ""

let names = !args


let () =
  if !j <= 1 then
    Misc.iter_argv
      (fun name ->
        let comargs = String.concat " " !comargs in
        let com = sprintf "%s %s %s" !com comargs name in
        ignore (Sys.command com))
      names
  else
    let module T =
      Task
        (struct
          let com = !com
          let comargs = !comargs
          let verbose = !verbose
          let mode = !mode
        end) in
    T.run !j names


