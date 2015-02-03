open Printf

let opt = ref AutoOpt.default

let parse_arg d p s = match p s with
| Some m -> m
| None -> raise (Arg.Bad d)

let prog = Sys.argv.(0)

let restart = ref false
let interactive = ref false
let runaddr = ref None

open AutoOpt

let read_conf name = opt := AutoLex.conf !opt name

let speclist = 
  ("-v", Arg.Unit (fun () -> opt := incr_verbose !opt),"be verbose")::
  ("-version", Arg.Unit (fun () -> print_endline Version.version ; exit 0),
   " show version number and exit")::
  ("-switch", Arg.Set Misc.switch,"switch something")::
  ("-arch",
   Arg.String (fun s -> opt := set_arch s !opt),
   sprintf "<X86|PPC|ARM> architecture (default %s)"
     (Archs.pp default.arch))::
  ("-mode",
   Arg.String (fun s -> opt := set_mode s !opt),
   sprintf "<conform|explo> main mode (default %s)"
     (pp_mode default.mode))::
  ("-nprocs",
   Arg.Int (fun i -> opt := set_nprocs i !opt),
   sprintf
     "<n> generate tests up to <n> processors (defaults: X86=%i, PPC=%i, ARM=%i)"
     (get_nprocs Archs.X86 default)
     (get_nprocs Archs.PPC default)
     (get_nprocs Archs.ARM default)
  )::
  ("-restart", Arg.Set restart, "restart the experiment in hand")::
  ("-i", Arg.Set interactive, " force interactive mode for restarted experiment")::
  ("-addr", Arg.String (fun s -> runaddr := Some s),
   "<addr> override ip address of target machine for restarted experiment")::
  []

let main () =
  Arg.parse speclist read_conf
   (sprintf "Usage: %s [configfile]" prog);
  if !restart then begin
    let name = "." in
    let ropt,c =
      Misc.input_protect_gen
        open_in_bin
        (fun chan ->
          (Marshal.from_channel chan : AutoOpt.t * AutoRun.ckpt))
        (Filename.concat name AutoOpt.ckpt_name) in
    
    let opt =
      { ropt  with
        verbose = !opt.verbose ;
        force_interactive = false ;
      } in
    let opt =
      if !interactive then { opt with force_interactive = true ; }
      else opt in
    let opt = match opt.mach,!runaddr with
    | Cross (a1,_),Some a2 ->
        { opt with mach = Cross (a1,a2); }
    | Distant _,Some a ->
        { opt with mach = Distant a; }
    | _,_ -> opt in
    let m = AutoConf.mk_config opt in
    let module C = (val m : AutoConf.S) in
    let module M = AutoRun.Make(C) in
    match C.mode with
    | Explo -> M.restore_explo c
    | Conform -> M.restore_conform c
  end else begin
    let m = AutoConf.mk_config !opt in
    let module C = (val m : AutoConf.S) in
    let module M = AutoRun.Make(C) in
    match C.mode with
    | Explo -> M.go_explo ()
    | Conform -> M.go_conform ()
  end

let () =
  try main ()
  with Misc.Fatal msg ->
    eprintf "Fatal error: %s\n%!" msg ;
    exit 1

