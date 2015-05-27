open Printf

module Top
    (Opt:
       sig
         val verbose : bool
       end) =
  struct

    module T = struct
      type t = 
        { tname : string ;
          hash : string option; } 
    end

    module Make(A:ArchBase.S) = struct

      let zyva name parsed =
	let tname = name.Name.name in
	let hash = MiscParser.get_hash parsed in
	if Opt.verbose 
	then eprintf "Name=%s\nHash=%s\n\n"
			   tname
			    (match hash with 
			    | None -> "none" 
			    | Some h -> h);
        { T.tname = tname ;
          hash = hash; }
    end

    module Z = ToolParse.Top(T)(Make)

    let add k v m =
      let old = StringMap.safe_find StringSet.empty k m in
      StringMap.add k (StringSet.add v old) m

    let do_test name (byName,byHash,fnames as k) =
      try
        let {T.tname; hash; } = Z.from_file name in
        let hash = match hash with
        | None -> assert false
        | Some h -> h in
        add tname hash byName,add hash tname byHash,add tname name fnames
      with
      | Misc.Exit -> k
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg ;
          k
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
          raise e

    let is_singleton s  = match StringSet.as_singleton s with
    | Some _ -> true
    | None -> false

    let zyva tests =
      let byName,byHash,fnames =
        Misc.fold_argv do_test tests (StringMap.empty,StringMap.empty,StringMap.empty) in
      StringMap.iter
        (fun tname hashes ->
          if not (is_singleton hashes) then
            eprintf "Error: name %s has different hashes\n"  tname)
        byName ;
      StringMap.iter
        (fun _hash names ->
          if not (is_singleton names) then
            eprintf "Warning: tests {%s} are the same test\n"
              (StringSet.pp_str "," Misc.identity names))
        byHash ;
      StringMap.iter
        (fun name fnames ->
          if not (is_singleton fnames) then begin
            eprintf "Warning: test %s is referenced more than once:\n"
              name ;
            StringSet.iter
              (fun fname -> eprintf "  %s\n" fname)
              fnames
          end)
        fnames ;
      ()
  end


let verbose = ref false
let arg = ref []
let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mdiag"

let () =
  Arg.parse
    ["-v",Arg.Unit (fun () -> verbose := true), " be verbose";]
    (fun s -> arg := !arg @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)

let tests = List.rev !arg

module X =
  Top
    (struct
      let verbose = !verbose
    end)

let () = X.zyva tests
