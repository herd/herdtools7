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

    let do_test name (byName,byHash,all as k) =
      try
        let {T.tname; hash; } as i = Z.from_file name in
        let hash = match hash with
        | None -> assert false
        | Some h -> h in
        add tname hash byName,add hash tname byHash
      with
      | Misc.Exit -> k
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg ;
          k
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
          raise e

    let zyva tests =
      let byName,byHash,_ =
        Misc.fold_argv do_test tests (StringMap.empty,StringMap.empty,[]) in
      StringMap.iter
        (fun tname hashes ->
          if not (InfoSet.is_singleton hashes) then
            eprintf "Error: name %s has different hashes\n"  name)
        byName ;
      StringMap.iter
        (fun _hash names ->
          if not (InfoSet.is_singleton names) then
            eprintf "Warning: names {%s} are the same test\n"
              (StringSet.pp_str "," names))
        byHash ;
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
