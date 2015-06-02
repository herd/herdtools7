open Printf

let verbose = ref false
let map = ref None
let args = ref []
let prog =
  if Array.length Sys.argv > 0 
  then Sys.argv.(0)
  else "jingle"

exception Error of string

let () = Arg.parse
           ["-v",Arg.Unit (fun () -> verbose := true),
	    "- be verbose";
	    "-theme",Arg.String (fun s -> map := Some s),
	    "<name> - give the theme file <name>"]
	   (fun s -> args := s :: !args)
	   (sprintf "Usage: %s [option]* -map <file> [test]*" prog)

let parsed = match !map with
  | None -> raise (Error "No map file provided.")
  | Some s -> Misc.input_protect ParseMap.parse s

module Conf : Mapping.Config =
  struct
    module Source = (val Arch.get_arch parsed.ParseMap.source)
    module Target = (val Arch.get_arch parsed.ParseMap.target)
    module SParser = (val Arch.get_parser parsed.ParseMap.source)
    module TParser = (val Arch.get_parser parsed.ParseMap.target)
    let conversions = parsed.ParseMap.conversions
  end

let () = if !verbose then
	   eprintf "Reading theme file :\n";
	 List.iter (fun (s,t) ->
		    eprintf "\"%s\" -> \"%s\"\n" s t)
		   parsed.ParseMap.conversions

module Trad = Mapping.Make(Conf)
    
