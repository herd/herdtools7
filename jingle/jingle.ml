open Printf

let verbose = ref false
let map = ref None
let outdir = ref None
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
	    "<name> - give the theme file <name>";
	    "-o",Arg.String (fun s -> outdir := Some s),
	    "<name> - directory for output files"]
	   (fun s -> args := s :: !args)
	   (sprintf "Usage: %s [option]* -map <file> [test]*" prog)

let parsed = match !map with
  | None -> raise (Error "No map file provided.")
  | Some s -> Misc.input_protect ParseMap.parse s

let () = if !verbose then
	   (eprintf "Reading theme file :\n";
	    List.iter (fun (s,t) ->
		       eprintf "\"%s\" -> \"%s\"\n" s t)
		      parsed.ParseMap.conversions)
	     
module Source = (val Arch.get_arch parsed.ParseMap.source)
module Target = (val Arch.get_arch parsed.ParseMap.target)

module Trad = Mapping.Make(struct
			    module Source = Source
			    module Target = Target
			    let conversions = parsed.ParseMap.conversions
			  end)
			    
let do_trans file = 
  let fin chin =
    let sres = let module SP = Splitter.Make(Splitter.Default) in
	       SP.split (Filename.basename file) chin in
    let fout out = try
	let trans_test = Trad.translate chin sres in
	Target.Dumper.dump_info out sres.Splitter.name trans_test
      with e -> eprintf "Error in test %s.\n" (Filename.basename file);
		raise e
    in match !outdir with
       | None -> fout stdout
       | Some s -> 
	  Misc.output_protect fout (Filename.concat s 
					     (Filename.basename file))
  in Misc.input_protect fin file

let () = 
  let open Misc in
  match !args with
  | [] -> iter_stdin do_trans
  | tests -> iter_argv do_trans tests

