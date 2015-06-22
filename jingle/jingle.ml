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
    
module Dumper = SimpleDumper.Make(struct
            module A = Target

            let dump_loc = MiscParser.dump_location

            let dump_state_atom a =
              MiscParser.dump_state_atom dump_loc SymbConstant.pp_v a

            type state = MiscParser.state

            let dump_state st =
              String.concat " "
                (List.map
                   (fun a -> sprintf "%s;" (dump_state_atom a))
                   st)

                
            type constr = MiscParser.constr
            let dump_atom a =
              let open ConstrGen in
              match a with
              | LV (loc,v) -> dump_state_atom (loc,(MiscParser.TyDef,v))
              | LL (loc1,loc2) ->
                  sprintf "%s=%s" (dump_loc loc1) (MiscParser.dump_rval loc2)

            let dump_constr = ConstrGen.constraints_to_string dump_atom

            type location = MiscParser.location
            let dump_location = dump_loc
          end)
			    
let do_trans file = 
  let chin = open_in file in
  let sres = let module SP = Splitter.Make(Splitter.Default) in
	     SP.split file chin 
  in
  let f out = try
      let trans_test = Trad.translate chin sres in
      Dumper.dump_info out sres.Splitter.name trans_test
    with e -> eprintf "Error in test %s.\n" file;
	      raise e
  in match !outdir with
     | None -> f stdout
     | Some s -> 
	Misc.output_protect f (Filename.concat s (Filename.basename file))  

let () = 
  let open Misc in
  match !args with
  | [] -> iter_stdin do_trans
  | tests -> iter_argv do_trans tests

