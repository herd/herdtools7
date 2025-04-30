(** uoiam is miaou backwards *)

open Printf

module
  Make
   (O:sig
     val verbose : int
     val includes : string list
     val libdir : string
   end) = struct

open Earley_core

let () = ignore (O.includes) ; ignore (O.libdir)


type tag = And | Or

let rec get_tag = function
  | [] ->  Warn.fatal "No tag"
  | w::ws ->
      begin
        match Misc.lowercase w with
        | "one" -> Or
        | "all" -> And
        | _ -> get_tag ws
      end

type reduced =
  | Rel of string * (string * string)
  | Set of string * string

module FD = Finddef.Make(struct let verbose = O.verbose > 0 end)

let reduce ws =
  let name,args = FD.find ws in
  match args with
  | [| e |] -> Set (name,e)
  | [| e1; e2; |] -> Rel (name,(e1,e2))
  | _ -> assert false

type t =
  | Def of tag * string list * t list
  | Arg of reduced * string list
 
let parser define =
  | ws:words ":" args:args0 -> ( Def (get_tag ws,ws,args) )

and parser args0 =
  | xs:arg0+  -> ( xs )

and parser arg0 =
  | "o" ws:words "." -> ( Arg (reduce ws,ws) )
  | "o" ws:words ":" xs:args1 -> (Def (get_tag ws,ws,xs))

and parser args1 =
  | xs:arg1+ -> ( xs )

and parser arg1 =
  | "-" ws:words "." -> ( Arg (reduce ws,ws) )

and parser words = ws:word+ -> ( ws )
and parser word =
  | w:RE("[-/a-zA-Z]+") -> ( w )
  | e:"E" n:RE("[1-9]") -> ( e ^ n )

and parser main = define+ EOF

let pp_words chan ws =
  fprintf chan "[%s]" (String.concat "," ws)

let pp_tag = function
  | Or -> "Or"
  | And -> "And"
let pp_reduced chan = function
  | Rel (name,(e1,e2)) ->
      fprintf chan "%s(%s,%s)" name e1 e2
  | Set (name,e) ->
      fprintf chan "%s(%s)" name e

let rec do_pp_tree i chan  = function
  | Def (tag,ws,args) ->
      fprintf chan "%s<%s>%a\n" i (pp_tag tag) pp_words ws ;
      do_pp_trees ("  "^i) chan args ;
      if i = "" then fprintf chan "\n%!"
  | Arg (pp,_) -> fprintf chan "%s%a\n" i pp_reduced pp

and do_pp_trees i chan = List.iter (do_pp_tree i chan)

let pp_defs = do_pp_trees ""

let zyva chan =
  let tree =
    Earley.parse_channel main Blanks.default chan in
  if O.verbose > 0 then printf "%a\n%!" pp_defs tree
end

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "miaou7"

let verbose = ref 0
let libdir = ref (Filename.concat Version.libdir "herd")
let includes = ref []
let arg = ref None

let options =
  [
(* Basic *)
    ("-version", Arg.Unit
     (fun () -> printf "%s, Rev: %s\n" Version.version Version.rev ; exit 0),
   " show version number and exit") ;
    ("-libdir", Arg.Unit (fun () -> print_endline !libdir; exit 0),
    " show installation directory and exit");
    ("-set-libdir", Arg.String (fun s -> libdir := s),
    "<path> set installation directory to <path>");
    ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
    ("-q", Arg.Unit (fun _ -> verbose := -1 ),
   "<default> do not show diagnostics");
    ("-I", Arg.String (fun s -> includes := !includes @ [s]),
   "<dir> add <dir> to search path");
]

let () =
  Arg.parse
    options
    (fun s -> arg := Some s)
    (sprintf "Usage: %s [option] [file]" prog)

module Zyva =
  Make
    (struct
      let verbose = !verbose
      let libdir = !libdir
      let includes = !includes
    end)

let arg = !arg
let () =
  match arg with
  | None -> Zyva.zyva stdin
  | Some name -> Misc.input_protect Zyva.zyva name
