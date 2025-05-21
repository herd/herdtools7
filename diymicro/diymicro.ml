let rec pp_directions = function
  | [e] -> Edge.pp_direction e
  | e::q -> Edge.pp_direction e^" -> "^pp_directions (q)
  | _ -> ""

let rec pp_dirloc = function
  | [e] -> Cycle.pp_location e
  | e::q -> Cycle.pp_location e^" -> "^pp_dirloc (q)
  | _ -> ""

let rec pp_cycle directions locations procs edges = match directions, locations, procs, edges with
  | [], [], [], [] -> ""
  | d::dq, l::lq, p::pq, e::eq ->
    "("^Cycle.pp_proc p^") "
    ^Edge.pp_direction d^" "^Cycle.pp_location l^" --"
    ^Edge.pp_edge e^"-> "^(pp_cycle dq lq pq eq)
  | _ -> assert false


let () =
  let print_and_newline string = string^"\n" |> print_string in

  let edges = List.init (Array.length Sys.argv - 1) (fun i -> 
    Lexing.from_string Sys.argv.(i+1)
    |> Parser.main Lexer.token
  ) in
  Edge.pp_edges edges |> print_and_newline;

  let directions = Cycle.assign_directions edges in
  pp_directions directions |> print_and_newline;

  let locations = Cycle.emit_locations edges in
  pp_dirloc locations |> print_and_newline;

  let procs = Cycle.emit_procs edges in
  pp_cycle directions locations procs edges |> print_and_newline