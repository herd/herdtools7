[@@@warning "-44"]

type edge = { label : string; source : string; target : string }
type dir = R | W
type atom = A | L | AX | LX

type regular_mem_event = {
  id : string;
  dir : dir;
  mem_loc : string;
  atom : atom option;
  value : int;
  proc : int;
  poi : int;
  instruction : string;
}

type memory_event =
  | Init of { id : string; mem_loc : string; value : int }
  | Regular of regular_mem_event

type branch_type = Bcc | Pred

type branching_event = {
  id : string;
  ty : branch_type;
  proc : int;
  poi : int;
  instruction : string;
}

type register = { proc : int; ix : int }
type reg_value = Scalar of int | Mem_loc of string

type register_event = {
  id : string;
  dir : dir;
  reg : register;
  value : reg_value;
  proc : int;
  poi : int;
  instruction : string;
}

type fence_event = { id : string; proc : int; poi : int; instruction : string }

type event =
  | Memory of memory_event
  | Branching of branching_event
  | Register of register_event
  | Fence of fence_event

type execution = { events : event list; edges : edge list }

(* --- Helpers --- *)

let mevent_mem_loc = function Init ev -> ev.mem_loc | Regular ev -> ev.mem_loc
let mevent_value = function Init ev -> ev.value | Regular ev -> ev.value
let mevent_id = function Init ev -> ev.id | Regular ev -> ev.id
let mevent_dir = function Init _ -> W | Regular ev -> ev.dir

let event_id = function
  | Memory ev -> mevent_id ev
  | Branching ev -> ev.id
  | Register ev -> ev.id
  | Fence ev -> ev.id

(* TODO: use splitdot7 for this *)
let split_digraph_blocks (src : string) : string list =
  let sep = "digraph G {" in
  let parts = CCString.split ~by:sep src in
  match parts with
  | [] | [ _ ] -> []
  | _ :: rest -> List.map (fun p -> sep ^ p) rest

let trim = String.trim

let split_on_l (s : string) : string list =
  (* DOT encodes line breaks inside labels as the literal "\l" substring. *)
  CCString.split ~by:{|\l|} s

(* --- Regexes --- *)

module Regex = struct
  open Re

  let node_id : t =
    let digits = rep1 digit in
    seq [ str "eiid"; digits ]

  let any_except : char -> t = fun c -> compl [ char c ]

  let node : re =
    seq
      [
        bol;
        group node_id;
        space;
        str "[label=\"";
        group (rep1 (any_except '"'));
        char '"';
      ]
    |> compile

  let edge : re =
    seq
      [
        bol;
        group node_id;
        str " -> ";
        group node_id;
        space;
        str "[label=\"";
        group (rep1 (any_except '"'));
        char '"';
      ]
    |> compile

  let lbl_grp : t = seq [ rep1 alnum; char ':'; space ]
  let dir : t = alt [ char 'R'; char 'W' ]

  let mem_event : re =
    seq
      [
        lbl_grp;
        group dir;
        char '[';
        group (rep1 alnum);
        char ']';
        group (opt (alt [ str "Acq"; str "Rel" ]));
        group (opt (char '*'));
        char '=';
        group (rep1 digit);
      ]
    |> compile

  let proc_poi : Re.re =
    let open Re in
    seq
      [
        str "proc:P"; group (rep1 digit); space; str "poi:"; group (rep1 digit);
      ]
    |> compile

  let reg_event : re =
    seq
      [
        lbl_grp;
        group dir;
        group (rep1 digit);
        char ':';
        char 'X';
        group (rep1 digit);
        (* is this 'q' always present? *)
        char 'q';
        char '=';
        group (rep1 alnum);
      ]
    |> compile

  let branching_event : re =
    seq [ lbl_grp; str "Branching"; group (alt [ str "(bcc)"; str "(pred)" ]) ]
    |> compile

  let fence_event : re = seq [ lbl_grp; group (rep1 any) ] |> compile
end

(* --- Parsing functions --- *)

let try_parse_branching_event ~(eiid : string) (parts : string list) :
    event option =
  match parts with
  | [ head; proc_poi; instruction ] ->
      let open CCOption.Infix in
      let* g = Re.exec_opt Regex.branching_event head in
      let branch_type_str = Re.Group.get g 1 in
      let* ty =
        match branch_type_str with
        | "(bcc)" -> Some Bcc
        | "(pred)" -> Some Pred
        | _ -> None
      in
      let* proc_poi_g = Re.exec_opt Regex.proc_poi proc_poi in
      let proc = int_of_string (Re.Group.get proc_poi_g 1) in
      let poi = int_of_string (Re.Group.get proc_poi_g 2) in
      Some (Branching { id = eiid; ty; proc; poi; instruction })
  | _ -> None

let parse_atom ~atom_al ~atom_x : atom option =
  match (atom_al, atom_x) with
  | "Acq", "" -> Some A
  | "Acq", "*" -> Some AX
  | "Rel", "" -> Some L
  | "Rel", "*" -> Some LX
  | _ -> None

let parse_dir (dir : string) : dir option =
  match dir with "R" -> Some R | "W" -> Some W | _ -> None

let try_parse_memory_event ~(eiid : string) (parts : string list) : event option
    =
  match parts with
  | head :: rest ->
      let open CCOption.Infix in
      let* head_g = Re.exec_opt Regex.mem_event head in
      let dir_str = Re.Group.get head_g 1 in
      let* dir = parse_dir dir_str in
      let mem_loc = Re.Group.get head_g 2 in
      let atom_al = Re.Group.get head_g 3 in
      let atom_x = Re.Group.get head_g 4 in
      let atom = parse_atom ~atom_al ~atom_x in
      let value_str = Re.Group.get head_g 5 in
      let value = int_of_string value_str in
      let id = eiid in
      let* mem_ev =
        match rest with
        | [ "Init" ] when dir = W && atom = None ->
            Some (Init { id; mem_loc; value })
        | [ proc_poi; instruction ] ->
            let* proc_poi_g = Re.exec_opt Regex.proc_poi proc_poi in
            let proc = int_of_string (Re.Group.get proc_poi_g 1) in
            let poi = int_of_string (Re.Group.get proc_poi_g 2) in
            Some
              (Regular { id; dir; mem_loc; atom; value; proc; poi; instruction })
        | _ -> None
      in
      Some (Memory mem_ev)
  | [] -> None

let try_parse_register_event ~(eiid : string) (parts : string list) :
    event option =
  match parts with
  | [ head; proc_poi; instruction ] ->
      let open CCOption.Infix in
      let* head_g = Re.exec_opt Regex.reg_event head in
      let dir_str = Re.Group.get head_g 1 in
      let* dir = parse_dir dir_str in
      let reg_proc = int_of_string (Re.Group.get head_g 2) in
      let reg_ix = int_of_string (Re.Group.get head_g 3) in
      let reg = { proc = reg_proc; ix = reg_ix } in
      let value_str = Re.Group.get head_g 4 in
      let value =
        match CCInt.of_string value_str with
        | Some scalar -> Scalar scalar
        | None -> Mem_loc value_str
      in
      let* proc_poi_g = Re.exec_opt Regex.proc_poi proc_poi in
      let proc = int_of_string (Re.Group.get proc_poi_g 1) in
      let poi = int_of_string (Re.Group.get proc_poi_g 2) in
      Some (Register { id = eiid; dir; reg; value; proc; poi; instruction })
  | _ -> None

let try_parse_fence_event ~(eiid : string) (parts : string list) =
  match parts with
  | [ head; proc_poi; instruction ] ->
      let open CCOption.Infix in
      let* head_instr_g = Re.exec_opt Regex.fence_event head in
      let head_instr = Re.Group.get head_instr_g 1 in
      let* _ = if head_instr = instruction then Some () else None in
      let* proc_poi_g = Re.exec_opt Regex.proc_poi proc_poi in
      let proc = int_of_string (Re.Group.get proc_poi_g 1) in
      let poi = int_of_string (Re.Group.get proc_poi_g 2) in
      Some (Fence { id = eiid; proc; poi; instruction })
  | _ -> None

let parse_node_from_label ~(eiid : string) (label_text : string) : event =
  let parts =
    split_on_l label_text |> List.map trim |> List.filter (fun s -> s <> "")
  in
  if List.length parts = 0 then
    failwith (Printf.sprintf "Empty label for node %s" eiid)
  else
    let node =
      CCOption.choice
        [
          try_parse_memory_event ~eiid parts;
          try_parse_register_event ~eiid parts;
          try_parse_branching_event ~eiid parts;
          try_parse_fence_event ~eiid parts;
        ]
    in
    CCOption.get_exn_or "failed to parse node" node

let parse_execution (block : string) : execution =
  let events =
    CCString.lines block
    |> List.fold_left
         (fun acc line ->
           match Re.exec_opt Regex.node line with
           | Some g ->
               let eiid = Re.Group.get g 1 in
               let label_text = Re.Group.get g 2 in
               let evt = parse_node_from_label ~eiid label_text in
               evt :: acc
           | None -> acc)
         []
    |> List.rev
  in
  let edges =
    CCString.lines block
    |> List.fold_left
         (fun acc line ->
           match Re.exec_opt Regex.edge line with
           | Some g ->
               let source = Re.Group.get g 1 in
               let target = Re.Group.get g 2 in
               let label = Re.Group.get g 3 in
               { label; source; target } :: acc
           | None -> acc)
         []
    |> List.rev
  in
  { events; edges }

(* --- Graph representation --- *)

module G = struct
  module Int = struct
    type t = int

    let compare = Int.compare
    let hash = Hashtbl.hash
    let equal = Int.equal
  end

  module String = struct
    type t = string

    let compare = String.compare
    let default = ""
  end

  include Graph.Imperative.Digraph.ConcreteLabeled (Int) (String)
end

module Simple_paths (G : Graph.Sig.G) = struct
  module V = G.V
  module E = G.E
  module VS = Set.Make (V)

  let get_label : E.t -> E.label = E.label

  let iter_simple_paths (g : G.t) ~(src : V.t) ~(dst : V.t)
      ~(pred : E.label -> bool) (yield : V.t list -> unit) =
    let rec dfs visited path v =
      if V.equal v dst then yield (List.rev (v :: path))
      else
        G.iter_succ
          (fun w ->
            let ed : E.t = G.find_edge g v w in
            let lbl = get_label ed in
            if (not (VS.mem w visited)) && pred lbl then
              dfs (VS.add w visited) (v :: path) w)
          g v
    in
    dfs (VS.singleton src) [] src
end

let get_int_id ev_id =
  CCString.chop_prefix ~pre:"eiid" ev_id
  |> CCOption.get_exn_or "malformed event id"
  |> int_of_string

let iter_simple_paths ~(f : string list -> unit) ~(pred : string -> bool)
    ~(src : string) ~(dst : string) (g : G.t) =
  let module SP = Simple_paths (G) in
  let src = get_int_id src in
  let dst = get_int_id dst in
  let f int_l =
    int_l |> List.map (fun int_id -> Format.sprintf "eiid%d" int_id) |> f
  in
  SP.iter_simple_paths ~src ~dst ~pred g f

let simple_paths_iter ~(src : string) ~(dst : string) ~(pred : string -> bool)
    (g : G.t) : string list Util.Iter.t =
 fun f -> iter_simple_paths ~f ~src ~dst ~pred g

(* --- Public API --- *)

let to_dgraph (exec : execution) : G.t =
  let g = G.create () in
  let () =
    exec.events
    |> List.iter (fun ev ->
        let int_id = get_int_id (event_id ev) in
        G.add_vertex g int_id)
  in
  let () =
    exec.edges
    |> List.iter (fun ed ->
        let source_id = get_int_id ed.source in
        let target_id = get_int_id ed.target in
        G.add_edge_e g (G.E.create source_id ed.label target_id))
  in
  g

let parse_dot (content : string) : execution list =
  let blocks = split_digraph_blocks content in
  List.map parse_execution blocks
