type loc = Loc of int
type proc = Proc of int

(* new_loc: get a new location
loc_count: get the current number of locations *)
let new_loc, loc_count =
  let counter = ref 0 in
  let inner_new_loc () =
    let loc = Loc !counter in
    let () = incr counter in loc
  in inner_new_loc, (fun () -> !counter)

(* new_proc: get a new proc
proc_count: get the current number of procs *)
let new_proc, proc_count =
  let counter = ref 0 in
  let inner_new_proc () =
    let p = Proc !counter in
    let () = incr counter in p
  in inner_new_proc, (fun () -> !counter)



(** memory event *)
type event = {
  mutable direction: Edge.direction option;
  mutable location: loc option;
  mutable proc: proc option;
  mutable value: int option;
}

(** cycle: Circular double linked list of edges/events *)
type t = {
  edge: Edge.t;
  mutable next: t;
  mutable prev: t;
  source_event: event;
}


(** Pretty printers **)

let pp_location (Loc i) =
  if i < 3 then String.make 1 ((Char.code 'x')+i |> Char.chr)
  else "loc_"^(string_of_int (i-3))

let pp_proc (Proc i) = "P"^string_of_int i

let pp_event evt =
  "("^
  (match evt.proc with Some p -> pp_proc p | None -> "*")^") "^
  (match evt.direction with Some d -> Edge.pp_direction d | None -> "*")^
  (match evt.location with Some l -> pp_location l | None -> "*")^" "^
  (match evt.value with Some v -> string_of_int v | None -> "*")

let pp_cycle cycle_start =
  let rec pp_aux node =
    pp_event node.source_event^" -"^Edge.pp_edge node.edge^"->\n"^
    if node.next == cycle_start then "" else pp_aux node.next
  in pp_aux cycle_start


(** Creating and assigning the various cycle values **)

let create_cycle edges =
  let empty_evt () = {direction=None; location=None; value=None; proc=None} in
  let rec create_aux first_node previous = function
    | [] -> first_node.prev <- previous; first_node
    | edge::eq -> let rec l = {
                    edge=edge;
                    source_event=empty_evt ();
                    prev=previous; next=l; (* l.next will soon be replaced *)
                  } in l.next <- create_aux first_node l eq; l
  in match edges with
  [] -> failwith "No edges"
  | e::q -> let rec l = {edge=e; source_event=empty_evt (); next=l; prev=l} in
            l.next <- create_aux l l q; l (* l.prev is replaced in the recursive call *)


(** Find the first node of the cycle satisfying a condition *)
let find_first cycle_start condition =
  let rec find_first_aux node =
    if condition node then
      node
    else if node.next != cycle_start then
      find_first_aux node.next
    else
      raise Not_found
  in find_first_aux cycle_start


(** Assign its direction to each event of the cycle *)
 let assign_directions cycle_start =
  let rec assign_aux node =
    let edge_dir1, _ = Edge.edge_direction node.edge in
    let _, edge_dir2 = Edge.edge_direction node.prev.edge in
    if edge_dir1 <> edge_dir2 then failwith (
      "Unable to unify edge direction "^Edge.pp_edge node.prev.edge^
      "-> ("^Edge.pp_direction edge_dir2^" <> "^Edge.pp_direction edge_dir1^") -"
      ^Edge.pp_edge node.edge
    );
    node.source_event.direction <- Some edge_dir1;
    if node.next != cycle_start then assign_aux node.next
  in assign_aux cycle_start


(** Assign a location to each event of the cycle *)
let assign_locations cycle_start =
  (* Find the first node after a different location *)
  let first_location = new_loc () in
  let first_node =
    try (find_first cycle_start (fun n -> Edge.edge_location n.edge = Edge.Different)).next
    with Not_found -> failwith "No location change in cycle"
  in

  let rec assign_aux node source_location =
    node.source_event.location <- Some source_location;
    if node.next != first_node then
      let loc = if Edge.edge_location node.edge = Edge.Different then new_loc () else source_location
      in assign_aux node.next loc
    else begin
      (* end of the cycle. We check that current edge location is Different, (as selected by find_first)
        we check that source and target loc indeed differ *)

      assert (Edge.edge_location node.edge = Edge.Different);

      if (node.source_event.location = node.next.source_event.location) then
        failwith ("Cannot get a changing location across "^Edge.pp_edge node.edge)
    end
  in assign_aux first_node first_location


(** Assign a proc. to each event of the cycle *)
let assign_procs cycle_start =
  let is_external = function
    | Edge.Rf ie | Edge.Fr ie | Edge.Ws ie -> ie=Edge.External
    | _ -> false
  in

  let first_proc = new_proc () in
  let first_node =
    try (find_first cycle_start (fun n -> is_external n.edge)).next
    with Not_found -> failwith "No location change in cycle"
  in

  let rec assign_aux node source_proc =
    node.source_event.proc <- Some source_proc;
    if node.next != first_node then
      let proc = if is_external node.edge then new_proc () else source_proc
      in assign_aux node.next proc
    else begin
      (* end of the cycle. We check that current edge is external, (as selected by find_first)
        we check that source and target proc indeed differ *)

      assert (is_external node.edge);

      if (node.source_event.proc = node.next.source_event.proc) then
        failwith ("Cannot get a changing proc across "^Edge.pp_edge node.edge)
    end
  in assign_aux first_node first_proc


(** Assign a value to each event, incrementing at each write *)
let assign_values cycle_start =
  let first_node =
    try find_first cycle_start (fun e -> e.source_event.location <> e.prev.source_event.location)
    with Not_found -> failwith "No location change in cycle"
  in
  let rec assign_values_aux node value = (* Uses of a location always follow directly each other *)
    let next_value = match node.source_event.direction with
    | None -> failwith ("Direction not assigned on the source of edge "^Edge.pp_edge node.edge)
    | Some (Edge.Wr|Edge.Wm) -> node.source_event.value <- Some (value+1); value+1
    | Some (Edge.Rr|Edge.Rm) -> node.source_event.value <- Some value; value
    in if node.next != first_node then begin
      assign_values_aux node.next (
        if Edge.edge_location node.edge = Edge.Different then 0 else next_value
      )
    end

  in assign_values_aux first_node 0



  let make_cycle edges =
  let cycle = create_cycle edges in
  "EDGES\n"^(pp_cycle cycle)^"\n"|> Utils.verbose_print;

  assign_directions cycle;
  "DIRECTIONS\n"^(pp_cycle cycle)^"\n"|> Utils.verbose_print;

  assign_locations cycle;
  "LOCATIONS\n"^(pp_cycle cycle)^"\n"|> Utils.verbose_print;

  assign_procs cycle;
  "PROCS\n"^(pp_cycle cycle)^"\n"|> Utils.verbose_print;

  assign_values cycle;
  "READ/WRITE VALUES\n"^(pp_cycle cycle)^"\n"|> Utils.verbose_print;

  cycle