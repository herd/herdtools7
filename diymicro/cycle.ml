open Edge

type loc = Loc of int
type proc = Proc of int

(* memory event *)
type event = {
  mutable direction: direction option;
  mutable location: loc option;
  mutable proc: proc option;
  mutable value: int option;
}

(* Circular double linked list of edges/events *)
type cycle = {
  edge: edge;
  mutable next: cycle;
  mutable prev: cycle;
  source_event: event;
}


(* new_loc: assign a new location
loc_count: get the current number of locations *)
let new_loc, loc_count =
  let counter = ref 0 in
  let inner_new_loc () =
    let loc = Loc !counter in
    let () = incr counter in loc
  in inner_new_loc, (fun () -> !counter)

let pp_location (Loc i) =
  if i < 3 then String.make 1 ((Char.code 'x')+i |> Char.chr)
  else "loc_"^(string_of_int (i-3))

let pp_proc (Proc i) = "P"^string_of_int i

let pp_event evt =
  "("^
  (match evt.proc with Some p -> pp_proc p | None -> "*")^") "^
  (match evt.direction with Some d -> pp_direction d | None -> "*")^
  (match evt.location with Some l -> pp_location l | None -> "*")^" "^
  (match evt.value with Some v -> string_of_int v | None -> "*")

let pp_cycle cycle_start =
  let rec pp_aux node =
    pp_event node.source_event^" -"^pp_edge node.edge^"->\n"^
    if node.next == cycle_start then "" else pp_aux node.next
  in pp_aux cycle_start

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
    let edge_dir1, _ = edge_direction node.edge in
    let _, edge_dir2 = edge_direction node.prev.edge in
    if edge_dir1 <> edge_dir2 then failwith (
      "Unable to unify edge direction "^pp_edge node.prev.edge^
      "-> ("^pp_direction edge_dir2^" <> "^pp_direction edge_dir1^") -"
      ^pp_edge node.edge
    );
    node.source_event.direction <- Some edge_dir1;
    if node.next != cycle_start then assign_aux node.next
  in assign_aux cycle_start


(** Assign a location to each event of the cycle *)
let assign_locations cycle_start =
  (* Find the first node after a different location *)
  let first_location = new_loc () in
  let first_node =
    try (find_first cycle_start (fun n -> edge_location n.edge = Different)).next
    with Not_found -> failwith "No location change in cycle"
  in

  let rec assign_aux node source_location =
    node.source_event.location <- Some source_location;
    if node.next != first_node then
      let loc = if edge_location node.edge = Different then new_loc () else source_location
      in assign_aux node.next loc
    else begin
      (* end of the cycle. We check that current edge location is Different, (as selected by find_first)
        we check that source and target loc indeed differ *)

      assert (edge_location node.edge = Different);

      if (node.source_event.location = node.next.source_event.location) then
        failwith ("Cannot get a changing location across "^pp_edge node.edge)
    end
  in assign_aux first_node first_location


(** Assign a proc. to each event of the cycle *)
let assign_procs cycle_start =
  let proc_count = ref 0 in
  let new_proc () =
    let p = Proc !proc_count
    in let () = incr proc_count
    in p
  in let is_external = function
    | Rf ie | Fr ie | Ws ie -> ie=External
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

      (* this is independent of program input and should always be verified *)
      assert (is_external node.edge);

      if (node.source_event.proc = node.next.source_event.proc) then
        failwith ("Cannot get a changing proc across "^pp_edge node.edge)
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
    | None -> failwith ("Direction not assigned on the source of edge "^pp_edge node.edge)
    | Some (Wr|Wm) -> node.source_event.value <- Some (value+1); value+1
    | Some (Rr|Rm) -> node.source_event.value <- Some value; value
    in if node.next != first_node then begin
      assign_values_aux node.next (
        if edge_location node.edge = Different then 0 else next_value
      )
    end

  in assign_values_aux first_node 0


let make edges =
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