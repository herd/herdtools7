type proc = Proc of int

module A = struct
  include AArch64_compile
end

(* new_proc: get a new proc
proc_count: get the current number of procs *)
let new_proc, proc_count =
  let counter = ref 0 in
  let inner_new_proc () =
    let p = Proc !counter in
    let () = incr counter in
    p
  in
  inner_new_proc, fun () -> !counter

type event = {
  annot : Edge.annot;
  direction : Edge.direction;
  mutable location : A.loc option;
  mutable proc : proc option;
  mutable value : int option;
  mutable is_significant : bool;
      (* significant reads will be added to final conditions *)
}
(** memory event *)

type t = {
  edge : Edge.t;
  mutable next : t;
  mutable prev : t;
  source_event : event;
}
(** cycle: Circular double linked list of edges/events *)

(** Pretty printers **)
let pp_proc (Proc i) = "P" ^ string_of_int i

let pp_event evt =
  "("
  ^ (match evt.proc with Some p -> pp_proc p | None -> "*")
  ^ ") "
  ^ Edge.pp_direction evt.direction
  ^
  match evt.direction with
  | Edge.Rr | Edge.Wr -> "   "
  | _ -> (
      ":"
      ^ (match evt.location with Some l -> A.pp_location l | None -> "*")
      ^ " "
      ^ match evt.value with Some v -> string_of_int v | None -> "*")

let pp_cycle cycle_start =
  let rec pp_aux node =
    pp_event node.source_event ^ " -" ^ Edge.pp_edge node.edge
    ^ (let annot = node.next.source_event.annot in
       if annot <> Edge.AnnotNone then ":" ^ Edge.pp_annot annot else "")
    ^ "->\n"
    ^ if node.next == cycle_start then "" else pp_aux node.next
  in
  pp_aux cycle_start

(** Creating and assigning the various cycle values **)

let create_cycle edges =
  let new_evt direction annot =
    {
      annot;
      direction;
      location = None;
      value = None;
      proc = None;
      is_significant = false;
    }
  in
  let rec create_aux first_node previous prev_annot = function
    | [] ->
        first_node.prev <- previous;
        first_node
    | (edge, annot) :: eq ->
        let src_dir, _ = Edge.edge_direction edge in
        let rec this_node =
          {
            edge;
            source_event = new_evt src_dir prev_annot;
            prev = previous;
            next = this_node;
            (* l.next will soon be replaced *)
          }
        in
        this_node.next <- create_aux first_node this_node annot eq;
        this_node
  in
  match edges with
  | [] -> Warn.fatal "No edges"
  | (edge, annot) :: q ->
      let _, last_annot = Utils.list_last q in
      let src_dir, _ = Edge.edge_direction edge in
      let rec first_node =
        {
          edge;
          source_event = new_evt src_dir last_annot;
          next = first_node;
          prev = first_node;
        }
      in
      first_node.next <- create_aux first_node first_node annot q;
      first_node (* first_node.prev is replaced in the recursive call *)

(** Find the first node of the cycle satisfying a condition *)
let find_first cycle_start condition =
  let rec find_first_aux node =
    if condition node then node
    else if node.next != cycle_start then find_first_aux node.next
    else raise Not_found
  in
  find_first_aux cycle_start

let find_last cycle_start condition =
  let rec find_last_aux node =
    if condition node then node
    else if node.prev != cycle_start then find_last_aux node.prev
    else raise Not_found
  in
  find_last_aux cycle_start

(** Ensure directions compatibility between edges *)
let check_directions cycle_start =
  let rec assign_aux node =
    let edge_dir1, _ = Edge.edge_direction node.edge in
    let _, edge_dir2 = Edge.edge_direction node.prev.edge in
    if edge_dir1 <> edge_dir2 then
      Warn.fatal "Incompatible directions %s -> (%s <> %s) -%s"
        (Edge.pp_edge node.prev.edge)
        (Edge.pp_direction edge_dir2)
        (Edge.pp_direction edge_dir1)
        (Edge.pp_edge node.edge);
    if node.next != cycle_start then assign_aux node.next
  in
  assign_aux cycle_start

(** Assign a location to each event of the cycle *)
let assign_locations cycle_start =
  (* Find the first node after a different location *)
  let first_location = A.next_loc () in
  let first_node =
    try
      (find_last cycle_start.prev (fun n ->
           Edge.edge_location n.edge = Edge.Different))
        .next
    with Not_found -> Warn.fatal "No location change in cycle"
  in

  let rec assign_aux node source_location =
    node.source_event.location <- Some source_location;
    if node.next != first_node then
      let loc =
        if Edge.edge_location node.edge = Edge.Different then A.next_loc ()
        else source_location
      in
      assign_aux node.next loc
    else (
      (* end of the cycle. We check that current edge location is Different, (as selected by find_first)
        we check that source and target loc indeed differ *)
      assert (Edge.edge_location node.edge = Edge.Different);

      if node.source_event.location = node.next.source_event.location then
        Warn.fatal "Cannot get a changing location across "
          (Edge.pp_edge node.edge))
  in
  assign_aux first_node first_location

(** Assign a proc. to each event of the cycle *)
let assign_procs cycle_start =
  let is_external = function
    | Edge.Rf ie | Edge.Fr ie | Edge.Ws ie -> ie = Edge.External
    | Edge.Iico i -> i.Edge.ie = Edge.External
    | _ -> false
  in

  let first_proc = new_proc () in
  let first_node =
    try (find_first cycle_start.prev (fun n -> is_external n.edge)).next
    with Not_found -> Warn.fatal "No location change in cycle"
  in

  let rec assign_aux node source_proc =
    node.source_event.proc <- Some source_proc;
    if node.next != first_node then
      let proc = if is_external node.edge then new_proc () else source_proc in
      assign_aux node.next proc
    else (
      (* end of the cycle. We check that current edge is external, (as selected by find_first)
        we check that source and target proc indeed differ *)
      assert (is_external node.edge);

      if node.source_event.proc = node.next.source_event.proc then
        Warn.fatal "Cannot get a changing proc across %s"
          (Edge.pp_edge node.edge))
  in
  assign_aux first_node first_proc

(** Assign a value to each event, incrementing at each write *)
let assign_values cycle_start =
  let first_node =
    try
      find_first cycle_start (fun e ->
          e.source_event.location <> e.prev.source_event.location)
    with Not_found -> Warn.fatal "No location change in cycle"
  in
  let rec assign_values_aux node value =
    (* Uses of a location always follow directly each other *)
    let next_value =
      match node.source_event.direction with
      | Edge.Wm ->
          node.source_event.value <- Some (value + 1);
          value + 1
      | Edge.Rm ->
          node.source_event.value <- Some value;
          value
      | _ -> value (* Rr and Wr don't get an assigned value *)
    in
    if node.next != first_node then
      assign_values_aux node.next
        (if Edge.edge_location node.edge = Edge.Different then 0 else next_value)
  in
  assign_values_aux first_node 0

(** Mark significant read (ie R -Fr-> or -Rf-> R) as such *)
let set_significant_reads first_node =
  let rec set_significant_reads node =
    if node.next != first_node then set_significant_reads node.next;
    match node.prev.edge, node.edge with
    | Edge.Rf _, _
    | _, Edge.Fr _
    | Edge.Iico Edge.{significant_dest = true}, _
    | _, Edge.Iico Edge.{significant_source = true} ->
        node.source_event.is_significant <- true
    | _ -> ()
  in
  set_significant_reads first_node

let make_cycle edges =
  let cycle = create_cycle edges in
  "EDGES\n" ^ pp_cycle cycle ^ "\n" |> Utils.verbose_print;

  check_directions cycle;

  assign_locations cycle;
  "LOCATIONS\n" ^ pp_cycle cycle ^ "\n" |> Utils.verbose_print;

  assign_procs cycle;
  "PROCS\n" ^ pp_cycle cycle ^ "\n" |> Utils.verbose_print;

  assign_values cycle;
  "READ/WRITE VALUES\n" ^ pp_cycle cycle ^ "\n" |> Utils.verbose_print;

  set_significant_reads cycle;

  cycle
