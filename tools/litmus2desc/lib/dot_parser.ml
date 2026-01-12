type dir = Execution.dir

type regular_mem_event = {
  id : int;
  dir : dir;
  mem_loc : string;
  value : int;
  proc : int;
  poi : int;
  instruction : string;
}

type memory_event =
  | Init of { id : int; mem_loc : string; value : int }
  | Regular of regular_mem_event

type branch_type = Bcc | Pred

type branching_event = {
  id : int;
  ty : branch_type;
  proc : int;
  poi : int;
  instruction : string;
}

type register = { proc : int; name : string }
type reg_value = Scalar of int | Mem_loc of string

let pp_reg_value fmt =
  let open Format in
  function
  | Scalar v -> fprintf fmt "%d" v
  | Mem_loc loc -> fprintf fmt "%s" loc

type register_event = {
  id : int;
  dir : dir;
  reg : register;
  value : reg_value;
  proc : int;
  poi : int;
  instruction : string;
}

type fence_event = { id : int; proc : int; poi : int; instruction : string }

type event =
  | Memory of memory_event
  | Branching of branching_event
  | Register of register_event
  | Fence of fence_event

(* --- Helpers --- *)

let mevent_mem_loc = function Init ev -> ev.mem_loc | Regular ev -> ev.mem_loc
let mevent_value = function Init ev -> ev.value | Regular ev -> ev.value
let mevent_id = function Init ev -> ev.id | Regular ev -> ev.id
let mevent_dir = function Init _ -> Execution.W | Regular ev -> ev.dir

let event_id = function
  | Memory ev -> mevent_id ev
  | Branching ev -> ev.id
  | Register ev -> ev.id
  | Fence ev -> ev.id

(* Convert from Execution.t (JSON graph) to the local execution type. *)

let events_of_execution (e : Execution.t) : event list =
  let mem_loc_of_location = function
    | Execution.Global global_pretty -> global_pretty
    | Execution.Reg { reg_pretty; _ } -> reg_pretty
  in

  let reg_of_location = function
    | Execution.Reg { proc; reg_pretty } -> { proc; name = reg_pretty }
    | _ -> failwith "register event without register location"
  in

  let mem_value_of_value = function
    | Execution.Val (Execution.Concrete { scalar_pretty }) ->
        int_of_string scalar_pretty
    | Execution.Val (Execution.Symbolic _)
    | Execution.Val (Execution.Pteval _)
    | Execution.Var _ | Execution.Val_pretty _ ->
        failwith "non-concrete memory value not supported"
  in

  let reg_value_of_value = function
    | Execution.Val (Execution.Concrete { scalar_pretty }) ->
        Scalar (int_of_string scalar_pretty)
    | Execution.Val (Execution.Symbolic { symbol_pretty }) ->
        Mem_loc symbol_pretty
    | Execution.Val_pretty s -> (
        match int_of_string_opt s with Some i -> Scalar i | None -> Mem_loc s)
    | Execution.Var _ | Execution.Val (Execution.Pteval _) ->
        failwith "register value Var not supported"
  in

  e.events
  |> List.filter_map (fun (ev : Execution.event) ->
      match ev.iiid with
      | Execution.Spurious -> None
      | Execution.Init -> (
          match ev.act with
          | Execution.Memory ma ->
              let id = ev.eiid in
              let mem_loc = mem_loc_of_location ma.loc in
              let value = mem_value_of_value ma.value in
              Some (Memory (Init { id; mem_loc; value }))
          | Execution.Fault -> None
          | _ -> None)
      | Execution.Index { proc; poi; inst_pretty = instruction } -> (
          match ev.act with
          | Execution.Memory ma ->
              let id = ev.eiid in
              let mem_loc = mem_loc_of_location ma.loc in
              let value = mem_value_of_value ma.value in
              let dir = ma.dir in
              Some
                (Memory
                   (Regular { id; dir; mem_loc; value; proc; poi; instruction }))
          | Execution.Register ra ->
              let id = ev.eiid in
              let reg = reg_of_location ra.loc in
              let value = reg_value_of_value ra.value in
              Some
                (Register
                   { id; dir = ra.dir; reg; value; proc; poi; instruction })
          | Execution.Barrier { barrier_pretty } ->
              let id = ev.eiid in
              Some (Fence { id; proc; poi; instruction = barrier_pretty })
          | Execution.Commit { commit } ->
              let id = ev.eiid in
              let ty =
                match commit with
                | "bcc" -> Bcc
                | "pred" -> Pred
                | _ -> failwith "unknown commit type for branching"
              in
              Some (Branching { id; ty; proc; poi; instruction })
          | Execution.Fault -> None))

let is_load (ev : event) =
  match ev with
  | Memory (Regular ev) -> ev.dir = R
  | Register ev -> ev.dir = R
  | _ -> false
