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

val pp_reg_value : Format.formatter -> reg_value -> unit

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

val events_of_execution : Execution.t -> event list
val mevent_mem_loc : memory_event -> string
val mevent_value : memory_event -> int
val mevent_id : memory_event -> int
val mevent_dir : memory_event -> dir
val event_id : event -> int
val is_load : event -> bool
