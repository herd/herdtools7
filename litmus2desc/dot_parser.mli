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

val parse_dot : string -> execution list
val mevent_mem_loc : memory_event -> string
val mevent_value : memory_event -> int
val mevent_id : memory_event -> string
val mevent_dir : memory_event -> dir
val event_id : event -> string

module G :
  Graph.Sig.I
    with type V.t = int
     and type V.label = int
     and type E.t = int * string * int
     and type E.label = string

val to_dgraph : execution -> G.t

val iter_simple_paths :
  f:(string list -> unit) ->
  pred:(string -> bool) ->
  src:string ->
  dst:string ->
  G.t ->
  unit

val simple_paths_iter :
  src:string ->
  dst:string ->
  pred:(string -> bool) ->
  G.t ->
  string list Util.Iter.t
