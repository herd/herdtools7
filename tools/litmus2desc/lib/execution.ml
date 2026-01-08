type event_ref = { eiid : int }
type rel_edge = { src : event_ref; tgt : event_ref }
type location = Reg of { proc : int; reg_pretty : string } | Global of string

let pp_event_ref fmt (ref : event_ref) =
  Format.fprintf fmt "{ eiid = %d }" ref.eiid

let pp_rel_edge fmt (ed : rel_edge) =
  Format.fprintf fmt "%a -> %a" pp_event_ref ed.src pp_event_ref ed.tgt

let location_equal (l1 : location) (l2 : location) : bool = l1 = l2

let pp_location fmt : location -> unit =
  let open Format in
  function
  | Reg { proc; reg_pretty } -> fprintf fmt "Reg (%d, %s)" proc reg_pretty
  | Global global_pretty -> fprintf fmt "Global (%s)" global_pretty

type dir = R | W

type cst =
  | Concrete of { scalar_pretty : string }
  | Symbolic of { symbol_pretty : string }
  | Pteval of { pteval_pretty : string; as_physical : string option }

type value = Var of int | Val of cst | Val_pretty of string

type mem_action = {
  dir : dir;
  loc : location;
  value : value;
  is_implicit : bool;
  is_atomic : bool;
}

type reg_action = { dir : dir; loc : location; value : value }

type action =
  | Memory of mem_action
  | Register of reg_action
  | Barrier of { barrier_pretty : string }
  | Commit of { commit : string }
  | Fault

type instr_index = { proc : int; poi : int; inst_pretty : string }
type iiid = Init | Spurious | Index of instr_index
type event = { act : action; eiid : int; iiid : iiid }

module RFMap = struct
  type src = Final of location | Load of event_ref
  type tgt = Init | Store of event_ref
  type edge = { src : src; tgt : tgt }

  let pp_tgt fmt =
    let open Format in
    function
    | Init -> fprintf fmt "Init"
    | Store ref -> fprintf fmt "Store (%a)" pp_event_ref ref
end

type viewed_before = (string * rel_edge list) list

type t = {
  events : event list;
  iico_data : rel_edge list;
  iico_ctrl : rel_edge list;
  speculated : event_ref list;
  rfmap : RFMap.edge list;
  viewed_before : viewed_before;
  visible_po : rel_edge list option;
}
