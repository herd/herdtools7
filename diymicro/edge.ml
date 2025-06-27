module A = struct
  include AArch64_compile
end

(** Type definitions *)

(** Register event, Memory read/write (true if an edge does the event) *)
type direction = RegEvent | Rm of bool | Wm of bool

(** Change process ? *)
type int_ext = Internal | External

(** Change location ? *)
type sd = Same | Different

(** Dp.. dependencies *)
type dp = Addr | Data | Ctrl

(** memory event annotation *)
type annot = AnnotNone | A | L | X

(** dependencies from a compilation function to the next one,
    register+optionally its known (as in final-condition verified) value *)
type node_dep =
  | DepAddr of A.reg * int option
  | DepData of A.reg * int option
  | DepCtrl of A.reg * int option
  | DepReg of A.reg * int option
  | DepNone

type event_data = A.loc * int * annot * bool
(** essential event data, passed to iico edges wanting to compile an event
    - location of the event
    - value
    - annotation
    - is_significant *)

type iico_edge = {
  mutable repr : string;
      (* Expected to be "" in an add_iico call, replaced later by get_iico *)
  compile_edge :
    A.state ->
    node_dep ->
    event_data option ->
    event_data option ->
    int A.kinstruction A.kpseudo list * node_dep * A.state;
  direction : direction * direction;
  ie : int_ext;
  sd : sd;
  significant_source : bool;
  significant_dest : bool;
}

type iico = {
  instruction_name : string;
  to_edge : string -> string -> iico_edge;
  inputs : string list;
  outputs : string list;
}

type t =
  | Rf of int_ext
  | Fr of int_ext
  | Ws of int_ext
  | Po of sd * direction * direction
  | Dp of dp * sd * direction * direction
  | Dmb of A.mBReqTypes * sd * direction * direction
  | RfReg
  | Iico of iico_edge

(** edge attributes *)
let edge_direction = function
  | Rf _ -> Wm false, Rm false
  | Fr _ -> Rm false, Wm false
  | Ws _ -> Wm false, Wm false
  | Po (_, dir1, dir2) | Dp (_, _, dir1, dir2) | Dmb (_, _, dir1, dir2) ->
      dir1, dir2
  | RfReg -> Rm false, RegEvent
  | Iico i -> i.direction

let edge_location = function
  | RfReg -> Different
  | Rf _ | Fr _ | Ws _ -> Same
  | Po (sd, _, _) | Dp (_, sd, _, _) | Dmb (_, sd, _, _) -> sd
  | Iico i -> i.sd

let iico_ht = Hashtbl.create 10
let add_iico iico = Hashtbl.add iico_ht iico.instruction_name iico

(** look-up an iico by instruction name *)
let get_iico s = Hashtbl.find iico_ht s

let iico_to_edge iico src dst =
  let _ = List.find (( = ) src) iico.inputs in
  let _ = List.find (( = ) dst) iico.outputs in
  let edge = iico.to_edge src dst in
  edge.repr <- iico.instruction_name ^ " " ^ src ^ "->" ^ dst;
  Iico edge

(** look-up an iico_edge by instruction name, src and dst*)
let get_iico_edge s src dst = iico_to_edge (get_iico s) src dst

let list_iico_edges () =
  Hashtbl.fold
    (fun k i acc ->
      Printf.sprintf "iico[%s {%s}->{%s}]\n" k
        (String.concat "," i.inputs)
        (String.concat "," i.outputs)
      :: acc)
    iico_ht []
  |> List.sort String.compare |> String.concat "" |> print_string

let dependency_reg = function
  | DepAddr (r, _) -> r
  | DepData (r, _) -> r
  | DepCtrl (r, _) -> r
  | DepReg (r, _) -> r
  | DepNone -> Warn.fatal "Unable to get register of dependency DepNone"

(** Pretty printers *)

let pp_direction = function RegEvent -> "r" | Rm _ -> "R" | Wm _ -> "W"
let pp_int_ext = function Internal -> "i" | External -> "e"
let pp_sd = function Same -> "s" | Different -> "d"
let pp_dp = function Addr -> "Addr" | Data -> "Data" | Ctrl -> "Ctrl"
let pp_annot = function AnnotNone -> "" | A -> "A" | L -> "L" | X -> "X"

let pp_node_dep =
  let pp_int_option = function None -> "" | Some v -> "=" ^ string_of_int v in
  function
  | DepAddr (r, v) -> "Addr " ^ A.pp_reg r ^ pp_int_option v
  | DepData (r, v) -> "Data " ^ A.pp_reg r ^ pp_int_option v
  | DepCtrl (r, v) -> "Ctrl " ^ A.pp_reg r ^ pp_int_option v
  | DepReg (r, v) -> "Reg " ^ A.pp_reg r ^ pp_int_option v
  | DepNone -> "None"

let pp_edge = function
  | Rf ie -> "Rf" ^ pp_int_ext ie
  | Fr ie -> "Fr" ^ pp_int_ext ie
  | Ws ie -> "Fr" ^ pp_int_ext ie
  | Po (sd, dir1, dir2) ->
      "Po" ^ pp_sd sd ^ pp_direction dir1 ^ pp_direction dir2
  | Dp (dp, sd, Rm _, dir) -> "Dp" ^ pp_dp dp ^ pp_sd sd ^ pp_direction dir
  | Dp (dp, sd, dir1, dir2) ->
      "Dp" ^ pp_dp dp ^ pp_sd sd ^ pp_direction dir1 ^ pp_direction dir2
  | Dmb (b_type, sd, dir1, dir2) ->
      "DMB"
      ^ (if b_type <> A.FULL then "." else "")
      ^ A.pp_type b_type ^ pp_sd sd ^ pp_direction dir1 ^ pp_direction dir2
  | RfReg -> "Rf-reg"
  | Iico i -> "iico[" ^ i.repr ^ "]"

let pp_edges edges = List.map pp_edge edges |> String.concat " -> "

let pp_annotated_edge (edge, annot) =
  match annot with
  | AnnotNone -> pp_edge edge
  | _ -> pp_edge edge ^ ":" ^ pp_annot annot

let pp_quoted_annotated_edge ae =
  match ae with
  | Iico _, _ -> "'" ^ pp_annotated_edge ae ^ "'"
  | _ -> pp_annotated_edge ae
