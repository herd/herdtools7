(** Type definitions *)

(* Intrinsic event and edge *)
type ievent = string
type iedge = ievent * ievent

(** Register event, Memory read (true if an edge does the event), Memory Write
    (true if an edge does the event) *)
type direction = RegEvent | Rm of bool | Wm of bool

(* Change process ? *)
type int_ext = Internal | External

(* Change location ? *)
type sd = Same | Different
type dp = Addr | Data | Ctrl | Reg

(** memory event annotation *)
type annot = AnnotNone | A | L | X

type node_dep =
  | DepAddr of AArch64_compile.reg * int option
  | DepData of AArch64_compile.reg * int option
  | DepCtrl of AArch64_compile.reg * int option
  | DepReg of AArch64_compile.reg * int option
  | DepNone

type event_data = AArch64_compile.loc * int * annot
(** essential event data, passed to iico edges wanting to compile an event *)

type iico = {
  repr : string;
  compile_edge :
    AArch64_compile.state ->
    node_dep ->
    event_data option ->
    event_data option ->
    int AArch64Base.kinstruction AArch64_compile.kpseudo list
    * node_dep
    * AArch64_compile.state;
  direction : direction * direction;
  ie : int_ext;
  sd : sd;
  significant_source : bool;
  significant_dest : bool;
}

type t =
  | Rf of int_ext
  | Fr of int_ext
  | Ws of int_ext
  | Po of sd * direction * direction
  | Dp of dp * sd * direction * direction
  | BasicDep of direction * direction (* Carries a dependency on *)
  | Iico of iico

(** edge attributes *)
let edge_direction = function
  | Rf _ -> Wm false, Rm false
  | Fr _ -> Rm false, Wm false
  | Ws _ -> Wm false, Wm false
  | Po (_, dir1, dir2) -> dir1, dir2
  | Dp (_, _, dir1, dir2) -> dir1, dir2
  | BasicDep (dir1, dir2) -> dir1, dir2
  | Iico i -> i.direction

let edge_location = function
  | BasicDep (RegEvent, (Rm _ | Wm _)) -> Different
  | Rf _ | Fr _ | Ws _ | BasicDep _ -> Same
  | Po (sd, _, _) | Dp (_, sd, _, _) -> sd
  | Iico i -> i.sd

let iico_ht = Hashtbl.create 10
let get_iico s = Hashtbl.find iico_ht s
let add_iico iico = Hashtbl.add iico_ht iico.repr iico

let list_iico_edges () =
  Hashtbl.iter (fun k _ -> Printf.printf "iico[%s]\n" k) iico_ht

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

let pp_dp = function
  | Addr -> "Addr"
  | Data -> "Data"
  | Ctrl -> "Ctrl"
  | Reg -> "Reg"

let pp_annot = function AnnotNone -> "" | A -> "A" | L -> "L" | X -> "X"

let pp_node_dep =
  let pp_int_option = function None -> "" | Some v -> "=" ^ string_of_int v in
  function
  | DepAddr (r, v) -> "Addr " ^ AArch64_compile.pp_reg r ^ pp_int_option v
  | DepData (r, v) -> "Data " ^ AArch64_compile.pp_reg r ^ pp_int_option v
  | DepCtrl (r, v) -> "Ctrl " ^ AArch64_compile.pp_reg r ^ pp_int_option v
  | DepReg (r, v) -> "Reg " ^ AArch64_compile.pp_reg r ^ pp_int_option v
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
  | BasicDep (dir1, dir2) -> "basic_dep" ^ pp_direction dir1 ^ pp_direction dir2
  | Iico i -> "iico[" ^ i.repr ^ "]"

let rec pp_edges = function
  | [e] -> pp_edge e
  | e :: q -> pp_edge e ^ " -> " ^ pp_edges q
  | _ -> ""
