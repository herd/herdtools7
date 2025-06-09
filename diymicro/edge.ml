(** Type definitions *)

(* Intrinsic event and edge *)
type ievent = string
type iedge = ievent * ievent

(* Direction: Register read, Register write, Memory read, Memory Write *)
type direction = Rr | Wr | Rm | Wm

(* Change process ? *)
type int_ext = Internal | External

(* Change location ? *)
type sd = Same | Different
type dp = Addr | Data | Ctrl

type node_dep =
  | DepAddr of AArch64_compile.reg
  | DepData of AArch64_compile.reg
  | DepCtrl of AArch64_compile.reg
  | DepReg of AArch64_compile.reg
  | DepNone

type iico = {
  repr : string;
  compile_edge :
    AArch64_compile.state ->
    node_dep ->
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

(** memory event annotation *)
type annot = AnnotNone | A | L

(** edge attributes *)
let edge_direction = function
  | Rf _ -> Wm, Rm
  | Fr _ -> Rm, Wm
  | Ws _ -> Wm, Wm
  | Po (_, dir1, dir2) -> dir1, dir2
  | Dp (_, _, dir1, dir2) -> dir1, dir2
  | BasicDep (dir1, dir2) -> dir1, dir2
  | Iico i -> i.direction

let edge_location = function
  | Rf _ | Fr _ | Ws _ | BasicDep _ -> Same
  | Po (sd, _, _) | Dp (_, sd, _, _) -> sd
  | Iico i -> i.sd

let iico_ht = Hashtbl.create 10

let get_iico s =
  try Hashtbl.find iico_ht s
  with Not_found -> Warn.fatal "Unknown edge iico[%s]" s

let add_iico iico = Hashtbl.add iico_ht iico.repr iico

let dependency_reg = function
  | DepAddr r -> r
  | DepData r -> r
  | DepCtrl r -> r
  | DepReg r -> r
  | DepNone -> Warn.fatal "Unable to get register of dependency DepNone"

(** Pretty printers *)

let pp_direction = function Rr -> "Rr" | Wr -> "Wr" | Rm -> "R" | Wm -> "W"
let pp_int_ext = function Internal -> "i" | External -> "e"
let pp_sd = function Same -> "s" | Different -> "d"
let pp_dp = function Addr -> "Addr" | Data -> "Data" | Ctrl -> "Ctrl"
let pp_annot = function AnnotNone -> "" | A -> "A" | L -> "L"

let pp_node_dep = function
  | DepAddr r -> "Addr " ^ AArch64_compile.pp_reg r
  | DepData r -> "Data " ^ AArch64_compile.pp_reg r
  | DepCtrl r -> "Ctrl " ^ AArch64_compile.pp_reg r
  | DepReg r -> "Reg " ^ AArch64_compile.pp_reg r
  | DepNone -> "None"

let pp_edge = function
  | Rf ie -> "Rf" ^ pp_int_ext ie
  | Fr ie -> "Fr" ^ pp_int_ext ie
  | Ws ie -> "Fr" ^ pp_int_ext ie
  | Po (sd, dir1, dir2) ->
      "Po" ^ pp_sd sd ^ pp_direction dir1 ^ pp_direction dir2
  | Dp (dp, sd, Rm, dir) -> "Dp" ^ pp_dp dp ^ pp_sd sd ^ pp_direction dir
  | Dp (dp, sd, dir1, dir2) ->
      "Dp" ^ pp_dp dp ^ pp_sd sd ^ pp_direction dir1 ^ pp_direction dir2
  | BasicDep (dir1, dir2) -> "basic_dep" ^ pp_direction dir1 ^ pp_direction dir2
  | Iico i -> "iico[" ^ i.repr ^ "]"

let rec pp_edges = function
  | [e] -> pp_edge e
  | e :: q -> pp_edge e ^ " -> " ^ pp_edges q
  | _ -> ""
