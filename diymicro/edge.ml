(** Type definitions *)

type operation = string (* should be a graph parsed from a file *)

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
  | DepReg of AArch64_compile.reg
  | DepNone

type t =
  | Rf of int_ext
  | Fr of int_ext
  | Ws of int_ext
  | Po of sd * direction * direction
  | Dp of dp * sd * direction
  | Iico of operation * iedge

(** edge attributes *)
let edge_direction = function
  | Rf _ -> Wm, Rm
  | Fr _ -> Rm, Wm
  | Ws _ -> Wm, Wm
  | Po (_, dir1, dir2) -> dir1, dir2
  | Dp (_, _, dir) -> Rm, dir
  | Iico _ -> failwith "iico edges not implemented"

let edge_location = function
  | Rf _ | Fr _ | Ws _ -> Same
  | Po (sd, _, _) | Dp (_, sd, _) -> sd
  | Iico _ -> failwith "iico edges not implemented"

(** Pretty printers *)

let pp_direction = function Rr -> "Rr" | Wr -> "Wr" | Rm -> "R" | Wm -> "W"
let pp_int_ext = function Internal -> "i" | External -> "e"
let pp_sd = function Same -> "s" | Different -> "d"
let pp_dp = function Addr -> "Addr" | Data -> "Data" | Ctrl -> "Ctrl"

let pp_edge = function
  | Rf ie -> "Rf" ^ pp_int_ext ie
  | Fr ie -> "Fr" ^ pp_int_ext ie
  | Ws ie -> "Fr" ^ pp_int_ext ie
  | Po (sd, dir1, dir2) ->
      "Po" ^ pp_sd sd ^ pp_direction dir1 ^ pp_direction dir2
  | Dp (dp, sd, dir) -> "Dp" ^ pp_dp dp ^ pp_sd sd ^ pp_direction dir
  | Iico (op, (ievt1, ievt2)) -> "iico[" ^ op ^ ":" ^ ievt1 ^ "->" ^ ievt2 ^ "]"

let rec pp_edges = function
  | [e] -> pp_edge e
  | e :: q -> pp_edge e ^ " -> " ^ pp_edges q
  | _ -> ""
