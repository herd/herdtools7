open ASTUtils

let _runtime_assertions = true

type pointer = int

module PMap = Map.Make (Int)
module PSet = Set.Make (Int)

type 'v t = { env : pointer IMap.t; mem : 'v PMap.t }

let alloc =
  let next = ref 0 in
  fun () ->
    let r = !next in
    next := r + 1;
    r

let empty = { env = IMap.empty; mem = PMap.empty }
let mem x t = IMap.mem x t.env

let assign x v t =
  let p = IMap.find x t.env in
  { t with mem = PMap.add p v t.mem }

let declare x v t =
  let () = if _runtime_assertions then assert (not (mem x t)) in
  let p = alloc () in
  { env = IMap.add x p t.env; mem = PMap.add p v t.mem }

let add x v t = try assign x v t with Not_found -> declare x v t

let find x t =
  let p = IMap.find x t.env in
  PMap.find p t.mem

let find_opt x t = try find x t with Not_found -> None

let remove x t =
  try
    let p = IMap.find x t.env in
    { mem = PMap.remove p t.mem; env = IMap.remove x t.env }
  with Not_found -> t

let patch_mem ~t_env ~t_mem to_avoid =
  let env = t_env.env
  and mem =
    try
      List.fold_left
        (fun mem x ->
          let p = IMap.find x t_mem.env in
          PMap.remove p mem)
        t_mem.mem to_avoid
    with Not_found -> assert false
  in
  { env; mem }
