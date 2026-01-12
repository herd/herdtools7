type vertex = int
type edge = { label : string; source : vertex; target : vertex }
type t = { vertices : IntSet.t; succ : edge list IntMap.t }

let make vertices edges =
  let vertices =
    List.fold_left (fun acc v -> IntSet.add v acc) IntSet.empty vertices
  in
  let vertices =
    List.fold_left
      (fun acc edge -> acc |> IntSet.add edge.source |> IntSet.add edge.target)
      vertices edges
  in
  let succ =
    List.fold_right
      (fun edge acc ->
        IntMap.update edge.source
          (function
            | None -> Some [ edge ] | Some edges -> Some (edge :: edges))
          acc)
      edges IntMap.empty
  in
  { vertices; succ }

let outgoing_edges (g : t) (v : vertex) : edge list =
  match IntMap.find_opt v g.succ with Some edges -> edges | None -> []

let iter_simple_paths ~(src : vertex) ~(dst : vertex)
    ~(pred : src:vertex -> tgt:vertex -> string -> bool)
    ~(f : vertex list -> unit) (g : t) : unit =
  if not (IntSet.mem src g.vertices && IntSet.mem dst g.vertices) then ()
  else
    let rec dfs visited path v =
      if v = dst then f (List.rev (v :: path))
      else
        outgoing_edges g v
        |> List.iter (fun edge ->
            let next = edge.target in
            if
              (not (IntSet.mem next visited))
              && pred ~src:edge.source ~tgt:next edge.label
            then
              let visited' = IntSet.add next visited in
              dfs visited' (v :: path) next)
    in
    dfs (IntSet.singleton src) [] src

let simple_paths_iter ~(src : int) ~(dst : int)
    ~(pred : src:vertex -> tgt:vertex -> string -> bool) (g : t) :
    int list Util.Iter.t =
 fun f -> iter_simple_paths ~f ~src ~dst ~pred g
