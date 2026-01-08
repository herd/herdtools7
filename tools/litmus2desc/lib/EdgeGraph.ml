module G = struct
  module Int = struct
    type t = int

    let compare = Int.compare
    let hash = Hashtbl.hash
    let equal = Int.equal
  end

  module String = struct
    type t = string

    let compare = String.compare
    let default = ""
  end

  include Graph.Imperative.Digraph.ConcreteLabeled (Int) (String)
end

type t = G.t
type vertex = int
type edge = { label : string; source : vertex; target : vertex }

module Simple_paths (G : Graph.Sig.G) = struct
  module V = G.V
  module E = G.E
  module VS = Set.Make (V)

  let get_label : E.t -> E.label = E.label

  let iter_simple_paths (g : G.t) ~(src : V.t) ~(dst : V.t)
      ~(pred : E.label -> bool) (yield : V.t list -> unit) =
    let rec dfs visited path v =
      if V.equal v dst then yield (List.rev (v :: path))
      else
        G.iter_succ
          (fun w ->
            let ed : E.t = G.find_edge g v w in
            let lbl = get_label ed in
            if (not (VS.mem w visited)) && pred lbl then
              dfs (VS.add w visited) (v :: path) w)
          g v
    in
    dfs (VS.singleton src) [] src
end

let iter_simple_paths ~(f : int list -> unit) ~(pred : string -> bool)
    ~(src : int) ~(dst : int) (g : G.t) =
  let module SP = Simple_paths (G) in
  SP.iter_simple_paths ~src ~dst ~pred g f

let simple_paths_iter ~(src : int) ~(dst : int) ~(pred : string -> bool)
    (g : G.t) : int list Util.Iter.t =
 fun f -> iter_simple_paths ~f ~src ~dst ~pred g

let make vertices edges : G.t =
  let g = G.create () in
  let () = vertices |> List.iter (G.add_vertex g) in
  let () =
    edges
    |> List.iter (fun ed ->
        let source_id = ed.source in
        let target_id = ed.target in
        G.add_edge_e g (G.E.create source_id ed.label target_id))
  in
  g
