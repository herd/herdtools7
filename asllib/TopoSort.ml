module type OrderedHashedType = sig
  include Set.OrderedType
  include Hashtbl.HashedType with type t := t

  val to_string : t -> string
end

module Make (O : OrderedHashedType) = struct
  type succs = O.t -> O.t list

  module OSet = Set.Make (O)
  module OMap = Map.Make (O)
  module OTbl = Hashtbl.Make (O)

  module OStack = struct
    type t = { stack : O.t Stack.t; mutable set : OSet.t }

    let create () = { stack = Stack.create (); set = OSet.empty }

    let push o t =
      t.set <- OSet.add o t.set;
      Stack.push o t.stack

    let mem w t = OSet.mem w t.set

    let pop t =
      let o = Stack.pop t.stack in
      t.set <- OSet.remove o t.set;
      o

    let pop_until =
      let rec aux o acc t =
        let o' = pop t in
        let acc = o' :: acc in
        if O.compare o' o = 0 then acc else aux o acc t
      in
      fun o t -> aux o [] t
  end

  type data = {
    (* order from which nodes are discovered *)
    index : int;
    (* lowest index of nodes in the stack reachable from a node. *)
    lowlink : int;
  }
  (** Book-keeping for each node *)

  type state = { data : data OTbl.t; stack : OStack.t; index : int ref }
  (** All book-keeping *)

  let set_lowlink s v v_data lowlink =
    let v_data = { v_data with lowlink } in
    OTbl.replace s.data v v_data;
    v_data

  let rec each_successor succs s fold v (acc, v_data) w =
    let () =
      if false then
        Format.eprintf "each_successor of v=%s w=%s@." (O.to_string v)
          (O.to_string w)
    in
    match OTbl.find_opt s.data w with
    | None ->
        let acc, w_data = strong_connected succs s fold acc w in
        if v_data.lowlink > w_data.lowlink then
          (acc, set_lowlink s v v_data w_data.lowlink)
        else (acc, v_data)
    | Some w_data ->
        if OStack.mem w s.stack && v_data.lowlink > w_data.index then
          (acc, set_lowlink s v v_data w_data.index)
        else (acc, v_data)

  and strong_connected succs s fold acc v =
    (* Assumption that [s.data] does not contain [v]. *)
    let () =
      if false then Format.eprintf "strong_connected %s@." (O.to_string v)
    in
    let index = !(s.index) in
    let v_data = { index; lowlink = index } in
    let () = OTbl.add s.data v v_data
    and () = OStack.push v s.stack
    and () = incr s.index in
    let acc, v_data =
      List.fold_left (each_successor succs s fold v) (acc, v_data) (succs v)
    in
    let acc =
      if v_data.index = v_data.lowlink then
        let connected_component = OStack.pop_until v s.stack in
        fold connected_component acc
      else acc
    in
    (acc, v_data)

  let fold_strong_connected ?(size_hint = 16) fold nodes succs acc =
    let s =
      { data = OTbl.create size_hint; index = ref 0; stack = OStack.create () }
    in
    List.fold_left
      (fun acc v ->
        if OTbl.mem s.data v then acc
        else strong_connected succs s fold acc v |> fst)
      acc nodes

  let sort_connected nodes succs =
    fold_strong_connected List.cons nodes succs [] |> List.rev

  let index_connected =
    let indexer nodes (indexes, i) =
      let indexes =
        List.fold_left
          (fun indexes node -> OMap.add node i indexes)
          indexes nodes
      in
      (indexes, succ i)
    in
    fun nodes succs ->
      let indexes, _i =
        fold_strong_connected indexer nodes succs (OMap.empty, 0)
      in
      OMap.bindings indexes

  module Properties = struct
    let order_respected (nodes, succs) =
      let indexes = index_connected nodes succs |> OMap.of_list in
      let () =
        if false then (
          let open Format in
          eprintf "@[<hov 2>Indexes:@ ";
          OMap.iter
            (fun v i -> eprintf "@[<h>%s:%d@]@ " (O.to_string v) i)
            indexes;
          eprintf "@]@.")
      in
      List.for_all
        (fun v ->
          List.for_all
            (fun w ->
              match (OMap.find_opt v indexes, OMap.find_opt w indexes) with
              | Some i_v, Some i_w -> i_v >= i_w
              | _ -> false)
            (succs v))
        nodes
  end
end

module ASTFold = struct
  module O = struct
    include String

    let to_string s = s
  end

  module OSet = ASTUtils.ISet
  module TS = Make (O)
  module Tbl = TS.OTbl

  type t = {
    nodes : string list;
    succs : string -> string list;
    decls : AST.decl list Tbl.t;
  }

  let tbl_add_set tbl key values =
    match Tbl.find_opt tbl key with
    | None -> Tbl.add tbl key values
    | Some prev -> Tbl.replace tbl key (OSet.union values prev)

  let tbl_add_list tbl key values =
    match Tbl.find_opt tbl key with
    | None -> Tbl.add tbl key values
    | Some prev -> Tbl.replace tbl key (List.rev_append values prev)

  let def d =
    let open AST in
    match d.desc with
    | D_Func { name; _ } | D_GlobalStorage { name; _ } | D_TypeDecl (name, _, _)
      ->
        name

  let use d = ASTUtils.use_constant_decl OSet.empty d

  let extra_def d =
    let open AST in
    match d.desc with
    | D_TypeDecl (_, { desc = T_Enum names; _ }, _) -> names
    | _ -> []

  let build ast : t =
    let add_one (succ_tbl, decl_tbl) d =
      let v = def d and u = use d in
      tbl_add_set succ_tbl v u;
      tbl_add_list decl_tbl v [ d ];
      List.iter
        (fun v' ->
          tbl_add_set succ_tbl v' (OSet.singleton v);
          tbl_add_list decl_tbl v' [])
        (extra_def d);
      v
    in
    let succ_tbl, decls = (Tbl.create 16, Tbl.create 16) in
    let nodes = List.map (add_one (succ_tbl, decls)) ast in
    let () =
      Tbl.filter_map_inplace
        (fun _v d -> OSet.filter (Tbl.mem decls) d |> Option.some)
        succ_tbl
    in
    let () =
      if false then (
        let open Format in
        eprintf "@[<v 2>Dependencies:@ ";
        Tbl.iter
          (fun v -> eprintf "@[<h>%s <-- %a@]@ " v OSet.pp_print)
          succ_tbl;
        eprintf "@]@.")
    in
    let succs s = Tbl.find succ_tbl s |> OSet.to_list in
    { nodes; succs; decls }

  type step = Single of AST.decl | Recursive of AST.decl list

  let fold fold ast =
    let { nodes; succs; decls } = build ast in
    let folder nodes acc =
      let ds = List.concat_map (Tbl.find decls) nodes in
      match ds with
      | [] -> acc (* Can happen for fantom dependencies. *)
      | [ d ] -> fold (Single d) acc
      | _ -> fold (Recursive ds) acc
    in
    let size_hint = Tbl.length decls in
    TS.fold_strong_connected ~size_hint folder nodes succs
end
