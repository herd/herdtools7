(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Edge = struct
  type t = {
    left: string;
    right: string;
    desc: string;
  }
  let pp e = Printf.sprintf "%s -> %s" e.left e.right
end

module Node = struct
  type kind =  Mem | Reg_Data | Branching | Reg_Other
  type t = {
    desc: string;
    kind: kind;
  }
end

type t = {
  nodes: Node.t StringMap.t;
  edges: Edge.t list;
}

let empty = {
  nodes = StringMap.empty;
  edges = []
}

let str_contains str substr = 
  let reg = Str.regexp_string substr in
  try
    ignore (Str.search_forward reg str 0);
    true
  with Not_found -> false

let rec tr_stmt acc stmt =
  let module ParsedNode = ParsedDotGraph.Node in
  let module ParsedAttr = ParsedDotGraph.Attr in
  let module ParsedStmt = ParsedDotGraph.Stmt in
  let module ParsedEdge = ParsedDotGraph.Edge in
  let module ParsedSubgraph = ParsedDotGraph.Subgraph in

  let is_init_event str =
    let r = Str.regexp {|Init|} in
    try
      ignore (Str.search_backward r str (String.length str - 1));
      true
    with
    | Not_found -> false in

  let is_gpreg reg =
    let r = Str.regexp {|[BHWXQ][0-9]+|} in
    Str.string_match r reg 0 in

  let pp_reg reg =
    let reg = match reg with
    | "NZCV" -> "PSTATE.NZCV"
    | r -> r in
    if is_gpreg reg then reg else "`" ^ reg ^ "`" in

  match stmt with
  | ParsedStmt.Node n ->
    let label = List.find (fun a -> a.ParsedAttr.name = "label") n.ParsedNode.attrs in
    let value = label.ParsedAttr.value in
    if is_init_event value then
      (* Skip init events *)
      acc
    else begin
      let mem_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[\([a-zA-Z0-9_]+\)\]|} in
      let reg_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)[0-9]:\([A-Z_]+[0-9]*\)|} in
      let branching = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(\(\[[a-zA-Z0-9_]+\]\|[0-9]:[A-Z_]+[0-9]*\)\(==\|!=\)\(\[[a-zA-Z0-9_]+\]\|[0-9]:[A-Z_]+[0-9]*\))|} in
      let node = if Str.string_match mem_access value 0 then begin
        let rw = if Str.matched_group 1 value = "R" then
          "the Explicit Memory Read effect"
        else
          "the Explicit Memory Write effect" in
        let loc = "the Memory Location " ^ Str.matched_group 2 value in

        (* Look for the register that was used to address memory, which
          is part of the instruction (at the end of the attribute value) *)
        let address_reg = Str.regexp {|\[\([A-Z]+[0-9]*\)\]|} in
        ignore (Str.search_backward address_reg value (String.length value - 1));
        let loc = loc ^ " addressed by " ^ Str.matched_group 1 value in
        {
          Node.desc = rw ^ " of " ^ loc;
          kind = Node.Mem;
        }
      end
      else if Str.string_match reg_access value 0 then begin
        let rw = if Str.matched_group 1 value = "R" then
          "the Register Read effect"
        else
          "the Register Write effect" in
        let reg = pp_reg (Str.matched_group 2 value) in
        let kind = if str_contains value "(data)" then
          Node.Reg_Data
        else
          Node.Reg_Other in
        { Node.desc=rw ^ " of " ^ reg; kind=kind }
      end
      else if Str.string_match branching value 0 then begin
        let lhs = Str.matched_group 1 value in
        let eq = Str.matched_group 2 value in
        let rhs = Str.matched_group 3 value in

        (* Extracts the memory location or register name out of a lhs or rhs *)
        let mem_or_reg str =
          let mem = Str.regexp {|\[\([a-zA-Z0-9_]+\)\]|} in
          let reg = Str.regexp {|[0-9]:\([A-Z_]+[0-9]*\)|} in
          if Str.string_match mem str 0 then
            "the Memory Location " ^ Str.matched_group 1 str
          else if Str.string_match reg str 0 then
            "the Register " ^ (pp_reg (Str.matched_group 1 str))
          else
            Warn.fatal "String %s contains neither a register nor a memory address" str in
        
        let lhs = mem_or_reg lhs in
        let rhs = mem_or_reg rhs in
        let eq = if String.equal eq "==" then "are equal" else "are not equal" in
        let desc = "the Intrinsic Branching effect which checks whether the contents of " 
          ^ lhs ^ " " ^ eq ^ " to the contents of " ^ rhs in
        { Node.desc=desc; kind=Node.Branching }
      end
      else
        Warn.fatal "Unsupported type of effect label: %s" value in
      
      let map = StringMap.add n.ParsedNode.name node acc.nodes in
      { acc with nodes=map }
    end
  | ParsedStmt.Attr _ -> acc
  | ParsedStmt.Edge e ->
    let label = List.find (fun a -> a.ParsedAttr.name = "label") e.ParsedEdge.attrs in
    let value = label.ParsedAttr.value in
    if String.equal value "iico_data" then
      let desc = "an Intrinsic data dependency" in
      let edge = { Edge.left=e.ParsedEdge.left; right=e.ParsedEdge.right; desc=desc } in
      { acc with edges=edge::acc.edges }
    else if String.equal value "iico_ctrl" then
      let desc = "an Intrinsic control dependency" in
      let edge = { Edge.left=e.ParsedEdge.left; right=e.ParsedEdge.right; desc=desc } in
      { acc with edges=edge::acc.edges }
    else if String.equal value "iico_order" then
      let desc = "an Intrinsic order dependency" in
      let edge = { Edge.left=e.ParsedEdge.left; right=e.ParsedEdge.right; desc=desc } in
      { acc with edges=edge::acc.edges }
    else
      (* Skip any other kind of edge *)
      acc
  | ParsedStmt.Subgraph s ->
    List.fold_left tr_stmt acc s.ParsedSubgraph.stmts

let tr parsed_graph =
  let translated = List.fold_left tr_stmt empty parsed_graph.ParsedDotGraph.stmts in

  let module Adjacency = struct
    type t = {
      succs: int;
      preds: string list;
    }
    let empty = {
      succs = 0;
      preds = []
    }
  end in

  (* Build the adjacency map, containing all predecessors and number of
     successors for every node *)
  let nodes = StringMap.fold (fun node _ acc -> node :: acc) translated.nodes [] in
  let adj_map = List.fold_left (fun adj_map node ->
    StringMap.add node Adjacency.empty adj_map
  ) StringMap.empty nodes in
  let adj_map = List.fold_left (fun adj_map edge ->
    let adj_left = StringMap.find edge.Edge.left adj_map in
    let succs = adj_left.Adjacency.succs in
    let adj_left = { adj_left with Adjacency.succs = succs + 1 } in
    let adj_right = StringMap.find edge.Edge.right adj_map in
    let preds = adj_right.Adjacency.preds in
    let adj_right = { adj_right with Adjacency.preds = edge.Edge.left :: preds } in
    let adj_map = StringMap.add edge.Edge.left adj_left adj_map in
    StringMap.add edge.Edge.right adj_right adj_map
  ) adj_map translated.edges in

  (* All nodes with no successors *)
  let no_succs = StringMap.fold (fun node adj nodes ->
    if adj.Adjacency.succs = 0 then (node, 0) :: nodes else nodes
  ) adj_map [] in

  (* Topological sort of nodes - returns a map from node id
     to its level. If node n1 appears before n2 in the topological
     order of the graph, then level(n1) < level(n2), although the
     reverse is not necessarily true. Note that the algorithm
     assigns level 0 to all nodes with no successors and negative
     levels to the rest - the goal was that all terminal nodes are
     on the same level *)
  let rec top_sort adj_map res = function
  | [] -> res
  | (node, level) :: nodes ->
    let adj = StringMap.find node adj_map in
    let preds = adj.Adjacency.preds in
    let new_nodes, new_adj_map = List.fold_left (fun (nodes, adj_map) node ->
      let adj = StringMap.find node adj_map in
      let new_succs = adj.Adjacency.succs - 1 in
      let nodes = if new_succs = 0 then
        (node, level - 1) :: nodes
      else
        nodes in
      let new_adj = { adj with Adjacency.succs = new_succs } in
      let adj_map = StringMap.add node new_adj adj_map in
      nodes, adj_map
    ) ([], adj_map) preds in
    let new_nodes = nodes @ new_nodes in
    let res = StringMap.add node level res in
    top_sort new_adj_map res new_nodes in

  let levels = top_sort adj_map StringMap.empty no_succs in

  let cmp_nodes n1 n2 =
    let i1 = StringMap.find n1 levels in
    let i2 = StringMap.find n2 levels in
    if i1 = i2 then begin
      let node1 = StringMap.find n1 translated.nodes in
      let node2 = StringMap.find n2 translated.nodes in
      let kind1 = node1.Node.kind in
      let kind2 = node2.Node.kind in
      if kind1 = kind2 then
        String.compare n1 n2
      else
        compare kind1 kind2
    end
    else
      compare i1 i2 in

  (* Comparison function on edges - prioritises the in-node over
     the out-node, because once we start describing edges going
     inside a node, we want to describe all of those edges *)
  let cmp_edges e1 e2 =
    let cmp_right = cmp_nodes e1.Edge.right e2.Edge.right in
    let cmp_left = cmp_nodes e1.Edge.left e2.Edge.left in
    if cmp_right <> 0 then cmp_right else cmp_left in

  let sorted_edges = List.sort cmp_edges translated.edges in
  { translated with edges = sorted_edges }
