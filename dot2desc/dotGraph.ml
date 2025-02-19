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
  type kind = Data | Control | Order
  type t = {
    left: string;
    right: string;
    desc: string -> string -> string;
    kind: kind;
  }
  let pp e = Printf.sprintf "%s -> %s" e.left e.right
  let kind = function
  | "iico_data" -> Data
  | "iico_ctrl" -> Control
  | "iico_order" -> Order
  | s -> Warn.fatal "Invalid type of edge kind %s" s
end

module Node = struct
  type kind = Fault | Mem | Reg_Data | Branching | Reg_Other | TLBI | Empty
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

module ParsedNode = ParsedDotGraph.Node
module ParsedAttr = ParsedDotGraph.Attr
module ParsedStmt = ParsedDotGraph.Stmt
module ParsedEdge = ParsedDotGraph.Edge
module ParsedSubgraph = ParsedDotGraph.Subgraph

(* To be called after Str.string_match was called on the appropriate regex and
   string. The regex must contain 2 groups, the second of which matching the
   location, and the instruction must contain register addressing using the
   "[Xn]" syntax. *)
let get_loc_and_address_reg value =
  let loc = Str.matched_group 2 value in
  (* Look for the register that was used to address memory, which
    is part of the instruction (at the end of the attribute value) *)
  let address_reg = Str.regexp {|\[\([A-Z_]+[0-9]*\)\]|} in
  let reg = try
    ignore (Str.search_backward address_reg value (String.length value - 1));
    Str.matched_group 1 value
  with Not_found ->
    Warn.fatal "Could not find address register for memory access" in
  (loc, reg)

let get_label_value attrs =
  let label = List.find (fun a -> a.ParsedAttr.name = "label") attrs in
  label.ParsedAttr.value

let is_init_event n =
  let value = get_label_value n.ParsedNode.attrs in
  let r = Str.regexp {|Init|} in
  try
    ignore (Str.search_backward r value (String.length value - 1));
    true
  with
  | Not_found -> false

let make_monospace str =
  Printf.sprintf "`%s`" str

let tr_stmt acc stmt param_map =

  (* To be called after Str.string_match was called on the appropriate regex
    and string. The regex must contain 2 groups: the first is a (R|W) denoting
    whether the access is a read or a write effect, and the second matches the
    location. The instruction must contain register addressing using the "[Xn]"
    syntax. *)
  let do_mem_access value read write =
    let f = if Str.matched_group 1 value = "R" then read else write in
    let loc, reg = get_loc_and_address_reg value in
    let reg = StringMap.safe_find reg reg param_map in
    let is_explicit = not (str_contains value "NExp") in
    { Node.desc=f loc reg is_explicit; kind=Node.Mem } in

  let is_gpreg reg =
    let r = Str.regexp {|[BHWXQ][0-9]+|} in
    Str.string_match r reg 0 in

  let pp_reg reg =
    let reg = match reg with
    | "NZCV" -> "PSTATE.NZCV"
    | r -> r in
    let tr_reg = StringMap.safe_find reg reg param_map in
    if is_gpreg reg then tr_reg else make_monospace tr_reg in

  (* To be called after Str.string_match was called on the fault or exc_entry regexes *)
  let get_fault_name value =
    let gr = Str.matched_group 1 value in
    let els = String.split_on_char ',' gr in
    (* The name of the event should be at the end *)
    let last = List.hd (List.rev els) in
    let words = String.split_on_char ':' last in
    String.concat " " words in

  match stmt with
  | ParsedStmt.Node n ->
    let value = get_label_value n.ParsedNode.attrs in
    if is_init_event n then
      (* Skip init events *)
      acc
    else begin
      let symbol = "\\([a-zA-Z0-9_\\+]+\\)" in
      let tag = Printf.sprintf "tag(%s)" symbol in
      let pte = Printf.sprintf "PTE(%s)" symbol in
      let pa = Printf.sprintf "PA(%s)" symbol in
      let tlb = Printf.sprintf "TLB(%s)" symbol in
      let memloc_regex = Str.regexp symbol in
      let tag_regex = Str.regexp tag in
      let pte_regex = Str.regexp pte in
      let pa_regex = Str.regexp pa in
      let tlb_regex = Str.regexp tlb in
      let mem_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[\([a-zA-Z0-9_\+]+\)\]|} in
      let tag_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[tag(\([a-zA-Z0-9_\+]+\))\]|} in
      let pte_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[PTE(\([a-zA-Z0-9_\+]+\))\]|} in
      let pa_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[PA(\([a-zA-Z0-9_\+]+\))\]|} in
      let ifetch = Str.regexp {|[a-zA-Z0-9_]*: R\[label:\\"P[0-9]:\([a-zA-Z0-9_]+\)\\"\]IFetch=\([][,a-zA-Z0-9_ ]+\)|} in
      let tlbi = Str.regexp {|[a-zA-Z0-9_]*: TLBI(\([A-Z0-9]+\),\[\([a-zA-Z0-9_\+()]+\)\])|} in
      let generic_tlbi = Str.regexp {|[a-zA-Z0-9_]*: TLBI(\([A-Z0-9]+\))|} in
      let reg_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)[0-9]:\([A-Z_]+[0-9]*\)|} in
      let branching = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(\(\[[a-zA-Z0-9_\+]+\]\|[0-9]:[A-Z_]+[0-9]*\)\(==\|!=\)\(\[[a-zA-Z0-9_\+]+\]\|[0-9]:[A-Z_]+[0-9]*\))|} in
      let branching_mte_tag = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(color)(tag(\([a-zA-Z0-9_\+]+\)), \([A-Z_]+[0-9]*\))|} in
      let branching_pte = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(PTE(\([a-zA-Z0-9_\+]+\)), \([A-Z_]+[0-9]*\))\((\([a-zA-Z0-9_,:&|() ]+\))\)?|} in
      let branching_instr_cond = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)|} in
      let bcc_branching = Str.regexp {|[a-zA-Z0-9_]*: Branching(bcc)|} in
      let fault = Str.regexp {|[a-zA-Z0-9_]*: Fault(\([a-zA-Z0-9_:,]*\))|} in
      let exc_entry = Str.regexp {|[a-zA-Z0-9_]*: ExcEntry(\([a-zA-Z0-9_:,]*\))|} in
      let empty = Str.regexp {|[a-zA-Z0-9_]*: \\|} in
      let node = if Str.string_match mem_access value 0 then
        do_mem_access value DescDict.mem_read DescDict.mem_write
      else if Str.string_match tag_access value 0 then
        do_mem_access value DescDict.tag_read DescDict.tag_write
      else if Str.string_match pte_access value 0 then
        do_mem_access value DescDict.pte_read DescDict.pte_write
      else if Str.string_match pa_access value 0 then
        do_mem_access value DescDict.pa_read DescDict.pa_write
      else if Str.string_match ifetch value 0 then begin
        let f = DescDict.ifetch in
        let label = Str.matched_group 1 value in
        let instr = Str.matched_group 2 value in
        { Node.desc=f label instr; kind=Node.Mem }
      end
      else if Str.string_match tlbi value 0 then begin
        let f = DescDict.tlbi in
        let typ = make_monospace (Str.matched_group 1 value) in
        let loc = Str.matched_group 2 value in

        let reg_regex = Str.regexp {|[A-Z_]+[0-9]*|} in
        let reg = try
          ignore (Str.search_backward reg_regex value (String.length value - 1));
          Str.matched_group 0 value
        with Not_found ->
          Warn.fatal "Could not find register as part of TLBI instruction" in

        let loc = if Str.string_match tag_regex loc 0 then
          let loc = Str.matched_group 1 loc in
          DescDict.tagloc_of loc reg
        else if Str.string_match pte_regex loc 0 then
          let loc = Str.matched_group 1 loc in
          DescDict.pte_of loc reg
        else if Str.string_match pa_regex loc 0 then
          let loc = Str.matched_group 1 loc in
          DescDict.pa_of loc reg
        else if Str.string_match tlb_regex loc 0 then
          let loc = Str.matched_group 1 loc in
          DescDict.tlb_of loc reg
        else if Str.string_match memloc_regex loc 0 then begin
          DescDict.memloc_addr_by loc reg end
        else
          Warn.fatal "Unrecognised type of location %s" loc in
        { Node.desc=f typ loc; kind=Node.TLBI }
      end
      else if Str.string_match generic_tlbi value 0 then begin
        let f = DescDict.generic_tlbi in
        let typ = make_monospace (Str.matched_group 1 value) in
        { Node.desc=f typ; kind=Node.TLBI }
      end
      else if Str.string_match reg_access value 0 then begin
        let f = if Str.matched_group 1 value = "R" then DescDict.reg_read else DescDict.reg_write in
        let reg = pp_reg (Str.matched_group 2 value) in
        let kind = if str_contains value "(data)" then
          Node.Reg_Data
        else
          Node.Reg_Other in
        { Node.desc=f reg; kind=kind }
      end
      else if Str.string_match branching value 0 then begin
        let f = DescDict.branching in
        let lhs = Str.matched_group 1 value in
        let rel = Str.matched_group 2 value in
        let rhs = Str.matched_group 3 value in

        (* Extracts the memory location or register name out of a lhs or rhs *)
        let mem_or_reg str =
          let mem = Str.regexp {|\[\([a-zA-Z0-9_\+]+\)\]|} in
          let reg = Str.regexp {|[0-9]:\([A-Z_]+[0-9]*\)|} in
          if Str.string_match mem str 0 then
            DescDict.memloc (Str.matched_group 1 str)
          else if Str.string_match reg str 0 then
            DescDict.reg (pp_reg (Str.matched_group 1 str))
          else
            Warn.fatal "String %s contains neither a register nor a memory address" str in
        
        let lhs = mem_or_reg lhs in
        let rhs = mem_or_reg rhs in
        let cond = if String.equal rel "==" then
          DescDict.eq_contents lhs rhs
        else
          DescDict.neq_contents lhs rhs in
        { Node.desc=f cond; kind=Node.Branching }
      end
      else if Str.string_match branching_mte_tag value 0 then begin
        let f = DescDict.branching in
        let loc = Str.matched_group 1 value in
        let reg = pp_reg (Str.matched_group 2 value) in
        let cond = DescDict.mte_cond loc reg in
        { Node.desc=f cond; kind=Node.Branching }
      end
      else if Str.string_match branching_pte value 0 then begin
        let f = DescDict.branching in
        let loc = Str.matched_group 1 value in
        let pred = try
          let text = Str.matched_group 4 value in
          (* Sometimes there can be text before the logical expression *)
          let words = String.split_on_char ',' text in
          String.trim (List.hd (List.rev words))
        with Not_found -> "valid:1 && af:1" in
        let reg = pp_reg (Str.matched_group 2 value) in
        let cond = DescDict.pte_cond loc reg pred in
        { Node.desc=f cond; kind=Node.Branching }
      end
      else if Str.string_match branching_instr_cond value 0 then begin
        let f = DescDict.branching in
        let cond = Str.regexp {|\(EQ\|NE\)|} in
        try
          ignore (Str.search_backward cond value (String.length value - 1));
          let cond = Str.matched_group 1 value in
          let cond = DescDict.instr_cond (StringMap.safe_find cond cond param_map) in
          { Node.desc=f cond; kind=Node.Branching }
        with
        | Not_found ->
          (* Fallback to Bcc Branching *)
          { Node.desc=DescDict.bcc_branching; kind=Node.Branching }
      end
      else if Str.string_match bcc_branching value 0 then
        { Node.desc=DescDict.bcc_branching; kind=Node.Branching }
      else if Str.string_match fault value 0 then begin
        let f = DescDict.fault in
        let name = get_fault_name value in
        { Node.desc=f name; kind=Node.Fault }
        end
      else if Str.string_match exc_entry value 0 then begin
        let f = DescDict.exc_entry in
        let name = get_fault_name value in
        { Node.desc=f name; kind=Node.Fault }
        end
      else if Str.string_match empty value 0 then
        { Node.desc=DescDict.empty; kind=Node.Empty }
      else
        Warn.fatal "Unsupported type of effect label: %s" value in
      
      let map = StringMap.add n.ParsedNode.name node acc.nodes in
      { acc with nodes=map }
    end
  | ParsedStmt.Attr _ -> acc
  | ParsedStmt.Edge e -> begin
      let value = get_label_value e.ParsedEdge.attrs in
      try
        let desc = StringMap.find value DescDict.edges in
        let kind = Edge.kind value in
        let edge = { Edge.left=e.ParsedEdge.left; right=e.ParsedEdge.right; desc=desc; kind=kind } in
        { acc with edges=edge::acc.edges }
      with Not_found ->
        (* Skip any other kind of edge *)
        acc
    end
  | ParsedStmt.Subgraph _ ->
    Warn.fatal "Found dot subgraph even after flattenning was performed"

let convert_bnodes stmts =
  let node_map = List.fold_left (fun map stmt ->
    match stmt with
    | ParsedStmt.Node n ->
      let name = n.ParsedNode.name in
      let value = get_label_value n.ParsedNode.attrs in
      StringMap.add name value map
    | _ -> map
  ) StringMap.empty stmts in

  (* Compute the list of direct iico_data predecessors for every node *)
  let preds = StringMap.fold (fun key _ acc ->
    StringMap.add key [] acc
  ) node_map StringMap.empty in
  let preds = List.fold_left (fun map stmt ->
    match stmt with
    | ParsedStmt.Edge e ->
      let value = get_label_value e.ParsedEdge.attrs in
      if value = "iico_data" then
        let left = e.ParsedEdge.left in
        let right = e.ParsedEdge.right in
        let preds = StringMap.find right map in
        StringMap.add right (left :: preds) map
      else map
    | _ -> map
  ) preds stmts in

  let get_matching_nodes regex =
    StringMap.fold (fun key value m ->
      if Str.string_match regex value 0 then
        StringMap.add key value m
      else
        m
    ) node_map StringMap.empty in

  (* Get all predecessors that match pred_regex and extract the information
     (in practice location and address register), and then insert it into
     the nodes matching node_regex *)
  let update_labels node_regex pred_regex build_templ =
    let nodes = get_matching_nodes node_regex in
    StringMap.fold (fun key value map ->
      let ps = StringMap.find key preds in
      let matching_ps = List.fold_left (fun acc p ->
        let p_val = StringMap.find p node_map in
        if Str.string_match pred_regex p_val 0 then p_val :: acc else acc
      ) [] ps in
      match matching_ps with
      | [] -> map
      | p_val :: _ ->
        ignore (Str.string_match pred_regex p_val 0);
        let templ = build_templ p_val in
        let new_value = Str.replace_first node_regex templ value in
        StringMap.add key new_value map
    ) nodes StringMap.empty in

  (* Heuristic: If there is a PTE/MTE R/W predecessor that feeds its data to a
     branching effect, then probably the condition of that branching effect
     checks those values *)
  let branching_mte_tag = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(color)|} in
  let branching = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)|} in
  let tag_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[tag(\([a-zA-Z0-9_\+]+\))\]|} in
  let pte_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[PTE(\([a-zA-Z0-9_\+]+\))\]|} in

  let bcolour_nodes = update_labels branching_mte_tag tag_access (fun p_val ->
    (* The address register should be the same in the p_val as in value *)
    let loc, reg = get_loc_and_address_reg p_val in
    Printf.sprintf "\\0(tag(%s), %s)" loc reg
  ) in
  let b_pte_nodes = update_labels branching pte_access (fun p_val ->
    let loc, reg = get_loc_and_address_reg p_val in
    Printf.sprintf "\\0(PTE(%s), %s)" loc reg
  ) in
  let b_nodes = StringMap.union_std (fun key _ _ ->
    Warn.fatal "Found %s which is both a colour and pte branching effect\n" key
  ) bcolour_nodes b_pte_nodes in

  List.map (function
    | ParsedStmt.Node n when StringMap.mem n.ParsedNode.name b_nodes ->
      let value = StringMap.find n.ParsedNode.name b_nodes in
      let attrs = List.map (fun a ->
        if a.ParsedAttr.name = "label" then
          { a with ParsedAttr.value=value }
        else
          a
      ) n.ParsedNode.attrs in
      ParsedStmt.Node { n with ParsedNode.attrs = attrs }
    | stmt -> stmt
  ) stmts

let flatten_subgraphs stmts =
  let rec flatten stmt acc = match stmt with
  | ParsedStmt.Subgraph s ->
      List.fold_right flatten s.ParsedSubgraph.stmts acc
  | stmt -> stmt :: acc in
  List.fold_right flatten stmts []

let get_param_maps stmts instr =
  let regex = Str.regexp {|\([A-Z]+\)\( \([][,a-zA-Z0-9_]+\)\)?|} in
  if not (Str.string_match regex instr 0) then
    Warn.fatal "Instr validation did not work. %s is malformed" instr;
  let instr_mnemonic = Str.matched_group 1 instr in
  let instr_params = try
    let params = Str.matched_group 3 instr in
    String.split_on_char ',' params
  with Not_found -> [] in
  let gpreg_regex = Str.regexp {|\[?\([BHWXQ]\)\([a-z0-9]+\)\]?|} in
  let md_instr_params = List.map (fun param ->
    if Str.string_match gpreg_regex param 0 then
      Str.matched_group 1 param ^ "~" ^ Str.matched_group 2 param ^ "~"
    else param
  ) instr_params in
  let graph_instr_params = List.map (fun param ->
    if Str.string_match gpreg_regex param 0 then
      Str.matched_group 1 param ^ Str.matched_group 2 param
    else param
  ) instr_params in

  let str = instr_mnemonic ^ {|\( \([][,a-zA-Z0-9_]+\)\)?|} in
  let regex = Str.regexp str in
  let stmt = try List.find (function
    | ParsedStmt.Node n ->
      let value = get_label_value n.ParsedNode.attrs in
      begin try
        ignore (Str.search_backward regex value (String.length value - 1));
        true
      with Not_found -> false
      end
    | _ -> false
  ) stmts
  with Not_found ->
    Warn.fatal "Unable to find instr %s in dot nodes labels" instr_mnemonic in

  let value = match stmt with
  | ParsedStmt.Node n -> get_label_value n.ParsedNode.attrs
  | _ -> Warn.fatal "This is not a dot graph node" in

  let read_params = try
    let params = Str.matched_group 2 value in
    String.split_on_char ',' params
  with Not_found -> [] in
  let gpreg_regex = Str.regexp {|\[?[BHWXQ]\([0-9]+\)\]?|} in
  let read_params = List.map (fun param ->
    if Str.string_match gpreg_regex param 0 then
      let index = Str.matched_group 1 param in
      "X" ^ index, "[BHWXQ]" ^ index
    else param, param
  ) read_params in
  let md_read_params, graph_read_params = List.split read_params in

  if (List.length md_read_params) <> (List.length md_instr_params) then
    Warn.user_error "Passed instr param and read param lists have \
      different lengths";
  let md_map = List.fold_left2 (fun map p1 p2 ->
    StringMap.add p1 p2 map
  ) StringMap.empty md_read_params md_instr_params in
  let graph_pairs = List.fold_right2 (fun p1 p2 acc ->
    (p1, p2) :: acc
  ) graph_read_params graph_instr_params [] in
  md_map, graph_pairs

let tr parsed_graph instr =
  let flattened_stmts = flatten_subgraphs parsed_graph.ParsedDotGraph.stmts in
  let md_param_map, graph_param_pairs = match instr with
  | Some s -> get_param_maps flattened_stmts s
  | None -> StringMap.empty, [] in

  let stmts = convert_bnodes flattened_stmts in  
  let translated = List.fold_left (fun acc stmt ->
    tr_stmt acc stmt md_param_map
  ) empty stmts in

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

  (* Sort the nodes in the original parsed graph according to the topological order *)
  let parsed_nodes = List.filter_map (function
    | ParsedStmt.Node n -> if is_init_event n then None else Some n
    | _ -> None
  ) flattened_stmts in

  let parsed_nodes = List.sort (fun n1 n2 ->
    cmp_nodes n1.ParsedNode.name n2.ParsedNode.name
  ) parsed_nodes in

  (* Compute the regex to search for in the label, which is the read param,
     preceded by an optional thread number and followed by an optional
     access size. These two can be present when the param is a gp register *)
  let param_replacements = List.map (fun (key, v) ->
    let str1 = "\\(R\\|W\\)[0-9]:" ^ key ^ "[a-z]?" in
    let str2 = "\\([0-9]:\\)?" ^ key ^ "[a-z]?" in
    let regex1 = Str.regexp str1 in
    let regex2 = Str.regexp str2 in
    let v1 = "\\1 " ^ v in
    regex1, v1, regex2, v
  ) graph_param_pairs in
  (* In case -instr was not passed, we need to just get rid of access sizes *)
  let param_replacements = if param_replacements = [] then
    let regex1 = Str.regexp {|\(R\|W\)[0-9]:\([A-Z][0-9]+\)[a-z]?|} in
    let v1 = "\\1 \\2" in
    let regex2 = Str.regexp {|\([0-9]:\)?\([A-Z][0-9]+\)[a-z]?|} in
    let v2 = "\\2" in
    [regex1, v1, regex2, v2]
  else param_replacements in

  (* Convert read to instr params, remove access sizes and tag every effect with Ei,
     where i is its index in the topological order, and get rid of everything
     following a \l - eg. thread, poi and instruction information *)
  let tag_regex = Str.regexp {|[a-zA-Z0-9_]+:|} in
  let newline_regex = Str.regexp {|\\l|} in
  let adapted_parsed_nodes = List.mapi (fun i n ->
    let value = get_label_value n.ParsedNode.attrs in
    let value = if Str.string_match tag_regex value 0 then
      let tag = Printf.sprintf "E%d" (i + 1) in
      let templ = tag ^ ":" in
      Str.replace_first tag_regex templ value
    else value in
    let value = List.fold_left (fun value (regex1, v1, regex2, v2) ->
      let value = Str.global_replace regex1 v1 value in
      Str.global_replace regex2 v2 value
    ) value param_replacements in
    let value = try
      let pos = Str.search_forward newline_regex value 0 in
      String.sub value 0 pos
    with Not_found -> value in
    let attrs = List.map (fun a ->
      if a.ParsedAttr.name = "label" then
        { a with ParsedAttr.value=value }
      else
        a
    ) n.ParsedNode.attrs in
    { n with ParsedNode.attrs = attrs }
  ) parsed_nodes in
  let adapted_parsed_nodes = List.fold_left (fun map n ->
    StringMap.add n.ParsedNode.name n map
  ) StringMap.empty adapted_parsed_nodes in

  (* Reinsert the parsed nodes into the original graph, in the same order *)
  let adapted_parsed_stmts = List.map (function
    | ParsedStmt.Node n ->
        let node = if is_init_event n then n
        else StringMap.find n.ParsedNode.name adapted_parsed_nodes in
        ParsedStmt.Node node
    | stmt -> stmt
  ) flattened_stmts in
  let adapted_parsed_graph = { parsed_graph with ParsedDotGraph.stmts = adapted_parsed_stmts } in

  (* Comparison function on edges - prioritises the in-node over
     the out-node, because once we start describing edges going
     inside a node, we want to describe all of those edges *)
  let cmp_edges e1 e2 =
    let cmp_right = cmp_nodes e1.Edge.right e2.Edge.right in
    let cmp_left = cmp_nodes e1.Edge.left e2.Edge.left in
    if cmp_right <> 0 then cmp_right else cmp_left in

  let sorted_edges = List.sort cmp_edges translated.edges in
  let sorted_translated_graph = { translated with edges = sorted_edges } in
  sorted_translated_graph, adapted_parsed_graph

let describe g =
  let module EdgeMap = MyMap.Make(struct
    type t = Edge.kind
    let compare = compare
  end) in
  
  let edge_codes = [
    (Edge.Data, ("D", 1));
    (Edge.Control, ("C", 1));
    (Edge.Order, ("O", 1))
  ] in
  let edge_codes = List.fold_left (fun map (key, value) ->
    EdgeMap.add key value map
  ) EdgeMap.empty edge_codes in

  let get_code kind map =
    let s, i = EdgeMap.find kind map in
    let code = Printf.sprintf "%s~%d~" s i in
    let map = EdgeMap.add kind (s, i + 1) map in
    code, map in

  let descs, _ = List.fold_left (fun (res, map) edge ->
    let edge_desc = edge.Edge.desc in
    try
      let lhs = StringMap.find edge.Edge.left g.nodes in
      let rhs = StringMap.find edge.Edge.right g.nodes in
      let code, map = get_code edge.Edge.kind map in
      let desc = Printf.sprintf "-   %s: %s.\n"
        code (edge_desc lhs.Node.desc rhs.Node.desc) in
      desc :: res, map
    with Not_found ->
      Warn.fatal "Could not find one of the nodes for edge %s\n" (Edge.pp edge)
    ) ([], edge_codes) g.edges in
  String.concat "\n" (List.rev descs)
