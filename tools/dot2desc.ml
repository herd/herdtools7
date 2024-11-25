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

let prog = Sys.argv.(0)

module DescDict = struct

  (* location -> description *)
  let memloc loc =
    Printf.sprintf "\\memloc{%s}" loc

  (* location -> address register -> description *)
  let memloc_addr_by loc reg =
    Printf.sprintf "\\memlocAddrBy{%s}{%s}" loc reg

  let tagloc_of loc reg =
    Printf.sprintf "\\taglocOf{%s}{%s}" loc (memloc_addr_by loc reg)

  let pte_of loc reg =
    Printf.sprintf "\\PTEof{%s}" (memloc_addr_by loc reg)

  let pa_of loc reg =
    Printf.sprintf "\\PAof{%s}" (memloc_addr_by loc reg)

  let tlb_of loc reg =
    Printf.sprintf "\\TLBof{%s}" (memloc_addr_by loc reg)

  (* register -> description *)
  let reg reg =
    Printf.sprintf "\\reg{%s}" reg

  (* register1 -> register2 -> description *)
  let reg_pair reg1 reg2 =
    Printf.sprintf "\\regPair{%s}{%s}" reg1 reg2

  (* Helpers *)
  let mre_of arg is_explicit =
    if is_explicit then
      Printf.sprintf "\\ExpMREof{%s}" arg
    else
      Printf.sprintf "\\ImpMREof{%s}" arg

  let mwe_of arg is_explicit =
    if is_explicit then
      Printf.sprintf "\\ExpMWEof{%s}" arg
    else
      Printf.sprintf "\\ImpMWEof{%s}" arg

  (* location -> address register -> is_explicit -> description *)
  let mem_read loc reg is_explicit =
    let arg = match reg with
    | Some r -> memloc_addr_by loc r
    | None -> memloc loc in
    mre_of arg is_explicit

  let mem_write loc reg is_explicit =
    let arg = match reg with
    | Some r -> memloc_addr_by loc r
    | None -> memloc loc in
    mwe_of arg is_explicit

  let tag_read loc reg is_explicit =
    if is_explicit then
      Printf.sprintf "\\ExpTagMREof{%s}" (tagloc_of loc reg)
    else
      Printf.sprintf "\\ImpTagMREof{%s}" (tagloc_of loc reg)

  let tag_write loc reg is_explicit =
    if is_explicit then
      Printf.sprintf "\\ExpTagMWEof{%s}" (tagloc_of loc reg)
    else  
      Printf.sprintf "\\ImpTagMWEof{%s}" (tagloc_of loc reg)

  let pte_read loc reg is_explicit =
    let arg = pte_of loc reg in
    mre_of arg is_explicit

  let pte_write loc reg is_explicit =
    let arg = pte_of loc reg in
    mwe_of arg is_explicit

  let pa_read loc reg is_explicit =
    let arg = pa_of loc reg in
    mre_of arg is_explicit

  let pa_write loc reg is_explicit =
    let arg = pa_of loc reg in
    mwe_of arg is_explicit

  (* label -> instruction -> description *)
  let ifetch label instr =
    Printf.sprintf "\\IFetch{%s}{%s}" label instr

  (* type -> location -> description *)
  let tlbi typ loc =
    Printf.sprintf "\\TLBIof{%s}{%s}" typ loc

  (* type -> label -> description *)
  let dc typ label =
    Printf.sprintf "\\DCof{%s}{%s}" typ label
  
  let ic typ label =
    Printf.sprintf "\\ICof{%s}{%s}" typ label

  (* type -> description *)
  let generic_tlbi typ =
    Printf.sprintf "\\genericTLBI{%s}" typ

  let generic_dc typ =
    Printf.sprintf "\\genericDC{%s}" typ

  let generic_ic typ =
    Printf.sprintf "\\genericIC{%s}" typ

  (* register -> description *)
  let reg_read reg =
    Printf.sprintf "\\RREof{%s}" reg

  let reg_write reg =
    Printf.sprintf "\\RWEof{%s}" reg

  (* location -> register -> description *)
  let mte_cond loc reg =
    Printf.sprintf "\\iseqCheck{\\allocTagOf{%s}}{\\logAddrTagIn{%s}}" loc reg

  (* location -> register -> logical predicate -> description *)
  let pte_cond loc reg pred =
    Printf.sprintf "\\PTECheck{%s}{%s}{%s}" loc reg pred

  (* condition -> description *)
  let instr_cond cond =
    Printf.sprintf "\\cond{%s}" cond

  (* register -> description *)
  let any_active r =
    Printf.sprintf "\\AnyActive{%s}" (reg r)

  (* register -> index -> description *)
  let active_elem r idx =
    Printf.sprintf "\\ActiveElem{%s}{%s}" (reg r) idx

  (* lhs -> rhs -> description *)
  let eq_contents lhs rhs =
    Printf.sprintf "\\eqContentsCheck{%s}{%s}" lhs rhs

  let neq_contents lhs rhs =
    Printf.sprintf "\\neqContentsCheck{%s}{%s}" lhs rhs

  (* condition -> description *)
  let branching cond =
    Printf.sprintf "\\IntrBranching{%s}" cond

  (* description *)
  let bcc_branching = "\\BccBranching{}"
  let exc_return = "\\ExcReturn{}"
  let empty = "\\Empty{}"

  (* name -> description *)
  let fault name =
    Printf.sprintf "\\genericFault{%s}" name

  let exc_entry name =
    Printf.sprintf "\\genericExcEntry{%s}" name

  (* edge1 -> edge2 -> description *)
  let iico_data e1 e2 =
    Printf.sprintf "\\iicodata{%s}{%s}" e1 e2

  let iico_ctrl e1 e2 =
    Printf.sprintf "\\iicoctrl{%s}{%s}" e1 e2

  let iico_order e1 e2 =
    Printf.sprintf "\\iicoorder{%s}{%s}" e1 e2

  (* Map from edge name to description *)
  let edges = StringMap.empty |>
    StringMap.add "iico_data" iico_data |>
    StringMap.add "iico_ctrl" iico_ctrl |>
    StringMap.add "iico_order" iico_order
end

module DotGraph = struct
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
    type kind = Fault | Mem | Reg_Data | Branching | Reg_Other | TLBI | DC_IC | Empty
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

  (** To be called after Str.string_match was called on the appropriate regex and
    string. The regex must contain 2 groups, the second of which matching the
    location. This will search for the address register in the instruction
    using the "[Xn]" syntax. If no such register is found, it returns None in
    its place. *)
  let get_loc_and_address_reg value =
    let loc = Str.matched_group 2 value in
    (* Look for the register that was used to address memory, which
      is part of the instruction (at the end of the attribute value) *)
    let address_reg = Str.regexp {|\[\([A-Z\._]+x?[0-9]*\)\]|} in
    let reg = try
      ignore (Str.search_backward address_reg value (String.length value - 1));
      Some (Str.matched_group 1 value)
    with Not_found -> None in
    (loc, reg)

  (** To be called after Str.string_match was called on the appropriate regex
    and string. The regex must contain 2 groups: the first is a (R|W) denoting
    whether the access is a read or a write effect, and the second matches the
    location. *)
  let do_mem_access param_map value read write =
    let f = if Str.matched_group 1 value = "R" then read else write in
    let loc, reg = get_loc_and_address_reg value in
    let reg = Option.map (fun reg -> StringMap.safe_find reg reg param_map) reg in
    let is_explicit = not (str_contains value "NExp") in
    { Node.desc=f loc reg is_explicit; kind=Node.Mem }

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

  let check_regex regex value = Str.string_match regex value 0

  (** Makes use of Str.string_match. If caller uses matching functions on
      previously used regexes, make sure this function is called after
      all other matching has been performed *)
  let is_gpreg reg =
    let r = Str.regexp {|\([CXWVBHSDQZP]\|\(ZA\)\)[0-9]+|} in
    check_regex r reg

  (** Makes use of Str.string_match. If caller uses matching functions on
    previously used regexes, make sure this function is called after
    all other matching has been performed *)
  let pp_reg param_map reg =
    let tr_reg = StringMap.safe_find reg reg param_map in
    if is_gpreg reg then tr_reg else make_monospace tr_reg

  (** To be called after Str.string_match was called on the fault or
    exc_entry regexes *)
  let get_fault_name value =
    let gr = Str.matched_group 1 value in
    let els = String.split_on_char ',' gr in
    (* The name of the event should be at the end *)
    let last = List.hd (List.rev els) in
    let words = String.split_on_char ':' last in
    String.concat " " words

  let require_address_reg f name loc reg is_explicit =
    match reg with
    | Some r -> f loc r is_explicit
    | None -> Warn.fatal "No address register found for %s access\n" name

  let tag_read = require_address_reg DescDict.tag_read "tag read"
  let tag_write = require_address_reg DescDict.tag_write "tag write"
  let pte_read = require_address_reg DescDict.pte_read "PTE read"
  let pte_write = require_address_reg DescDict.pte_write "PTE write"
  let pa_read = require_address_reg DescDict.pa_read "PA read"
  let pa_write = require_address_reg DescDict.pa_write "PA write"

  (* Effect matching regexes *)
  let symbol = "\\([a-zA-Z0-9_\\+]+\\)"
  let tag = Printf.sprintf "tag(%s)" symbol
  let pte = Printf.sprintf "PTE(%s)" symbol
  let pa = Printf.sprintf "PA(%s)" symbol
  let tlb = Printf.sprintf "TLB(%s)" symbol
  let memloc_regex = Str.regexp symbol
  let tag_regex = Str.regexp tag
  let pte_regex = Str.regexp pte
  let pa_regex = Str.regexp pa
  let tlb_regex = Str.regexp tlb
  let mem_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[\([a-zA-Z0-9_\+]+\)\]|} (* eg. a: R[x] *)
  let tag_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[tag(\([a-zA-Z0-9_\+]+\))\]|} (* eg. a: R[tag(x)] *)
  let pte_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[PTE(\([a-zA-Z0-9_\+]+\))\]|} (* eg. a: R[PTE(x)] *)
  let pa_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)\[PA(\([a-zA-Z0-9_\+]+\))\]|} (* eg. a: R[PA(x)] *)
  let ifetch = Str.regexp {|[a-zA-Z0-9_]*: R\[label:\\"P[0-9]:\([a-zA-Z0-9_]+\)\\"\]IFetch=\([][,a-zA-Z0-9\._ ]+\)|} (* eg. a: R[label:\\"P0:L0\\"]IFetch=NOP *)
  let tlbi = Str.regexp {|[a-zA-Z0-9_]*: TLBI(\([A-Z0-9]+\),\[\([a-zA-Z0-9_\+()]+\)\])|} (* eg. a: TLBI(VAAE1IS,[TLB(x)]) *)
  let generic_tlbi = Str.regexp {|[a-zA-Z0-9_]*: TLBI(\([A-Z0-9]+\))|} (* no location specified - eg. a: TLBI(VAAE1IS) *)
  let dc_ic = Str.regexp {|[a-zA-Z0-9_]*: \(DC\|IC\)(\([A-Z]+\),\[label:\\"P[0-9]:\([a-zA-Z0-9_]+\)\\"\])|} (* eg. a: DC(CVAU,label:\\"P0:m0\\") *)
  let generic_dc_ic = Str.regexp {|[a-zA-Z0-9_]*: \(DC\|IC\)(\([A-Z]+\))|} (* no label specified - eg. a: DC(CVAU) *)
  let reg_access = Str.regexp {|[a-zA-Z0-9_]*: \(R\|W\)[0-9]:\([A-Z\._]+x?[0-9]*\)|} (* eg. a: R0:X0 *)
  let branching = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(\([][,a-zA-Z0-9\._\+:{}]+\)\(==\|!=\)\([][,a-zA-Z0-9\._\+:{}]+\))|} (* eg. a: Branching(pred)([x]==0:X0) *)
  let branching_mte_tag = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(color)(tag(\([a-zA-Z0-9_\+]+\)), \([A-Z\._]+x?[0-9]*\))|} (* eg. a: Branching(pred)(tag(x), X1) *)
  let branching_pte = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(PTE(\([a-zA-Z0-9_\+]+\)), \([A-Z\._]+x?[0-9]*\))\((\([a-zA-Z0-9_,:&|() ]+\))\)?|} (* eg. a: Branching(pred)(PTE(x), X1)(valid:1 && af:1) *)
  let any_active = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(AnyActive(\([A-Z\._]+x?[0-9]*\)))|} (* eg. a: Branching(pred)(AnyActive(P0)) *)
  let active_elem = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(ActiveElem(\([A-Z\._]+x?[0-9]*\), \([0-9]+\)))|} (* eg. a: Branching(pred)(ActiveElem(P0, 0)) *)
  let branching_instr_cond = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)|} (* Only match after the above branching regexes have failed, since they are not exclusive with this one. eg. a: Branching(pred) *)
  let bcc_branching = Str.regexp {|[a-zA-Z0-9_]*: Branching(bcc)|} (* eg. a: Branching(bcc) *)
  let exc_return = Str.regexp {|[a-zA-Z0-9_]*: ExcReturn|} (* eg. a: ExcReturn *)
  let fault = Str.regexp {|[a-zA-Z0-9_]*: Fault(\([a-zA-Z0-9\._:,]*\))|} (* eg. a: Fault(W,loc:x:red,TagCheck)*)
  let exc_entry = Str.regexp {|[a-zA-Z0-9_]*: ExcEntry(\([a-zA-Z0-9\._:,]*\))|} (* eg. a: ExcEntry(W,loc:x:red,TagCheck)*)
  let empty_effect = Str.regexp {|[a-zA-Z0-9_]*: Empty|} (* eg. a: \*)

  let tr_stmt acc stmt param_map =
    let pp_reg = pp_reg param_map in
    match stmt with
    | ParsedStmt.Node n ->
      let value = get_label_value n.ParsedNode.attrs in
      let do_mem_access = do_mem_access param_map value in
      let check_node r = check_regex r value in
      if is_init_event n then
        (* Skip init events *)
        acc
      else begin
        (* Identify the type of the effect. Only one regex should match.
        Ensure branching_instr_cond is only checked after all the other
        branching regexes defined above it, since they are not exclusive. *)
        let node = if check_node mem_access then
          do_mem_access DescDict.mem_read DescDict.mem_write
        else if check_node tag_access then
          do_mem_access tag_read tag_write
        else if check_node pte_access then
          do_mem_access pte_read pte_write
        else if check_node pa_access then
          do_mem_access pa_read pa_write
        else if check_node ifetch then begin
          let f = DescDict.ifetch in
          let label = Str.matched_group 1 value in
          let instr = Str.matched_group 2 value in
          { Node.desc=f label instr; kind=Node.Mem }
        end
        else if check_node tlbi then begin
          let f = DescDict.tlbi in
          let typ = make_monospace (Str.matched_group 1 value) in
          let loc = Str.matched_group 2 value in

          let reg_regex = Str.regexp {|[A-Z\._]+x?[0-9]*|} in
          let reg = try
            ignore (Str.search_backward reg_regex value (String.length value - 1));
            Str.matched_group 0 value
          with Not_found ->
            Warn.fatal "Could not find register as part of TLBI instruction" in

          let check_loc r = check_regex r loc in
          let loc = if check_loc tag_regex then
            let loc = Str.matched_group 1 loc in
            DescDict.tagloc_of loc reg
          else if check_loc pte_regex then
            let loc = Str.matched_group 1 loc in
            DescDict.pte_of loc reg
          else if check_loc pa_regex then
            let loc = Str.matched_group 1 loc in
            DescDict.pa_of loc reg
          else if check_loc tlb_regex then
            let loc = Str.matched_group 1 loc in
            DescDict.tlb_of loc reg
          else if check_loc memloc_regex then begin
            DescDict.memloc_addr_by loc reg end
          else
            Warn.fatal "Unrecognised type of location %s" loc in
          { Node.desc=f typ loc; kind=Node.TLBI }
        end
        else if check_node generic_tlbi then begin
          let f = DescDict.generic_tlbi in
          let typ = make_monospace (Str.matched_group 1 value) in
          { Node.desc=f typ; kind=Node.TLBI }
        end
        else if check_node dc_ic then begin
          let f = if Str.matched_group 1 value = "DC" then DescDict.dc
          else DescDict.ic in
          let typ = Str.matched_group 2 value in
          let label = Str.matched_group 3 value in
          { Node.desc= f typ label; kind=Node.DC_IC }
        end
        else if check_node generic_dc_ic then begin
          let f = if Str.matched_group 1 value = "DC" then DescDict.generic_dc
          else DescDict.generic_ic in
          let typ = Str.matched_group 2 value in
          { Node.desc= f typ; kind=Node.DC_IC }
        end
        else if check_node reg_access then begin
          let f = if Str.matched_group 1 value = "R" then DescDict.reg_read else DescDict.reg_write in
          let reg = pp_reg (Str.matched_group 2 value) in
          let kind = if str_contains value "(data)" then
            Node.Reg_Data
          else
            Node.Reg_Other in
          { Node.desc=f reg; kind=kind }
        end
        else if check_node branching then begin
          let f = DescDict.branching in
          let lhs = Str.matched_group 1 value in
          let rel = Str.matched_group 2 value in
          let rhs = Str.matched_group 3 value in

          (* Extracts the memory location or register (pair) name(s) out of a lhs or rhs *)
          let mem_or_reg str =
            let mem = Str.regexp {|\[\([a-zA-Z0-9_\+]+\)\]|} in
            let reg = Str.regexp {|[0-9]:\([A-Z\._]+x?[0-9]*\)|} in
            let reg_pair = Str.regexp {|{[0-9]:\([A-Z\._]+x?[0-9]*\),[0-9]:\([A-Z\._]+x?[0-9]*\)}|} in
            let check_str r = check_regex r str in
            if check_str mem then
              DescDict.memloc (Str.matched_group 1 str)
            else if check_str reg then
              DescDict.reg (pp_reg (Str.matched_group 1 str))
            else if check_str reg_pair then begin
              let reg1 = Str.matched_group 1 str in
              let reg2 = Str.matched_group 2 str in
              DescDict.reg_pair (pp_reg reg1) (pp_reg reg2)
            end
            else
              Warn.fatal "String %s contains neither a register, register pair \
                nor a memory address" str in
          
          let lhs = mem_or_reg lhs in
          let rhs = mem_or_reg rhs in
          let cond = if String.equal rel "==" then
            DescDict.eq_contents lhs rhs
          else
            DescDict.neq_contents lhs rhs in
          { Node.desc=f cond; kind=Node.Branching }
        end
        else if check_node branching_mte_tag then begin
          let f = DescDict.branching in
          let loc = Str.matched_group 1 value in
          let reg = pp_reg (Str.matched_group 2 value) in
          let cond = DescDict.mte_cond loc reg in
          { Node.desc=f cond; kind=Node.Branching }
        end
        else if check_node branching_pte then begin
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
        else if check_node any_active then begin
          let f = DescDict.branching in
          let reg = Str.matched_group 1 value in
          let cond = DescDict.any_active reg in
          { Node.desc=f cond; kind=Node.Branching }
        end
        else if check_node active_elem then begin
          let f = DescDict.branching in
          let reg = Str.matched_group 1 value in
          let idx = Str.matched_group 2 value in
          let cond = DescDict.active_elem reg idx in
          { Node.desc=f cond; kind=Node.Branching }
        end
        else if check_node branching_instr_cond then begin
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
        else if check_node bcc_branching then
          { Node.desc=DescDict.bcc_branching; kind=Node.Branching }
        else if check_node exc_return then
          { Node.desc=DescDict.exc_return; kind=Node.Branching }
        else if check_node fault then begin
          let f = DescDict.fault in
          let name = get_fault_name value in
          { Node.desc=f name; kind=Node.Fault }
          end
        else if check_node exc_entry then begin
          let f = DescDict.exc_entry in
          let name = get_fault_name value in
          { Node.desc=f name; kind=Node.Fault }
          end
        else if check_node empty_effect then
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

  (** Build map from node id to the value of its 'label' attribute *)
  let build_node_map stmts = List.fold_left (fun map stmt ->
    match stmt with
    | ParsedStmt.Node n ->
      let name = n.ParsedNode.name in
      let value = get_label_value n.ParsedNode.attrs in
      StringMap.add name value map
    | _ -> map
  ) StringMap.empty stmts

  (** Filter out all nodes that don't match regex *)
  let get_matching_nodes node_map regex =
    StringMap.filter (fun key ->
      let value = StringMap.find key node_map in
      check_regex regex value
    ) node_map

  (** Compute the list of direct iico_data predecessors for every node *)
  let get_iico_preds node_map stmts =
    let preds = StringMap.map (fun _ -> []) node_map in
    List.fold_left (fun map stmt ->
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
    ) preds stmts


  (** Get all predecessors that match pred_regex, build a regex template with
    the information extracted from them (in practice location and address
    register) and then insert it into the nodes matching node_regex *)
  let update_labels node_map preds node_regex pred_regex build_templ =
    let nodes = get_matching_nodes node_map node_regex in
    StringMap.fold (fun key value map ->
      let ps = StringMap.find key preds in
      let matching_ps = List.fold_left (fun acc p ->
        let p_val = StringMap.find p node_map in
        if check_regex pred_regex p_val then p_val :: acc else acc
      ) [] ps in
      match matching_ps with
      | [] -> map
      | p_val :: _ ->
        ignore (check_regex pred_regex p_val);
        let templ = build_templ p_val in
        let new_value = Str.replace_first node_regex templ value in
        StringMap.add key new_value map
    ) nodes StringMap.empty

  (** Considers all nodes that have an iico_data arrow from them to a PTE or MTE
    branching node, extracts the location and register from them (most likely
    they will have this information) and inserts them into the corresponding
    branching nodes.  *)
  let convert_bnodes stmts =
    let node_map = build_node_map stmts in
    let preds = get_iico_preds node_map stmts in
    let update_labels = update_labels node_map preds in

    (* Heuristic: If there is a PTE/MTE R/W predecessor that feeds its data to a
      branching effect, then probably the condition of that branching effect
      checks those values *)
    let branching_mte_tag = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)(color)|} in
    let branching = Str.regexp {|[a-zA-Z0-9_]*: Branching(pred)|} in

    let bcolour_nodes = update_labels branching_mte_tag tag_access (fun p_val ->
      (* The address register should be the same in the p_val as in value *)
      let loc, reg = get_loc_and_address_reg p_val in
      let reg = match reg with
      | Some r -> r
      | None -> Warn.fatal "Missing address register for MTE access\n" in
      Printf.sprintf "\\0(tag(%s), %s)" loc reg
    ) in
    let b_pte_nodes = update_labels branching pte_access (fun p_val ->
      let loc, reg = get_loc_and_address_reg p_val in
      let reg = match reg with
      | Some r -> r
      | None -> Warn.fatal "Missing address register for PTE access\n" in
      Printf.sprintf "\\0(PTE(%s), %s)" loc reg
    ) in
    let b_nodes = StringMap.union_std (fun key _ _ ->
      Warn.fatal "Found %s which is both a colour and pte branching effect\n" key
    ) bcolour_nodes b_pte_nodes in

    (* Add the extra information to the MTE and PTE branching nodes *)
    List.map (function
      | ParsedStmt.Node n when StringMap.mem n.ParsedNode.name b_nodes ->
        let value = StringMap.find n.ParsedNode.name b_nodes in
        let attrs = List.map (fun a ->
          if a.ParsedAttr.name = "label" then
            { a with ParsedAttr.value=value }
          else a
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

  (** Takes [instr] as passed through the command line, which must match the
    instruction that produced the dot graph, and maps the parameters of [instr]
    with the parameters the instruction uses in the dot graph. This allows us to
    replace concrete registers like [X1], and [X2] with architectural registers
    like [Xn] and [Xm]. For example, the dot graph nodes contain
    [CAS X0, X1, [X2]] in their label, and [instr] is [CAS Xs, Xt, [Xn]]. The map
    will contain the entries [X0] -> [Xs], [X1] -> [Xt] and [X2] -> [Xn]. *)
  let get_param_maps stmts instr =
    let regex = Str.regexp {|\([A-Z]+\)\( \([][,a-zA-Z0-9\._]+\)\)?|} in
    if not (check_regex regex instr) then
      Warn.fatal "Instr validation did not work. %s is malformed" instr;
    let instr_mnemonic = Str.matched_group 1 instr in
    let instr_params = try
      let params = Str.matched_group 3 instr in
      String.split_on_char ',' params
    with Not_found -> [] in
    let gpreg_regex = Str.regexp {|\[?\([BHWXQ]\)\([a-z0-9]+\)\]?|} in
    let md_instr_params = List.map (fun param ->
      if check_regex gpreg_regex param then
        Str.matched_group 1 param ^ "~" ^ Str.matched_group 2 param ^ "~"
      else param
    ) instr_params in
    let graph_instr_params = List.map (fun param ->
      if check_regex gpreg_regex param then
        Str.matched_group 1 param ^ Str.matched_group 2 param
      else param
    ) instr_params in

    let str = instr_mnemonic ^ {|\( \([][,a-zA-Z0-9\._]+\)\)?|} in
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
      if check_regex gpreg_regex param then
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
      let str1 = "\\(R\\|W\\)[0-9]:" ^ key ^ "[bhwqs]?" in
      let str2 = "\\([0-9]:\\)?" ^ key ^ "[bhwqs]?" in
      let regex1 = Str.regexp str1 in
      let regex2 = Str.regexp str2 in
      let v1 = "\\1 " ^ v in
      regex1, v1, regex2, v
    ) graph_param_pairs in
    (* In case -instr was not passed, we need to just get rid of access sizes *)
    let param_replacements = if param_replacements = [] then
      let regex1 = Str.regexp {|\(R\|W\)[0-9]:\([A-Z\._]+x?[0-9]*\)[bhwqs]?|} in
      let v1 = "\\1 \\2" in
      let regex2 = Str.regexp {|\([0-9]:\)?\([A-Z\._]+x?[0-9]*\)[bhwqs]?|} in
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
      let value = if check_regex tag_regex value then
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
end

module ParseDotFile(C: sig
    val debuglexer : bool
    val instr: string option
  end) : sig
    val parse_file : string -> string option -> DotGraph.t list
  end =
struct
  let do_parse_file channel =
    let module DotLexer = DotLexer.Make(struct
      let debug = C.debuglexer
    end) in
    let lexbuf = Lexing.from_channel channel in
    try
      let graphs = DotParser.main DotLexer.token lexbuf in
      List.map (fun g -> DotGraph.tr g C.instr) graphs
    with
    | DotParser.Error ->
      Printf.eprintf "Syntax error at position %d\n" (Lexing.lexeme_start lexbuf);
      exit 1
    | e -> raise e

  let parse_file in_filename wb_filename =
    let pairs = Misc.input_protect do_parse_file in_filename in
    let tr_graphs, parsed_graphs = List.split pairs in

    let do_writeback channel =
      let printed_parsed_graphs = List.map ParsedDotGraph.pp parsed_graphs in
      let file_contents = String.concat "\n\n" printed_parsed_graphs in
      Printf.fprintf channel "%s\n" file_contents in

    Option.iter (fun fname -> Misc.output_protect do_writeback fname) wb_filename;
    tr_graphs
end

module Make
  (O: sig
    val debug: bool
    val instr: string option
  end) =
struct

  module Parse = ParseDotFile(struct
    let debuglexer = O.debug
    let instr = O.instr
  end)

  let exec filename wb_filename =
    let graphs = Parse.parse_file filename wb_filename in
    begin match graphs with
    | [] -> Printf.printf "No graph produced from dot file %s\n" filename
    | [g] -> Printf.printf "%s" (DotGraph.describe g)
    | gs -> let descs = List.map (fun g ->
        let desc = DotGraph.describe g in
        let sentences = String.split_on_char '\n' desc in
        let indent = "    " in
        let sentences = List.map (fun s -> if s = "" then s else indent ^ s) sentences in
        let desc = String.concat "\n" sentences in
        Printf.sprintf "-   All of the following apply:\n\n%s" desc
      ) gs in
      let desc = String.concat "\n" descs in
      Printf.printf "%s" desc
    end
end

let debug = ref false
let instr = ref None
let wb_fname = ref None
let args = ref []
let get_cmd_arg s = args := s :: !args

let options = [
  ArgUtils.parse_bool
    "-debug" debug "show debug output for the dot lexer" ;
  ArgUtils.parse_string_opt "-instr" instr
    "Instance of the instruction being run, used for substitution \
    of register names and/or condition variables";
  ArgUtils.parse_string_opt "-wbfname" wb_fname
    "Name of the file where the simplified dot graph will be pasted. \
    If left blank, it defaults to the input file"
]

let () =
  try
    Arg.parse options
      get_cmd_arg
      (Printf.sprintf "Usage %s [options] [dot_file]" prog);
    if List.length !args = 0 then
      invalid_arg (Printf.sprintf "%s run with no target dot file" prog);
    if List.length !args > 1 then
      invalid_arg (Printf.sprintf "Cannot run %s on more than one dot file at once" prog);

    (* Command line parameter denoting the instruction that produced the dot
    graph, with its parameters replaced 1-for-1 with the symbolic registers
    we want to use in the description. eg. CAS Xs,Xt,[Xn] *)
    begin match !instr with
    | None -> ()
    | Some s ->
      let instr_regex = Str.regexp {|\([A-Z]+\)\( \([][a-zA-Z0-9\._]+\)\)?\(,\([][a-zA-Z0-9\._]+\)\)*$|} in
      if not (Str.string_match instr_regex s 0) then
        invalid_arg "Invalid format for command. Command must have arguments separated by \
        commas, and with no whitespaces between them. The mnemonic and the first argument \
        are separated by exactly one space (eg. LDR Xn,[Xm])"
    end;

    let module Run = Make(struct
      let debug = !debug
      let instr = !instr
    end) in
    let filename = List.hd !args in
    Run.exec filename !wb_fname
  with
  | Misc.Fatal msg -> Printf.eprintf "%s: %s\n" prog msg ; exit 2
