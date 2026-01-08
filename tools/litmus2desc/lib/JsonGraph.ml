(* JSON graph decoders for herd's JSON output. *)

module Decoders = struct
  module D = Decoders_yojson.Basic.Decode
  open D.Infix
  open Execution

  let d_dir : dir D.decoder =
    let* s = D.string in
    match s with
    | "R" -> D.succeed R
    | "W" -> D.succeed W
    | _ -> D.fail "invalid direction"

  let d_event_ref : event_ref D.decoder =
    let+ eiid = D.field "eiid" D.int in
    { eiid }

  let d_rel_edge : rel_edge D.decoder =
    let* src = D.field "src" d_event_ref in
    let+ tgt = D.field "tgt" d_event_ref in
    { src; tgt }

  let d_location : location D.decoder =
    let* ty = D.field "type" D.string in
    match ty with
    | "reg" ->
        let* proc = D.field "proc" D.int in
        let+ reg_pretty = D.field "reg_pretty" D.string in
        Reg { proc; reg_pretty }
    | "global" ->
        let+ global_pretty = D.field "global_pretty" D.string in
        Global global_pretty
    | _ -> D.fail "unknown location type"

  let d_cst : cst D.decoder =
    let* ty = D.field "type" D.string in
    match ty with
    | "concrete" ->
        let+ scalar_pretty = D.field "scalar_pretty" D.string in
        Concrete { scalar_pretty }
    | "symbolic" ->
        let+ symbol_pretty = D.field "symbol_pretty" D.string in
        Symbolic { symbol_pretty }
    | "pteval" ->
        let* pteval_pretty = D.field "pteval_pretty" D.string in
        let+ as_physical = D.maybe (D.field "as_physical" D.string) in
        Pteval { pteval_pretty; as_physical }
    | _ -> D.fail "unknown constant value type"

  let d_value : value D.decoder =
    let* ty = D.field "type" D.string in
    match ty with
    | "var" ->
        let+ v = D.field "var" D.int in
        Var v
    | "val" ->
        let d_val_pretty =
          let+ vp = D.field "val_pretty" D.string in
          Val_pretty vp
        in
        let d_val =
          let+ v = D.field "val" d_cst in
          Val v
        in
        D.one_of [ ("val_pretty", d_val_pretty); ("val", d_val) ]
    | _ -> D.fail "unknown value type"

  let d_mem_action : mem_action D.decoder =
    let+ dir = D.field "dir" d_dir
    and+ loc = D.field "loc" d_location
    and+ value = D.field "value" d_value
    and+ is_implicit = D.field_opt_or ~default:false "is_implicit" D.bool
    and+ is_atomic = D.field_opt_or ~default:false "is_atomic" D.bool in
    { dir; loc; value; is_implicit; is_atomic }

  let d_reg_action : reg_action D.decoder =
    let+ dir = D.field "dir" d_dir
    and+ loc = D.field "loc" d_location
    and+ value = D.field "value" d_value in
    { dir; loc; value }

  let d_action : action D.decoder =
    let* ty = D.field "type" D.string in
    match ty with
    | "mem" ->
        let+ c = d_mem_action in
        Memory c
    | "reg" ->
        let+ c = d_reg_action in
        Register c
    | "barrier" ->
        let+ barrier_pretty = D.field "barrier_pretty" D.string in
        Barrier { barrier_pretty }
    | "commit" ->
        let+ commit = D.field "commit" D.string in
        Commit { commit }
    | "fault" -> D.succeed Fault
    | _ -> D.fail "unknown action type"

  let d_iiid : iiid D.decoder =
    let d_id =
      let* proc = D.field "proc" D.int in
      let* poi = D.field "poi" D.int in
      let+ inst_pretty = D.field "inst_pretty" D.string in
      Index { proc; poi; inst_pretty }
    in
    let d_init_spurious =
      let* s = D.string in
      match s with
      | "init" -> D.succeed Init
      | "spurious" -> D.succeed Spurious
      | _ -> D.fail "not a simple iiid string"
    in
    D.one_of [ ("init_spurious", d_init_spurious); ("id", d_id) ]

  let d_event : event D.decoder =
    let* act = D.field "act" d_action in
    let* eiid = D.field "eiid" D.int in
    let+ iiid = D.field "iiid" d_iiid in
    { act; eiid; iiid }

  open struct
    open RFMap

    let d_rfmap_src : src D.decoder =
      let* ty = D.field "type" D.string in
      match ty with
      | "final" ->
          let+ loc = D.field "loc" d_location in
          Final loc
      | "load" ->
          let+ event = D.field "event" d_event_ref in
          Load event
      | _ -> D.fail "unknown rfmap src"

    let d_rfmap_tgt : tgt D.decoder =
      let* ty = D.field "type" D.string in
      match ty with
      | "init" -> D.succeed Init
      | "store" ->
          let+ event = D.field "event" d_event_ref in
          Store event
      | _ -> D.fail "unknown rfmap tgt"

    let d_rfmap_edge : edge D.decoder =
      let* src = D.field "src" d_rfmap_src in
      let+ tgt = D.field "tgt" d_rfmap_tgt in
      { src; tgt }
  end

  let d_viewed_before : viewed_before D.decoder =
    D.key_value_pairs (D.list d_rel_edge)

  let decode : t D.decoder =
    let* events = D.field "events" (D.list d_event) in
    let* iico_data = D.field "iico_data" (D.list d_rel_edge) in
    let* iico_ctrl = D.field "iico_ctrl" (D.list d_rel_edge) in
    let* speculated = D.field "speculated" (D.list d_event_ref) in
    let* rfmap = D.field "rfmap" (D.list d_rfmap_edge) in
    let* viewed_before = D.field "viewed_before" d_viewed_before in
    let+ visible_po = D.maybe (D.field "visible_po" (D.list d_rel_edge)) in
    {
      events;
      iico_data;
      iico_ctrl;
      speculated;
      rfmap;
      viewed_before;
      visible_po;
    }

  let decode_list : t list D.decoder = D.list decode

  let decode_string (s : string) : (t list, string) result =
    D.decode_string decode_list s |> Result.map_error D.string_of_error
end

let decode_json_string = Decoders.decode_string
