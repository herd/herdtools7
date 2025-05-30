(** Transforms a cycle into a test, ie generates instructions, init and final conditions, and dumps them to a litmus test *)
module A = struct (* TODO Don't exactly know how to do modularity *)
  include MakeAArch64Base.Make(struct let is_morello = false end)
end

module C = struct
  include Cycle
end

(** current state inside a single proc *)
type state = {
  free_registers: A.reg list; (* available registers *)
  proc: C.proc; (* TODO juste pour enlever les warnings *)
}

(** metadata needed along instructions for a litmus test *)
type t = {
  cycle: C.t; (* edges, read and write values etc *)
  env: (C.loc * C.proc, A.reg) Hashtbl.t; (* loc, proc to register *)
  initial_values: (C.loc, int) Hashtbl.t; (* initial values of loc, 0 if not present *)
  mutable final_conditions: (C.proc * A.reg * int) list
}


let next_reg (st: state) = match st.free_registers with
  | r::rs -> r,{ st with free_registers=rs; }
  | [] -> failwith "No more free registers"


(** Returns a list of lists of nodes (cycles), splitted by proc *)
let split_by_proc (cycle: C.t) =
  let first_node = C.find_first cycle (fun c ->
    c.C.prev.C.source_event.C.proc <> Some (C.Proc 0) &&
    c.C.source_event.C.proc = Some (C.Proc 0)
  ) in
  let rec split_by_proc node =
    let next = if node.C.next != first_node then split_by_proc node.C.next else []
    in match next with
    | e::q -> (node::e)::q
    | [] -> [[node]]
  in split_by_proc cycle



let compile_event (st: state) (event: C.event) =
  let _ = event in
  st,[]

let compile_edge (st: state) (node: C.t) = 
  let _ = node in
  st,[]

(** Generate test from a cycle *)
let make_test (cycle: C.t) =
  let loc_count = C.loc_count () in
  let proc_count = C.proc_count () in
  let test = {
    cycle=cycle;
    env=Hashtbl.create (proc_count*loc_count);
    initial_values=Hashtbl.create loc_count;
    final_conditions=[]
  } in
  let compile_proc proc nodes =
    let rec init_st = function
      | C.Loc (-1) -> { free_registers = A.allowed_for_symb; proc = proc; }
      | C.Loc loc_i ->
        let st = init_st (C.Loc (loc_i-1)) in
        let r,st = next_reg st in
        Hashtbl.add test.env (C.Loc loc_i, proc) r;
        st
    in let rec compile_proc_aux st = function
      | [] -> st,[]
      | n::nq ->
        let st,evt_code = compile_event st n.C.source_event in
        let st,edge_code = compile_edge st n in
        let st,next_code = compile_proc_aux st nq in
        st,evt_code@edge_code@next_code
    in let st = init_st (C.Loc loc_count)
    in let _,ins = compile_proc_aux st nodes in ins

  in let rec compile_by_proc nodes_by_proc proc = match nodes_by_proc, proc with
    | [], _ -> []
    | nodes::nq, C.Proc proc_i ->
      let ins = compile_proc proc nodes in
      ins::(compile_by_proc nq (C.Proc (proc_i+1)))
    
  in test,compile_by_proc (split_by_proc cycle) (C.Proc 0)




(* Dumping test *)

let dump_init (test: t) (channel: out_channel) =
  let dump_initial_values () =
    for loc_i=0 to C.loc_count ()-1 do
      let loc = C.Loc loc_i in
      let init_val = match Hashtbl.find_opt test.initial_values loc with
      None -> 0 | Some v -> v
      in
      (Printf.fprintf channel "  %s = %d;\n" (C.pp_location loc) init_val)
    done
  in

  let dump_env () =
    for loc_i=0 to C.loc_count ()-1 do
      let loc = (C.Loc loc_i) in
      "  " |> output_string channel;
      for proc_i=0 to C.proc_count ()-1 do
        let proc = C.Proc proc_i in
        match Hashtbl.find_opt test.env (loc, proc) with
          Some reg ->
            Printf.fprintf channel "%d: %s = %s;\t" proc_i (A.pp_reg reg) (C.pp_location loc)
        | None -> ()
      done;
      "\n" |> output_string channel
    done
  in
  "{\n" |> output_string channel;
    dump_initial_values ();
    "\n" |> output_string channel;
    dump_env ();
  "}\n\n" |> output_string channel


(* Code pretty-print *)

let rec dump_pseudo = function
  | [] -> []
  | A.Instruction ins::rem -> A.dump_instruction ins::dump_pseudo rem
  | A.Label (lbl,ins)::rem ->
      Printf.sprintf "%s:" lbl::dump_pseudo (ins::rem)
  | A.Nop::rem -> dump_pseudo rem
  | A.Symbolic _::_ -> assert false (* no symbolic in diy *)
  | A.Macro (m,args)::rem ->
      Printf.sprintf "%s(%s)" m
        (String.concat ","
           (List.map A.pp_reg args))::
      dump_pseudo rem

let fmt_cols =
  let rec fmt_col p k = function
    | [] -> k
    | cs::prog ->
        (C.pp_proc (C.Proc p)::dump_pseudo cs)::
        fmt_col (p+1) k prog in
  fmt_col 0 []
  
let dump_code code channel =
  let pp = fmt_cols code in
  Misc.pp_prog channel pp


let dump_final test channel =
  let pp_clause (C.Proc proc, reg, value) =
    Printf.sprintf "%d:%s=%d" proc (A.pp_reg reg) value
  in let rec pp_clauses = function
    [] -> ""
  | [e] -> pp_clause e
  | e::q -> pp_clause e^" /\\ "^pp_clauses q
  in
  "exists ("^
    pp_clauses test.final_conditions^
  ")\n"
  |> output_string channel

let dump_test (test, instructions) (channel: out_channel) =
  Printf.fprintf channel "%s %s\n" (Archs.pp A.arch) "test.litmus";
  dump_init test channel;
  dump_code instructions channel;
  dump_final test channel