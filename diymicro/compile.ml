(** Transforms a cycle into a test, ie generates instructions, init and final conditions, and dumps them to a litmus test *)
module A = struct (* TODO Don't exactly know how to do modularity *)
  include AArch64
end

(** metadata needed along instructions for a litmus test *)
type t = {
  cycle: Cycle.t; (* edges, read and write values etc *)
  env: (Cycle.loc * Cycle.proc, A.reg) Hashtbl.t; (* loc, proc to register *)
  initial_values: (Cycle.loc, int) Hashtbl.t; (* initial values of loc, 0 if not present *)
  mutable final_conditions: (Cycle.proc * A.reg * int) list
}

(** Generate test from a cycle *)
let make_test (cycle: Cycle.t) =
  let loc_count = Cycle.loc_count () in
  let proc_count = Cycle.proc_count () in
  let test = {
    cycle=cycle;
    env=Hashtbl.create (proc_count*loc_count);
    initial_values=Hashtbl.create loc_count;
    final_conditions=[]
  } in
  (* initialise env *)
  for loc_i=0 to loc_count-1 do
    for proc_i=0 to proc_count-1 do
      let proc = Cycle.Proc proc_i in
      Hashtbl.add test.env (Cycle.Loc loc_i, proc) (A.next_reg proc)
    done
  done;
  test, List.init proc_count (fun _ -> []) (* TODO *)




(* Dumping test *)

let dump_init (test: t) (channel: out_channel) =
  let dump_initial_values () =
    for loc_i=0 to Cycle.loc_count ()-1 do
      let loc = Cycle.Loc loc_i in
      let init_val = match Hashtbl.find_opt test.initial_values loc with
      None -> 0 | Some v -> v
      in
      (Printf.fprintf channel "  %s = %d;\n" (Cycle.pp_location loc) init_val)
    done
  in

  let dump_env () =
    for loc_i=0 to Cycle.loc_count ()-1 do
      let loc = (Cycle.Loc loc_i) in
      "  " |> output_string channel;
      for proc_i=0 to Cycle.proc_count ()-1 do
        let proc = Cycle.Proc proc_i in
        match Hashtbl.find_opt test.env (loc, proc) with
          Some reg ->
            Printf.fprintf channel "%d: %s = %s;\t" proc_i (A.pp_reg reg) (Cycle.pp_location loc)
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
        (Cycle.pp_proc (Cycle.Proc p)::dump_pseudo cs)::
        fmt_col (p+1) k prog in
  fmt_col 0 []
  
let dump_code code channel =
  let pp = fmt_cols code in
  Misc.pp_prog channel pp


let dump_final test channel =
  let pp_clause (Cycle.Proc proc, reg, value) =
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