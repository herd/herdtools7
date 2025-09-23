type loc = Loc of int

type t = {
  free_registers : AArch64_compile.reg list; (* available registers *)
  env : (loc * AArch64_compile.reg) list;
      (* Assign a register to each location *)
  initial_values : (loc * int) list;
  final_conditions : (AArch64_compile.reg * int) list;
}
(** current state inside a single proc *)

let pp_location (Loc i) =
  if i < 3 then String.make 1 (Char.code 'x' + i |> Char.chr)
  else if i < 26 then String.make 1 (Char.code 'a' + i - 3 |> Char.chr)
  else "loc_" ^ string_of_int (i - 26)

let rec pp_env = function
  | [] -> ""
  | (loc, r) :: qe ->
      pp_location loc ^ " -> " ^ AArch64_compile.pp_reg r ^ "\n" ^ pp_env qe

let rec pp_initial_values = function
  | [] -> ""
  | (loc, v) :: q ->
      Printf.sprintf "  %s = %d;\n%s" (pp_location loc) v (pp_initial_values q)

(* new_loc: get a new location
   loc_count: get the current number of locations *)
let next_loc, loc_count, reset_loc_count =
  let counter = ref 0 in
  let inner_next_loc () =
    let loc = Loc !counter in
    let _ = incr counter in
    loc
  in
  inner_next_loc, (fun () -> !counter), fun () -> counter := 0

let next_reg (st : t) =
  match st.free_registers with
  | r :: rs -> r, {st with free_registers = rs}
  | [] -> Warn.user_error "No more free registers"

let assigned_next_loc (st : t) =
  let reg, st = next_reg st in
  let loc = next_loc () in
  loc, reg, {st with env = st.env @ [loc, reg]}

let add_condition st reg int =
  {st with final_conditions = st.final_conditions @ [reg, int]}

let set_initial st loc int =
  let rec set_initial_value = function
    | (l, _) :: _ when l = loc ->
        Warn.fatal "%s already has an initial value" (pp_location loc)
    | e :: q -> e :: set_initial_value q
    | [] -> [loc, int]
  in
  {st with initial_values = set_initial_value st.initial_values}

let set_register st loc reg =
  (* We allow a location to be assigned to multiple registers *)
  {st with env = st.env @ [loc, reg]}

let get_register st loc =
  try List.assoc loc st.env
  with Not_found -> Warn.fatal "No register assigned for %s" (pp_location loc)

(** Snippets of compilation *)

(** Compute a certain value to a register, if the register already has that
    value, *no new register is allocated*

    Therefore: don't use calc_value - or provide None as previous value if:
    - you will write to that register later: the source register could be
      already used in the final conditions or could be XZR
    - you will use it with do_add64 in a non-mixed setting *)
let calc_value st objective_value reg = function
  | Some v when v = objective_value -> [], reg, st
  | _ ->
      let new_reg, st = next_reg st in
      let ins =
        AArch64_compile.do_eor new_reg reg reg
        ::
        (if objective_value <> 0 then
           [AArch64_compile.addi new_reg new_reg objective_value]
         else [])
      in
      AArch64_compile.pseudo ins, new_reg, st
