module Make (A : Arch_herd.S) : sig
  type action =
    | Access of Dir.dirn * A.location * A.V.v * MachSize.sz
    | TooFar of string

  include Action.S with type action := action and module A = A
end = struct
  module A = A
  module V = A.V
  open Dir

  type action =
    | Access of Dir.dirn * A.location * A.V.v * MachSize.sz
    | TooFar of string

  let mk_init_write l sz v = Access (W, l, v, sz)

  let pp_action = function
    | Access (d, l, v, _sz) ->
        Format.asprintf "%s%s=%s" (pp_dirn d) (A.pp_location l) (V.pp_v v)
    | TooFar s -> Format.asprintf "TooFar: %s" s

  (* No isync, no barriers *)
  let is_isync _ = raise Misc.NoIsync
  let pp_isync = "ISYNC" (* what is that? *)
  let is_barrier _ = false
  let barrier_of _ = None
  let same_barrier_id _ _ = false (* should not happen *)

  (* Nothing special from now *)
  let arch_sets = []
  let arch_rels = []
  let arch_dirty = []

  (* Extractors *)
  let value_of = function Access (_, _, v, _) -> Some v | _ -> None
  let read_of = function Access (R, _, v, _) -> Some v | _ -> None
  let written_of = function Access (W, _, v, _) -> Some v | _ -> None
  let location_of = function Access (_, l, _, _) -> Some l | _ -> None
  let to_fault _ = None
  let get_mem_dir = function Access (d, _, _, _) -> d | _ -> assert false
  let get_mem_size = function Access (_, _, _, sz) -> sz | _ -> assert false

  (* Predicates *)
  let is_mem_store _ = false
  let is_mem_load _ = false
  let is_additional_mem_load _ = false
  let is_mem _ = false
  let is_tag _ = false
  let is_additional_mem _ = false
  let is_atomic _ = false
  let is_fault _ = false
  let is_pte_access _ = false (* What is this? *)
  let is_explicit _ = true (* What is this? *)

  (* Only symbolic registers, no mem, and only one proc *)
  let is_reg_any a = match a with Access _ -> true | _ -> false
  let is_store a = match a with Access (W, _, _, _) -> true | _ -> false
  let is_load a = match a with Access (R, _, _, _) -> true | _ -> false
  (* *)
  let is_reg_store a _proc = is_store a
  let is_reg_load a _proc = is_load a
  let is_reg a _proc = is_reg_any a
  let is_reg_store_any a = is_store a
  let is_reg_load_any a = is_load a
  let compatible_accesses a1 a2 = is_reg_any a1 && is_reg_any a2

  (* For bell annotations *)
  let annot_in_list _ _ = false

  (* Commits *)
  let is_bcc _ = false
  let is_pred _ = false
  let is_commit _ = false

  (* TooFar *)
  let toofar s = TooFar s
  let is_toofar = function TooFar _ -> true | _ -> false

  (* Equations *)
  let undetermined_vars_in_action a =
    match a with
    | Access (_, l, v, _) ->
        V.ValueSet.union (A.undetermined_vars_in_loc l) (V.undetermined_vars v)
    | TooFar _ -> assert false

  let simplify_vars_in_action soln a =
    match a with
    | Access (d, l, v, sz) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v' = V.simplify_var soln v in
        Access (d, l', v', sz)
    | TooFar _ -> assert false
end
