type key = DA | DB | IA | IB

let pp_upper_key = function
  | IA -> "IA"
  | IB -> "IB"
  | DA -> "DA"
  | DB -> "DB"

let pp_lower_key = function
  | IA -> "ia"
  | IB -> "ib"
  | DA -> "da"
  | DB -> "db"

let parse_key = function
  | "ia" | "IA" -> IA
  | "ib" | "IB" -> IB
  | "da" | "DA" -> DA
  | "db" | "DB" -> DB
  | s -> Warn.user_error "PAC fields must use keys in {da,db,ia,ib}, found %s" s

let compare_key x y =
  match x,y with
  | DA, DA | DB, DB | IA, IA | IB, IB -> 0
  | DA, _ | DB, IA | _, IB -> -1
  | _, _ -> 1

(* A vritual address must have a PAC field represented in the most significant
   bits of the pointer, this PAC field may contani multiple PAC signatures using
   the XOR off all the signatures *)
type signature =
  {
    (* the name of the varaible used as a virtual address to compute the pac
       field *)
    name: string ;
    (* "ia", "da", "ib" or "db" *)
    key : key ;
    (* modifier: a string used as a "salt" added to the hash *)
    modifier : string ;
    (* the offset of the pointer at the time we compute the PAC field *)
    offset : int ;
  }

let pp_signature p s =
  if Misc.int_eq p.offset 0
  then Printf.sprintf "pac%s(%s, %s)" (pp_lower_key p.key) s p.modifier
  else Printf.sprintf "pac%s(%s, %s, %d)" (pp_lower_key p.key) s p.modifier p.offset

let compare_signature p1 p2 =
  match compare_key p1.key p2.key with
  | 0 ->
      begin match String.compare p1.modifier p2.modifier with
      | 0 -> begin
        match Int.compare p1.offset p2.offset with
        | 0 -> String.compare p1.name p2.name
        | r -> r
      end
      | r -> r
      end
  | r -> r

module PacSet = Set.Make (struct
  type t = signature
  let compare = compare_signature
end)

module PacSetSet = Set.Make (PacSet)

module PacMap = Map.Make (struct
  type t = signature
  let compare = compare_signature
end)

type t = PacSet.t

let canonical = PacSet.empty

let is_canonical pac =
  PacSet.is_empty pac

(* add a PAC signature in a PAC field using an exclusive OR *)
let add name key modifier offset pac =
  if PacSet.mem {name; modifier; key; offset} pac then
    PacSet.remove {name; modifier; key; offset} pac
  else
    PacSet.add {name; modifier; key; offset} pac

(* Return the exclusive XOR of two sets of PAC fields, can be optimised but
   it's probably not very usefull as the size of the equations will be very
   small for small programs... *)
let xor (x: t) (y: t) : t =
  PacSet.diff (PacSet.union x y) (PacSet.inter x y)

let compare = PacSet.compare

let equal = PacSet.equal

let pp pac s =
  let rec aux = function
    | x :: xs -> pp_signature x (aux xs)
    | [] -> s
  in
  aux (PacSet.to_list pac)

(* A simplex like solver for linear constraints over PAC fields (linear
   because of the XOR), it use a bi-partition of the variables in two set:
   the basic variables and the non-basic varaibles (a variable is a PAC
   field), and canonical is 0 by construction *)
type solver_state =
  {
    (* For each basic variables, an equality of the form
          b_i = n_1 ^ ... ^ n_k
      with {n_1, ..., n_k} a set of non basic variables
    *)
    equalities: PacSet.t PacMap.t;
    (* a set of inequalities of the form n_1 ^ ... ^ n_k != 0, k must be
       at least 1, such that each inequality is not empty (otherwise we found
       a contradiction), {n_1, ..., n_k} must be a set of non-basic variables *)
    inequalities: PacSetSet.t;
  }

(* Comparison is used for final state pretty printing so we don't compare using
 the inequalities*)
let compare_solver_state s1 s2 =
  PacMap.compare PacSet.compare s1.equalities s2.equalities

(* Only pretty print the equalities: by default every elements are differents *)
let pp_solver solver =
  let rec aux name = function
    | x :: xs ->
        pp_signature x (aux name xs)
    | [] -> name
  in
  let pp_xor name set = aux name (PacSet.to_list set) in
  PacMap.fold (fun x def s ->
    Printf.sprintf " %s=%s;%s" (pp_signature x x.name) (pp_xor x.name def) s)
  solver.equalities ""

let empty_solver = {equalities= PacMap.empty; inequalities= PacSetSet.empty}

(* simplify all the basic variables by their definition *)
(* so the output only contain non-basic variables *)
let simplify (x: t) (equations: t PacMap.t) : t =
  PacSet.fold (fun var acc ->
      if PacMap.mem var equations
      then
        (* `var` is a basic variable, so we replace it with it's "definition"
           in equations *)
        xor
          (PacSet.remove var acc)
          (PacMap.find var equations) (* only-contain non-basic variables *)
      else acc
    ) x x

let normalize (x: t) {equalities} = simplify x equalities

(* Add the equality in a solver state and return the new solver state *)
let add_equality (x: t) (y: t) (state: solver_state) : solver_state option =
  (* the "new equality" to add to the solver if not empty *)
  let new_eq = simplify (xor x y) state.equalities in

  if PacSet.is_empty new_eq then Some state else begin
    let pivot = PacSet.min_elt new_eq in
    let def = PacSet.remove pivot new_eq in

    (* simplify all the current equations with the new one *)
    let simplified_equalities = PacMap.map
      (fun x -> simplify x (PacMap.singleton pivot def))
      state.equalities
    in

    (* simplify all the current inequations with the new one *)
    let simplified_inequalities = PacSetSet.map
      (fun x -> simplify x (PacMap.singleton pivot def))
      state.inequalities
    in

    let new_state = {
      (* add the new equality *)
      equalities = PacMap.add pivot def simplified_equalities;
      inequalities = simplified_inequalities
    } in

    if PacSetSet.exists PacSet.is_empty new_state.inequalities
    then None (* We found a contradiction *)
    else Some new_state (* No contradiction found *)
  end

(* Add the inequality x != y (rewrite x ^ y != 0) to the solver state *)
let add_inequality (x: t) (y: t) (state: solver_state) : solver_state option =
  let inequality = simplify (xor x y) state.equalities in
  if PacSet.is_empty inequality
  then None
  else
    (* If we have more than 2^n - 1 inequalities the pivot algorithm is not sound *)
    if PacSetSet.cardinal state.inequalities > 32767
    then Warn.user_error "PAC fields solver: too many inequalities to be sound"
    else Some {state with inequalities = PacSetSet.add inequality state.inequalities}
