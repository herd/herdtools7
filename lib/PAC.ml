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
type signature_ok =
  {
    (* the name of the varaible used as a virtual address to compute the pac
       field *)
    ok_name: string ;
    (* "ia", "da", "ib" or "db" *)
    key : key ;
    (* modifier: a string used as a "salt" added to the hash *)
    modifier : string ;
    (* the offset of the pointer at the time we compute the PAC field *)
    offset : int ;
  }

(* A pac field representing an error code used with pauth1 to represent the
   result of an authentication failure *)
type signature_err =
  {
    (* The name of the original virtual address *)
    err_name : string ;
    (* the error code is `true` for `DA`or `IA` otherwise *)
    is_key_a : bool ;
  }

type signature
  = Ok of signature_ok
  | Err of signature_err

let signature_name = function
  | Ok ok -> ok.ok_name
  | Err err -> err.err_name

let compare_signature_ok p1 p2 =
  match compare_key p1.key p2.key with
  | 0 ->
      begin match String.compare p1.modifier p2.modifier with
      | 0 -> begin
        match Int.compare p1.offset p2.offset with
        | 0 -> String.compare p1.ok_name p2.ok_name
        | r -> r
      end
      | r -> r
      end
  | r -> r

let compare_signature_err p1 p2 =
  match String.compare p1.err_name p2.err_name with
  | 0 ->
      begin match (p1.is_key_a,p2.is_key_a) with
      | true,true | false,false -> 0
      | true,false -> -1
      | false,true -> 1
      end
  | r -> r

let compare_signature p1 p2 =
  match p1,p2 with
  | Ok ok1, Ok ok2 -> compare_signature_ok ok1 ok2
  | Err err1,Err err2 -> compare_signature_err err1 err2
  | Ok _,Err _ -> -1
  | Err _,Ok _ -> 1

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
  let x = Ok {ok_name=name; offset; key; modifier} in
  if PacSet.mem x pac
  then PacSet.remove x pac
  else PacSet.add x pac

let error name key  =
  match key with
  | DA | IA ->
      PacSet.singleton (Err {err_name= name; is_key_a= true})
  | DB | IB ->
      PacSet.singleton (Err {err_name= name; is_key_a= false})

(* Return the exclusive XOR of two sets of PAC fields, can be optimised but
   it's probably not very usefull as the size of the equations will be very
   small for small programs... *)
let xor (x: t) (y: t) : t =
  PacSet.diff (PacSet.union x y) (PacSet.inter x y)

let compare = PacSet.compare

let equal = PacSet.equal

let get_name p =
  match PacSet.choose_opt p with
  | Some s -> Some (signature_name s)
  | None -> None

let pp pac s offset =
  (* Pretty print s+offset *)
  let pp_index s offset =
    if offset > 0
    then Printf.sprintf "%s+%d" s offset
    else if offset < 0
      then Printf.sprintf "%s-%d" s (-offset)
      else s in
  (* Take a PAC field as a list of signatures `l`, and an offset and pretty
     print `l+offset` *)
  let rec do_rec offset = function
    | [] -> pp_index s offset
    | Ok ok :: xs ->
        pp_index
          (Printf.sprintf "pac%s(%s,%s)"
            (pp_lower_key ok.key) (do_rec ok.offset xs) ok.modifier)
          (offset - ok.offset)
    | Err err :: xs ->
        pp_index
          (Printf.sprintf "non-canonical(%s,%s)"
            (do_rec 0 xs) (if err.is_key_a then "A" else "B"))
          offset
  in
  do_rec offset (PacSet.elements pac)

(* A simplex like solver for linear constraints over PAC fields (linear
   because of the XOR), it use a bi-partition of the variables in two set:
   the basic variables and the non-basic varaibles (a variable is a PAC
   field), and canonical is 0 by construction *)
type solver_state =
  {
    (* For each basic variables, an equality of the form
          b_i = n_1 ^ ... ^ n_k
      with {n_1, ..., n_k} a set of non basic variables
      In addition, all the basic variables must be of
      the form `Ok _`, to ensure we never have an
      unsatisfiable of the form `Err b_i = Err n_j`
    *)
    equalities: PacSet.t PacMap.t;
    (* a set of inequalities of the form n_1 ^ ... ^ n_k != 0, k must be
       at least 1, such that each inequality is not empty (otherwise we found
       a contradiction), {n_1, ..., n_k} must be a set of non-basic variables *)
    inequalities: PacSet.t list;
  }

(* Comparison is used for final state pretty printing so we don't compare using
 the inequalities*)
let compare_solver_state s1 s2 =
  PacMap.compare PacSet.compare s1.equalities s2.equalities

(* Only pretty print the equalities: by default every elements are differents *)
let pp_solver solver =
  let pp_xor name set = pp set name 0 in
  PacMap.fold (fun x def s ->
    let name = signature_name x in
    Printf.sprintf " %s=%s;%s" (pp_xor name (PacSet.singleton x)) (pp_xor name def) s)
  solver.equalities ""

let empty_solver = {equalities= PacMap.empty; inequalities= []}

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

(* Return the minimal `Ok _` variable from a simplified formula *)
let new_basic_variable (x: t) : signature option =
  PacSet.fold (fun p min_opt ->
    match (p,min_opt) with
    | Ok _, None -> Some p
    | Ok _, Some min ->
        if compare_signature p min > 0
        then Some min
        else Some p
    | Err _,_ ->
        min_opt
  ) x None

(* Add the equality in a solver state and return the new solver state *)
let add_equality (x: t) (y: t) (state: solver_state) : solver_state option =
  (* the "new equality" to add to the solver if not empty *)
  let new_eq = simplify (xor x y) state.equalities in

  match new_basic_variable new_eq with
  | None ->
      if PacSet.is_empty new_eq
      then Some state
      else None
  | Some pivot ->
    let def = PacSet.remove pivot new_eq in

    (* simplify all the current equations with the new one *)
    let simplified_equalities = PacMap.map
      (fun x -> simplify x (PacMap.singleton pivot def))
      state.equalities
    in

    (* simplify all the current inequations with the new one *)
    let simplified_inequalities = List.map
      (fun x -> simplify x (PacMap.singleton pivot def))
      state.inequalities
    in

    let new_state = {
      (* add the new equality *)
      equalities = PacMap.add pivot def simplified_equalities;
      inequalities = simplified_inequalities
    } in

    if List.exists PacSet.is_empty new_state.inequalities
    then None (* We found a contradiction *)
    else Some new_state (* No contradiction found *)

(* Add the inequality x != y (rewrite x ^ y != 0) to the solver state *)
let add_inequality (x: t) (y: t) (state: solver_state) : solver_state option =
  let inequality = simplify (xor x y) state.equalities in
  if PacSet.is_empty inequality
  then None
  else
    (* If we have more than `2^n - 1` inequalities the pivot algorithm is not
     * sound with `n` the size of the pac field in bits, and this size is at
     * least `15` bits *)
    if List.length state.inequalities > 32767
    then Warn.user_error "PAC fields solver: too many inequalities to be sound"
    else Some {state with inequalities = inequality :: state.inequalities}

let get_equalities (st: solver_state) : (t * t) list =
  PacMap.fold (fun x y acc -> (PacSet.singleton x, y) :: acc) st.equalities []

let get_inequalities (st: solver_state) : (t * t) list =
  List.map (fun x -> (x,canonical)) st.inequalities

let rec add_equalities (eqs: (t * t) list) (st: solver_state) : solver_state option =
  match eqs with
  | (x,y) :: eqs ->
    Option.bind (add_equality x y st) (add_equalities eqs)
  | [] ->
      Some st

let rec add_inequalities (eqs: (t * t) list) (st: solver_state) : solver_state option =
  match eqs with
  | (x,y) :: eqs ->
    Option.bind (add_inequality x y st) (add_inequalities eqs)
  | [] ->
      Some st

let rec conjunction s1 s2 : solver_state option =
  if PacMap.cardinal s1.equalities < PacMap.cardinal s2.equalities
  then conjunction s2 s1
  else
    match add_equalities (get_equalities s2) s1 with
    | Some s1 -> add_inequalities (get_inequalities s2) s1
    | None -> None
