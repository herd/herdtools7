type identifier = AST.identifier

module ISet = ASTUtils.ISet
module IMap = ASTUtils.IMap

module TimeFrame = struct
  type t = Constant | Config | Execution of bool

  let equal t1 t2 =
    match (t1, t2) with
    | Constant, Constant | Config, Config -> true
    | Execution b1, Execution b2 -> Bool.equal b1 b2
    | Constant, (Config | Execution _)
    | Config, (Constant | Execution _)
    | Execution _, (Config | Constant) ->
        false

  let is_before t1 t2 =
    match (t1, t2) with
    | Constant, Constant | Config, Config | Execution _, Execution _ -> true
    | Config, Execution _ | Constant, (Config | Execution _) -> true
    | Execution _, Config | (Config | Execution _), Constant -> false

  let max t1 t2 = if is_before t1 t2 then t2 else t1

  let of_ldk =
    let open AST in
    function
    | LDK_Constant -> Constant
    | LDK_Let -> Execution false
    | LDK_Var -> Execution true

  let of_gdk =
    let open AST in
    function
    | GDK_Constant -> Constant
    | GDK_Config -> Config
    | GDK_Let -> Execution false
    | GDK_Var -> Execution true

  let maxs = List.fold_left max Constant
  let is_immutable = function Constant | Config -> true | Execution b -> not b
end

type t =
  | ReadLocal of identifier * TimeFrame.t
  | WriteLocal of identifier
  | ReadGlobal of identifier * TimeFrame.t
  | WriteGlobal of identifier
  | ThrowException of identifier
  | RecursiveCall of identifier
  | PerformsAssertions
  | NonDeterministic

type side_effect = t

let equal (t1 : t) (t2 : t) : bool =
  match (t1, t2) with
  | ReadLocal (s1, _), ReadLocal (s2, _)
  | WriteLocal s1, WriteLocal s2
  | ReadGlobal (s1, _), ReadGlobal (s2, _)
  | WriteGlobal s1, WriteGlobal s2
  | ThrowException s1, ThrowException s2
  | RecursiveCall s1, RecursiveCall s2 ->
      String.equal s1 s2
  | PerformsAssertions, PerformsAssertions | NonDeterministic, NonDeterministic
    ->
      true
  | ( ReadLocal _,
      ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | ThrowException _
      | RecursiveCall _ | PerformsAssertions | NonDeterministic ) )
  | ( WriteLocal _,
      ( ReadGlobal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
      | PerformsAssertions | NonDeterministic ) )
  | ( ReadGlobal _,
      ( WriteGlobal _ | ThrowException _ | RecursiveCall _ | PerformsAssertions
      | NonDeterministic ) )
  | ( WriteGlobal _,
      ( ThrowException _ | RecursiveCall _ | PerformsAssertions
      | NonDeterministic ) )
  | ThrowException _, (RecursiveCall _ | PerformsAssertions | NonDeterministic)
  | RecursiveCall _, (PerformsAssertions | NonDeterministic)
  | PerformsAssertions, NonDeterministic
  | ( ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | ThrowException _
      | RecursiveCall _ | PerformsAssertions | NonDeterministic ),
      ReadLocal _ )
  | ( ( ReadGlobal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
      | PerformsAssertions | NonDeterministic ),
      WriteLocal _ )
  | ( ( WriteGlobal _ | ThrowException _ | RecursiveCall _ | PerformsAssertions
      | NonDeterministic ),
      ReadGlobal _ )
  | ( ( ThrowException _ | RecursiveCall _ | PerformsAssertions
      | NonDeterministic ),
      WriteGlobal _ )
  | (RecursiveCall _ | PerformsAssertions | NonDeterministic), ThrowException _
  | (PerformsAssertions | NonDeterministic), RecursiveCall _
  | NonDeterministic, PerformsAssertions ->
      false

let compare (t1 : t) (t2 : t) : int =
  match (t1, t2) with
  | ReadLocal (s1, _), ReadLocal (s2, _)
  | WriteLocal s1, WriteLocal s2
  | ReadGlobal (s1, _), ReadGlobal (s2, _)
  | WriteGlobal s1, WriteGlobal s2
  | ThrowException s1, ThrowException s2
  | RecursiveCall s1, RecursiveCall s2 ->
      String.compare s1 s2
  | PerformsAssertions, PerformsAssertions | NonDeterministic, NonDeterministic
    ->
      0
  | ( ReadLocal _,
      ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | ThrowException _
      | RecursiveCall _ | PerformsAssertions | NonDeterministic ) )
  | ( WriteLocal _,
      ( ReadGlobal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
      | PerformsAssertions | NonDeterministic ) )
  | ( ReadGlobal _,
      ( WriteGlobal _ | ThrowException _ | RecursiveCall _ | PerformsAssertions
      | NonDeterministic ) )
  | ( WriteGlobal _,
      ( ThrowException _ | RecursiveCall _ | PerformsAssertions
      | NonDeterministic ) )
  | ThrowException _, (RecursiveCall _ | PerformsAssertions | NonDeterministic)
  | RecursiveCall _, (PerformsAssertions | NonDeterministic)
  | PerformsAssertions, NonDeterministic ->
      1
  | ( ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | ThrowException _
      | RecursiveCall _ | PerformsAssertions | NonDeterministic ),
      ReadLocal _ )
  | ( ( ReadGlobal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
      | PerformsAssertions | NonDeterministic ),
      WriteLocal _ )
  | ( ( WriteGlobal _ | ThrowException _ | RecursiveCall _ | PerformsAssertions
      | NonDeterministic ),
      ReadGlobal _ )
  | ( ( ThrowException _ | RecursiveCall _ | PerformsAssertions
      | NonDeterministic ),
      WriteGlobal _ )
  | (RecursiveCall _ | PerformsAssertions | NonDeterministic), ThrowException _
  | (PerformsAssertions | NonDeterministic), RecursiveCall _
  | NonDeterministic, PerformsAssertions ->
      -1

let pp_print f =
  let open Format in
  function
  | ReadLocal (s, _) -> fprintf f "ReadLocal %S" s
  | WriteLocal s -> fprintf f "WriteLocal %S" s
  | ReadGlobal (s, _) -> fprintf f "ReadGlobal %S" s
  | WriteGlobal s -> fprintf f "WriteGlobal %S" s
  | ThrowException s -> fprintf f "RaiseException %S" s
  | RecursiveCall s -> fprintf f "RecursiveCall %S" s
  | PerformsAssertions -> fprintf f "PerformsAssertions"
  | NonDeterministic -> fprintf f "NonDeterministic"

let time_frame = function
  | ReadLocal (_, t) | ReadGlobal (_, t) -> t
  | WriteLocal _ | WriteGlobal _ | NonDeterministic | RecursiveCall _
  | ThrowException _ ->
      TimeFrame.Execution true
  | PerformsAssertions -> TimeFrame.Constant

let is_pure = function
  | ReadLocal _ | ReadGlobal _ -> true
  | WriteLocal _ | WriteGlobal _ | NonDeterministic | RecursiveCall _
  | ThrowException _ | PerformsAssertions ->
      false

let is_immutable = function
  | ReadLocal (_, tf) | ReadGlobal (_, tf) -> TimeFrame.is_immutable tf
  | WriteLocal _ | WriteGlobal _ | NonDeterministic | RecursiveCall _
  | ThrowException _ | PerformsAssertions ->
      false

let is_statically_evaluable se = is_pure se && is_immutable se

(* SES = Side Effect Set *)
module SES = struct
  (* This module uses an abstraction over a set of side-effects. *)
  type t = {
    (* Decomposition into subsets *)
    local_reads : ISet.t; (* Only store reads to mutable variables *)
    local_writes : ISet.t;
    global_reads : ISet.t; (* Only store reads to mutable variables *)
    global_writes : ISet.t;
    thrown_exceptions : ISet.t;
    recursive_calls : ISet.t;
    assertions_performed : bool;
    non_determinism : bool;
    (* Invariants kept *)
    max_local_read_time_frame : TimeFrame.t * identifier;
    max_global_read_time_frame : TimeFrame.t * identifier;
  }

  let empty =
    {
      local_reads = ISet.empty;
      local_writes = ISet.empty;
      global_reads = ISet.empty;
      global_writes = ISet.empty;
      thrown_exceptions = ISet.empty;
      recursive_calls = ISet.empty;
      assertions_performed = false;
      non_determinism = false;
      max_local_read_time_frame = (TimeFrame.Constant, "1");
      max_global_read_time_frame = (TimeFrame.Constant, "1");
    }

  let witnessed_time_frame_max ((t1, _w1) as tw1) ((t2, _w2) as tw2) =
    if TimeFrame.is_before t1 t2 then tw2 else tw1

  let max_time_frame ses =
    if
      ISet.is_empty ses.local_writes
      && ISet.is_empty ses.global_writes
      && ISet.is_empty ses.thrown_exceptions
      && ISet.is_empty ses.recursive_calls
      && not ses.non_determinism
    then
      TimeFrame.max
        (fst ses.max_global_read_time_frame)
        (fst ses.max_local_read_time_frame)
    else TimeFrame.Execution true

  let is_pure ses =
    ISet.is_empty ses.local_writes
    && ISet.is_empty ses.global_writes
    && ISet.is_empty ses.thrown_exceptions
    && ISet.is_empty ses.recursive_calls

  let all_reads_are_immutable ses =
    ISet.is_empty ses.local_reads && ISet.is_empty ses.global_reads

  let is_statically_evaluable ses =
    is_pure ses && (not ses.non_determinism)
    && (not ses.assertions_performed)
    && all_reads_are_immutable ses

  let is_deterministic ses = not ses.non_determinism

  let add_local_read s time_frame ses =
    let local_reads =
      if TimeFrame.is_immutable time_frame then ses.local_reads
      else ISet.add s ses.local_reads
    and max_local_read_time_frame =
      witnessed_time_frame_max (time_frame, s) ses.max_local_read_time_frame
    in
    { ses with local_reads; max_local_read_time_frame }

  let add_local_write s ses =
    { ses with local_writes = ISet.add s ses.local_writes }

  let add_global_read s time_frame ses =
    let global_reads =
      if TimeFrame.is_immutable time_frame then ses.global_reads
      else ISet.add s ses.global_reads
    and max_global_read_time_frame =
      witnessed_time_frame_max (time_frame, s) ses.max_global_read_time_frame
    in
    { ses with global_reads; max_global_read_time_frame }

  let add_global_write s ses =
    { ses with global_writes = ISet.add s ses.global_writes }

  let add_thrown_exception s ses =
    { ses with thrown_exceptions = ISet.add s ses.thrown_exceptions }

  let add_recursive_call s ses =
    { ses with recursive_calls = ISet.add s ses.thrown_exceptions }

  let set_assertions_performed ses = { ses with assertions_performed = true }
  let set_non_determinism ses = { ses with non_determinism = true }

  let add_side_effect se ses =
    match se with
    | ReadLocal (s, t) -> add_local_read s t ses
    | ReadGlobal (s, t) -> add_global_read s t ses
    | WriteLocal s -> add_local_write s ses
    | WriteGlobal s -> add_global_write s ses
    | ThrowException s -> add_thrown_exception s ses
    | RecursiveCall s -> add_recursive_call s ses
    | PerformsAssertions -> set_assertions_performed ses
    | NonDeterministic -> set_non_determinism ses

  (* Constructors *)
  let read_local s t = add_local_read s t empty
  let write_local s = add_local_write s empty
  let read_global s t = add_global_read s t empty
  let write_global s = add_global_write s empty
  let throw_exception s = add_thrown_exception s empty
  let recursive_call s = add_recursive_call s empty
  let performs_assertions = set_assertions_performed empty
  let non_deterministic = set_non_determinism empty

  let equal ses1 ses2 =
    ses1 == ses2
    || ISet.equal ses1.recursive_calls ses2.recursive_calls
       && ISet.equal ses1.global_reads ses2.global_reads
       && ISet.equal ses1.global_writes ses2.global_writes
       && ISet.equal ses1.local_reads ses2.local_reads
       && ISet.equal ses1.local_writes ses2.local_writes
       && ISet.equal ses1.thrown_exceptions ses2.thrown_exceptions
       && Bool.equal ses1.non_determinism ses2.non_determinism
       && Bool.equal ses1.assertions_performed ses2.assertions_performed
       && TimeFrame.equal
            (fst ses1.max_global_read_time_frame)
            (fst ses2.max_global_read_time_frame)
       && TimeFrame.equal
            (fst ses1.max_global_read_time_frame)
            (fst ses2.max_global_read_time_frame)

  let union ses1 ses2 =
    if ses1 == empty then ses2
    else if ses2 == empty then ses1
    else
      {
        local_reads = ISet.union ses1.local_reads ses2.local_reads;
        local_writes = ISet.union ses1.local_writes ses2.local_writes;
        global_reads = ISet.union ses1.global_reads ses2.global_reads;
        global_writes = ISet.union ses1.global_writes ses2.global_writes;
        thrown_exceptions =
          ISet.union ses1.thrown_exceptions ses2.thrown_exceptions;
        recursive_calls = ISet.union ses1.recursive_calls ses2.recursive_calls;
        assertions_performed =
          ses1.assertions_performed || ses2.assertions_performed;
        non_determinism = ses1.non_determinism || ses2.non_determinism;
        max_local_read_time_frame =
          witnessed_time_frame_max ses1.max_local_read_time_frame
            ses2.max_local_read_time_frame;
        max_global_read_time_frame =
          witnessed_time_frame_max ses1.max_global_read_time_frame
            ses2.max_global_read_time_frame;
      }

  (* Properties *)
  let is_side_effect_free ses =
    ISet.is_empty ses.global_writes
    && ISet.is_empty ses.local_writes
    && ISet.is_empty ses.thrown_exceptions
    && ISet.is_empty ses.recursive_calls
    && not ses.assertions_performed

  let is_side_effect_with_global_reads ses =
    is_side_effect_free ses && ISet.is_empty ses.global_reads

  let are_non_conflicting ses1 ses2 =
    if not (ISet.is_empty ses1.recursive_calls) then
      is_side_effect_with_global_reads ses2
    else if not (ISet.is_empty ses2.recursive_calls) then
      is_side_effect_with_global_reads ses1
    else if not (ISet.is_empty ses1.thrown_exceptions) then
      is_side_effect_free ses2
    else if not (ISet.is_empty ses2.thrown_exceptions) then
      is_side_effect_free ses1
    else
      ISet.disjoint ses1.global_writes ses2.global_writes
      && ISet.disjoint ses1.global_writes ses2.global_reads
      && ISet.disjoint ses1.global_reads ses2.global_writes
      && ISet.disjoint ses1.local_writes ses2.local_writes
      && ISet.disjoint ses1.local_writes ses2.local_reads
      && ISet.disjoint ses1.local_reads ses2.local_writes

  let get_side_effect ses =
    if not (ISet.is_empty ses.global_writes) then
      WriteGlobal (ISet.choose ses.global_writes)
    else if not (ISet.is_empty ses.local_writes) then
      WriteLocal (ISet.choose ses.local_writes)
    else if not (ISet.is_empty ses.thrown_exceptions) then
      ThrowException (ISet.choose ses.thrown_exceptions)
    else if not (ISet.is_empty ses.recursive_calls) then
      RecursiveCall (ISet.choose ses.recursive_calls)
    else if ses.assertions_performed then PerformsAssertions
    else raise Not_found

  let get_side_effect_with_reads ses =
    try get_side_effect ses
    with Not_found ->
      if not (ISet.is_empty ses.global_reads) then
        let name = ISet.choose ses.global_reads in
        ReadGlobal (name, TimeFrame.Execution true)
      else if not (ISet.is_empty ses.local_reads) then
        let name = ISet.choose ses.local_reads in
        ReadLocal (name, TimeFrame.Execution true)
      else raise Not_found

  let remove_pure ses =
    { ses with global_reads = ISet.empty; local_reads = ISet.empty }

  let choose_inter s1 s2 = ISet.inter s1 s2 |> ISet.choose

  let get_conflicting_side_effects ses1 ses2 =
    let time_frame = TimeFrame.Execution true in
    if not (ISet.is_empty ses1.thrown_exceptions) then
      (ThrowException (ISet.choose ses1.thrown_exceptions), get_side_effect ses2)
    else if not (ISet.is_empty ses2.thrown_exceptions) then
      (get_side_effect ses1, ThrowException (ISet.choose ses2.thrown_exceptions))
    else if not (ISet.is_empty ses1.recursive_calls) then
      ( RecursiveCall (ISet.choose ses1.recursive_calls),
        get_side_effect_with_reads ses2 )
    else if not (ISet.is_empty ses2.recursive_calls) then
      ( get_side_effect_with_reads ses1,
        RecursiveCall (ISet.choose ses2.recursive_calls) )
    else if not (ISet.disjoint ses1.global_writes ses2.global_writes) then
      let s = choose_inter ses1.global_writes ses2.global_writes in
      (WriteGlobal s, WriteGlobal s)
    else if not (ISet.disjoint ses1.global_writes ses2.global_reads) then
      let s = choose_inter ses1.global_writes ses2.global_reads in
      (WriteGlobal s, ReadGlobal (s, time_frame))
    else if not (ISet.disjoint ses1.global_reads ses2.global_writes) then
      let s = choose_inter ses1.global_reads ses2.global_writes in
      (ReadGlobal (s, time_frame), WriteGlobal s)
    else if not (ISet.disjoint ses1.local_writes ses2.local_writes) then
      let s = choose_inter ses1.local_writes ses2.local_writes in
      (WriteLocal s, WriteLocal s)
    else if not (ISet.disjoint ses1.local_writes ses2.local_reads) then
      let s = choose_inter ses1.local_writes ses2.local_reads in
      (WriteLocal s, ReadLocal (s, time_frame))
    else if not (ISet.disjoint ses1.local_reads ses2.local_writes) then
      let s = choose_inter ses1.local_reads ses2.local_writes in
      (ReadLocal (s, time_frame), WriteLocal s)
    else (get_side_effect ses1, get_side_effect ses2)

  let non_conflicting_union ~fail ses1 ses2 =
    if ses1 == empty then ses2
    else if ses2 == empty then ses1
    else if are_non_conflicting ses1 ses2 then union ses1 ses2
    else get_conflicting_side_effects ses1 ses2 |> fail

  let iterated_union union empty =
    (* Dichotomic implementation of iterated union. *)
    (* [unions2 acc [l1; l2; ...; l2n]] is the list
       [[union l1 l2; union l3 l4; ... union l2n-1 l2n]]. *)
    let rec unions2 acc = function
      | [] -> acc
      | [ h ] -> h :: acc
      | h1 :: h2 :: t -> unions2 (union h1 h2 :: acc) t
    in
    (* [unions li] calls [unions2] on [li] until it only has one element. *)
    let rec unions = function
      | [] -> empty
      | [ h ] -> h
      | li -> unions2 [] li |> unions
    in
    unions

  let unions = iterated_union union empty

  let non_conflicting_unions ~fail =
    iterated_union (non_conflicting_union ~fail) empty

  let union3 ses1 ses2 ses3 = union ses1 (union ses2 ses3)
  let fold_recursive_calls f ses = ISet.fold f ses.recursive_calls

  let remove_locals ses =
    {
      ses with
      local_reads = ISet.empty;
      local_writes = ISet.empty;
      max_local_read_time_frame = (TimeFrame.Constant, "1");
    }

  let remove_thrown_exceptions ses = { ses with thrown_exceptions = ISet.empty }
  let remove_recursive_calls ses = { ses with recursive_calls = ISet.empty }
  let remove_assertions ses = { ses with assertions_performed = false }
  let remove_non_determinism ses = { ses with non_determinism = false }

  let filter_thrown_exceptions f ses =
    { ses with thrown_exceptions = ISet.filter f ses.thrown_exceptions }

  let filter_recursive_calls f ses =
    { ses with recursive_calls = ISet.filter f ses.recursive_calls }

  module SESet = Set.Make (struct
    type t = side_effect

    let compare = compare
  end)

  let to_side_effect_list ses =
    let tf = TimeFrame.Execution true in
    let set_map_to_list f s = ISet.fold (fun elt accu -> f elt :: accu) s in
    let add_if elt test accu = if test then elt :: accu else accu in
    let add_from_tf f (t, w) set accu =
      if TimeFrame.equal t TimeFrame.Constant || ISet.mem w set then accu
      else f w t :: accu
    in
    []
    |> add_if PerformsAssertions ses.assertions_performed
    |> add_if NonDeterministic ses.non_determinism
    |> add_from_tf
         (fun s tf -> ReadGlobal (s, tf))
         ses.max_global_read_time_frame ses.global_reads
    |> add_from_tf
         (fun s tf -> ReadLocal (s, tf))
         ses.max_local_read_time_frame ses.local_reads
    |> set_map_to_list (fun s -> RecursiveCall s) ses.recursive_calls
    |> set_map_to_list (fun s -> ThrowException s) ses.thrown_exceptions
    |> set_map_to_list (fun s -> WriteGlobal s) ses.global_writes
    |> set_map_to_list (fun s -> ReadGlobal (s, tf)) ses.global_reads
    |> set_map_to_list (fun s -> WriteLocal s) ses.local_writes
    |> set_map_to_list (fun s -> ReadLocal (s, tf)) ses.local_reads

  let to_side_effect_set ses = to_side_effect_list ses |> SESet.of_list

  let pp_print f ses =
    let elements = to_side_effect_list ses in
    let open Format in
    let pp_sep f () = fprintf f ",@ " in
    fprintf f "@[[%a]@]" (pp_print_list ~pp_sep pp_print) elements
end
