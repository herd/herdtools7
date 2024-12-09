type identifier = AST.identifier

module ISet = ASTUtils.ISet
module IMap = ASTUtils.IMap

module TimeFrame = struct
  type t = Constant | Config | Execution

  let equal t1 t2 =
    match (t1, t2) with
    | Constant, Constant | Config, Config | Execution, Execution -> true
    | Constant, (Config | Execution)
    | Config, (Constant | Execution)
    | Execution, (Config | Constant) ->
        false

  let is_before t1 t2 =
    match (t1, t2) with
    | Constant, Constant | Config, Config | Execution, Execution -> true
    | Config, Execution | Constant, (Config | Execution) -> true
    | Execution, Config | (Config | Execution), Constant -> false

  let max t1 t2 = if is_before t1 t2 then t2 else t1

  let of_ldk =
    let open AST in
    function LDK_Constant -> Constant | LDK_Let | LDK_Var -> Execution

  let of_gdk =
    let open AST in
    function
    | GDK_Constant -> Constant
    | GDK_Config -> Config
    | GDK_Let | GDK_Var -> Execution
end

type read = { name : identifier; time_frame : TimeFrame.t; immutable : bool }

type t =
  | ReadsLocal of read
  | WritesLocal of identifier
  | ReadsGlobal of read
  | WritesGlobal of identifier
  | ThrowsException of identifier
  | CallsRecursive of identifier
  | PerformsAssertions
  | NonDeterministic

type side_effect = t

let equal (t1 : t) (t2 : t) : bool =
  match (t1, t2) with
  | ReadsLocal { name = s1 }, ReadsLocal { name = s2 }
  | WritesLocal s1, WritesLocal s2
  | ReadsGlobal { name = s1 }, ReadsGlobal { name = s2 }
  | WritesGlobal s1, WritesGlobal s2
  | ThrowsException s1, ThrowsException s2
  | CallsRecursive s1, CallsRecursive s2 ->
      String.equal s1 s2
  | PerformsAssertions, PerformsAssertions | NonDeterministic, NonDeterministic
    ->
      true
  | ( ReadsLocal _,
      ( WritesLocal _ | ReadsGlobal _ | WritesGlobal _ | ThrowsException _
      | CallsRecursive _ | PerformsAssertions | NonDeterministic ) )
  | ( WritesLocal _,
      ( ReadsGlobal _ | WritesGlobal _ | ThrowsException _ | CallsRecursive _
      | PerformsAssertions | NonDeterministic ) )
  | ( ReadsGlobal _,
      ( WritesGlobal _ | ThrowsException _ | CallsRecursive _
      | PerformsAssertions | NonDeterministic ) )
  | ( WritesGlobal _,
      ( ThrowsException _ | CallsRecursive _ | PerformsAssertions
      | NonDeterministic ) )
  | ThrowsException _, (CallsRecursive _ | PerformsAssertions | NonDeterministic)
  | CallsRecursive _, (PerformsAssertions | NonDeterministic)
  | PerformsAssertions, NonDeterministic
  | ( ( WritesLocal _ | ReadsGlobal _ | WritesGlobal _ | ThrowsException _
      | CallsRecursive _ | PerformsAssertions | NonDeterministic ),
      ReadsLocal _ )
  | ( ( ReadsGlobal _ | WritesGlobal _ | ThrowsException _ | CallsRecursive _
      | PerformsAssertions | NonDeterministic ),
      WritesLocal _ )
  | ( ( WritesGlobal _ | ThrowsException _ | CallsRecursive _
      | PerformsAssertions | NonDeterministic ),
      ReadsGlobal _ )
  | ( ( ThrowsException _ | CallsRecursive _ | PerformsAssertions
      | NonDeterministic ),
      WritesGlobal _ )
  | ( (CallsRecursive _ | PerformsAssertions | NonDeterministic),
      ThrowsException _ )
  | (PerformsAssertions | NonDeterministic), CallsRecursive _
  | NonDeterministic, PerformsAssertions ->
      false

let compare (t1 : t) (t2 : t) : int =
  match (t1, t2) with
  | ReadsLocal { name = s1 }, ReadsLocal { name = s2 }
  | WritesLocal s1, WritesLocal s2
  | ReadsGlobal { name = s1 }, ReadsGlobal { name = s2 }
  | WritesGlobal s1, WritesGlobal s2
  | ThrowsException s1, ThrowsException s2
  | CallsRecursive s1, CallsRecursive s2 ->
      String.compare s1 s2
  | PerformsAssertions, PerformsAssertions | NonDeterministic, NonDeterministic
    ->
      0
  | ( ReadsLocal _,
      ( WritesLocal _ | ReadsGlobal _ | WritesGlobal _ | ThrowsException _
      | CallsRecursive _ | PerformsAssertions | NonDeterministic ) )
  | ( WritesLocal _,
      ( ReadsGlobal _ | WritesGlobal _ | ThrowsException _ | CallsRecursive _
      | PerformsAssertions | NonDeterministic ) )
  | ( ReadsGlobal _,
      ( WritesGlobal _ | ThrowsException _ | CallsRecursive _
      | PerformsAssertions | NonDeterministic ) )
  | ( WritesGlobal _,
      ( ThrowsException _ | CallsRecursive _ | PerformsAssertions
      | NonDeterministic ) )
  | ThrowsException _, (CallsRecursive _ | PerformsAssertions | NonDeterministic)
  | CallsRecursive _, (PerformsAssertions | NonDeterministic)
  | PerformsAssertions, NonDeterministic ->
      1
  | ( ( WritesLocal _ | ReadsGlobal _ | WritesGlobal _ | ThrowsException _
      | CallsRecursive _ | PerformsAssertions | NonDeterministic ),
      ReadsLocal _ )
  | ( ( ReadsGlobal _ | WritesGlobal _ | ThrowsException _ | CallsRecursive _
      | PerformsAssertions | NonDeterministic ),
      WritesLocal _ )
  | ( ( WritesGlobal _ | ThrowsException _ | CallsRecursive _
      | PerformsAssertions | NonDeterministic ),
      ReadsGlobal _ )
  | ( ( ThrowsException _ | CallsRecursive _ | PerformsAssertions
      | NonDeterministic ),
      WritesGlobal _ )
  | ( (CallsRecursive _ | PerformsAssertions | NonDeterministic),
      ThrowsException _ )
  | (PerformsAssertions | NonDeterministic), CallsRecursive _
  | NonDeterministic, PerformsAssertions ->
      -1

let pp_print f =
  let open Format in
  function
  | ReadsLocal { name = s } -> fprintf f "ReadsLocal %S" s
  | WritesLocal s -> fprintf f "WritesLocal %S" s
  | ReadsGlobal { name = s } -> fprintf f "ReadsGlobal %S" s
  | WritesGlobal s -> fprintf f "WritesGlobal %S" s
  | ThrowsException s -> fprintf f "ThrowsException %S" s
  | CallsRecursive s -> fprintf f "CallsRecursive %S" s
  | PerformsAssertions -> fprintf f "PerformsAssertions"
  | NonDeterministic -> fprintf f "NonDeterministic"

let time_frame = function
  | ReadsLocal { time_frame } | ReadsGlobal { time_frame } -> time_frame
  | WritesLocal _ | WritesGlobal _ | NonDeterministic | CallsRecursive _
  | ThrowsException _ ->
      TimeFrame.Execution
  | PerformsAssertions -> TimeFrame.Constant

let is_pure = function
  | ReadsLocal _ | ReadsGlobal _ | NonDeterministic | PerformsAssertions -> true
  | WritesLocal _ | WritesGlobal _ | CallsRecursive _ | ThrowsException _ ->
      false

let is_statically_evaluable = function
  | ReadsLocal { immutable } | ReadsGlobal { immutable } -> immutable
  | WritesLocal _ | WritesGlobal _ | NonDeterministic | CallsRecursive _
  | ThrowsException _ | PerformsAssertions ->
      false

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
    calls_recursives : ISet.t;
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
      calls_recursives = ISet.empty;
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
      && ISet.is_empty ses.calls_recursives
      && not ses.non_determinism
    then
      TimeFrame.max
        (fst ses.max_global_read_time_frame)
        (fst ses.max_local_read_time_frame)
    else TimeFrame.Execution

  let is_pure ses =
    ISet.is_empty ses.local_writes
    && ISet.is_empty ses.global_writes
    && ISet.is_empty ses.thrown_exceptions
    && ISet.is_empty ses.calls_recursives

  let all_reads_are_immutable ses =
    ISet.is_empty ses.local_reads && ISet.is_empty ses.global_reads

  let is_statically_evaluable ses =
    is_pure ses && (not ses.non_determinism)
    && (not ses.assertions_performed)
    && all_reads_are_immutable ses

  let is_deterministic ses = not ses.non_determinism

  let add_local_read s time_frame immutable ses =
    let local_reads =
      if immutable then ses.local_reads else ISet.add s ses.local_reads
    and max_local_read_time_frame =
      witnessed_time_frame_max (time_frame, s) ses.max_local_read_time_frame
    in
    { ses with local_reads; max_local_read_time_frame }

  let add_local_write s ses =
    { ses with local_writes = ISet.add s ses.local_writes }

  let add_global_read s time_frame immutable ses =
    let global_reads =
      if immutable then ses.global_reads else ISet.add s ses.global_reads
    and max_global_read_time_frame =
      witnessed_time_frame_max (time_frame, s) ses.max_global_read_time_frame
    in
    { ses with global_reads; max_global_read_time_frame }

  let add_global_write s ses =
    { ses with global_writes = ISet.add s ses.global_writes }

  let add_thrown_exception s ses =
    { ses with thrown_exceptions = ISet.add s ses.thrown_exceptions }

  let add_calls_recursive s ses =
    { ses with calls_recursives = ISet.add s ses.calls_recursives }

  let add_assertion ses = { ses with assertions_performed = true }
  let add_non_determinism ses = { ses with non_determinism = true }

  let add_side_effect se ses =
    match se with
    | ReadsLocal { name; time_frame; immutable } ->
        add_local_read name time_frame immutable ses
    | ReadsGlobal { name; time_frame; immutable } ->
        add_global_read name time_frame immutable ses
    | WritesLocal s -> add_local_write s ses
    | WritesGlobal s -> add_global_write s ses
    | ThrowsException s -> add_thrown_exception s ses
    | CallsRecursive s -> add_calls_recursive s ses
    | PerformsAssertions -> add_assertion ses
    | NonDeterministic -> add_non_determinism ses

  (* Constructors *)
  let reads_local s t immutable = add_local_read s t immutable empty
  let writes_local s = add_local_write s empty
  let reads_global s t immutable = add_global_read s t immutable empty
  let writes_global s = add_global_write s empty
  let throws_exception s = add_thrown_exception s empty
  let calls_recursive s = add_calls_recursive s empty
  let performs_assertions = add_assertion empty
  let non_deterministic = add_non_determinism empty

  let equal ses1 ses2 =
    ses1 == ses2
    || ISet.equal ses1.calls_recursives ses2.calls_recursives
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
        calls_recursives =
          ISet.union ses1.calls_recursives ses2.calls_recursives;
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
  let is_side_effect_free ses = is_pure ses && not ses.assertions_performed

  let is_side_effect_free_without_global_reads ses =
    is_side_effect_free ses && ISet.is_empty ses.global_reads

  let are_non_conflicting ses1 ses2 =
    if not (ISet.is_empty ses1.calls_recursives) then
      is_side_effect_free_without_global_reads ses2
    else if not (ISet.is_empty ses2.calls_recursives) then
      is_side_effect_free_without_global_reads ses1
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

  let choose_side_effect ses =
    if not (ISet.is_empty ses.global_writes) then
      WritesGlobal (ISet.choose ses.global_writes)
    else if not (ISet.is_empty ses.local_writes) then
      WritesLocal (ISet.choose ses.local_writes)
    else if not (ISet.is_empty ses.thrown_exceptions) then
      ThrowsException (ISet.choose ses.thrown_exceptions)
    else if not (ISet.is_empty ses.calls_recursives) then
      CallsRecursive (ISet.choose ses.calls_recursives)
    else if ses.assertions_performed then PerformsAssertions
    else raise Not_found

  let make_reads name =
    { name; time_frame = TimeFrame.Execution; immutable = false }

  let make_reads_local name = ReadsLocal (make_reads name)
  let make_reads_global name = ReadsGlobal (make_reads name)

  let choose_side_effect_with_reads ses =
    try choose_side_effect ses
    with Not_found ->
      if not (ISet.is_empty ses.global_reads) then
        let name = ISet.choose ses.global_reads in
        make_reads_global name
      else if not (ISet.is_empty ses.local_reads) then
        let name = ISet.choose ses.local_reads in
        make_reads_global name
      else raise Not_found

  let remove_pure ses =
    { ses with global_reads = ISet.empty; local_reads = ISet.empty }

  let choose_inter s1 s2 = ISet.inter s1 s2 |> ISet.choose

  let choose_conflicting_side_effects ses1 ses2 =
    if not (ISet.is_empty ses1.thrown_exceptions) then
      ( ThrowsException (ISet.choose ses1.thrown_exceptions),
        choose_side_effect ses2 )
    else if not (ISet.is_empty ses2.thrown_exceptions) then
      ( choose_side_effect ses1,
        ThrowsException (ISet.choose ses2.thrown_exceptions) )
    else if not (ISet.is_empty ses1.calls_recursives) then
      ( CallsRecursive (ISet.choose ses1.calls_recursives),
        choose_side_effect_with_reads ses2 )
    else if not (ISet.is_empty ses2.calls_recursives) then
      ( choose_side_effect_with_reads ses1,
        CallsRecursive (ISet.choose ses2.calls_recursives) )
    else if not (ISet.disjoint ses1.global_writes ses2.global_writes) then
      let s = choose_inter ses1.global_writes ses2.global_writes in
      (WritesGlobal s, WritesGlobal s)
    else if not (ISet.disjoint ses1.global_writes ses2.global_reads) then
      let name = choose_inter ses1.global_writes ses2.global_reads in
      (WritesGlobal name, make_reads_global name)
    else if not (ISet.disjoint ses1.global_reads ses2.global_writes) then
      let name = choose_inter ses1.global_reads ses2.global_writes in
      (make_reads_global name, WritesGlobal name)
    else if not (ISet.disjoint ses1.local_writes ses2.local_writes) then
      let s = choose_inter ses1.local_writes ses2.local_writes in
      (WritesLocal s, WritesLocal s)
    else if not (ISet.disjoint ses1.local_writes ses2.local_reads) then
      let name = choose_inter ses1.local_writes ses2.local_reads in
      (WritesLocal name, make_reads_local name)
    else if not (ISet.disjoint ses1.local_reads ses2.local_writes) then
      let s = choose_inter ses1.local_reads ses2.local_writes in
      (make_reads_local s, WritesLocal s)
    else (choose_side_effect ses1, choose_side_effect ses2)

  let non_conflicting_union ~fail ses1 ses2 =
    if ses1 == empty then ses2
    else if ses2 == empty then ses1
    else if are_non_conflicting ses1 ses2 then union ses1 ses2
    else choose_conflicting_side_effects ses1 ses2 |> fail

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
  let get_calls_recursives ses = ses.calls_recursives

  let remove_locals ses =
    {
      ses with
      local_reads = ISet.empty;
      local_writes = ISet.empty;
      max_local_read_time_frame = (TimeFrame.Constant, "1");
    }

  let remove_thrown_exceptions ses = { ses with thrown_exceptions = ISet.empty }
  let remove_calls_recursives ses = { ses with calls_recursives = ISet.empty }
  let remove_assertions ses = { ses with assertions_performed = false }
  let remove_non_determinism ses = { ses with non_determinism = false }

  let filter_thrown_exceptions f ses =
    { ses with thrown_exceptions = ISet.filter f ses.thrown_exceptions }

  let filter_calls_recursives f ses =
    { ses with calls_recursives = ISet.filter f ses.calls_recursives }

  module SESet = Set.Make (struct
    type t = side_effect

    let compare = compare
  end)

  let to_side_effect_list ses =
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
         (fun name time_frame ->
           ReadsGlobal { name; time_frame; immutable = true })
         ses.max_global_read_time_frame ses.global_reads
    |> add_from_tf
         (fun name time_frame ->
           ReadsLocal { name; time_frame; immutable = true })
         ses.max_local_read_time_frame ses.local_reads
    |> set_map_to_list (fun s -> CallsRecursive s) ses.calls_recursives
    |> set_map_to_list (fun s -> ThrowsException s) ses.thrown_exceptions
    |> set_map_to_list (fun s -> WritesGlobal s) ses.global_writes
    |> set_map_to_list make_reads_global ses.global_reads
    |> set_map_to_list (fun s -> WritesLocal s) ses.local_writes
    |> set_map_to_list make_reads_local ses.local_reads

  let to_side_effect_set ses = to_side_effect_list ses |> SESet.of_list

  let pp_print f ses =
    let elements = to_side_effect_list ses in
    let open Format in
    let pp_sep f () = fprintf f ",@ " in
    fprintf f "@[[%a]@]" (pp_print_list ~pp_sep pp_print) elements
end
