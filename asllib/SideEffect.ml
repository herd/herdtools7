type identifier = AST.identifier

module ISet = ASTUtils.ISet

type t =
  | ReadLocal of identifier
  | WriteLocal of identifier
  | ReadGlobal of identifier
  | WriteGlobal of identifier
  | ThrowException of identifier
  | RecursiveCall of identifier
  | PerformsATC
  | NonDeterministic

type side_effect = t

let equal (t1 : t) (t2 : t) : bool =
  match (t1, t2) with
  | ReadLocal s1, ReadLocal s2
  | WriteLocal s1, WriteLocal s2
  | ReadGlobal s1, ReadGlobal s2
  | WriteGlobal s1, WriteGlobal s2
  | ThrowException s1, ThrowException s2
  | RecursiveCall s1, RecursiveCall s2 ->
      String.equal s1 s2
  | PerformsATC, PerformsATC | NonDeterministic, NonDeterministic -> true
  | ( ReadLocal _,
      ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | ThrowException _
      | RecursiveCall _ | PerformsATC | NonDeterministic ) )
  | ( WriteLocal _,
      ( ReadGlobal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
      | PerformsATC | NonDeterministic ) )
  | ( ReadGlobal _,
      ( WriteGlobal _ | ThrowException _ | RecursiveCall _ | PerformsATC
      | NonDeterministic ) )
  | ( WriteGlobal _,
      (ThrowException _ | RecursiveCall _ | PerformsATC | NonDeterministic) )
  | ThrowException _, (RecursiveCall _ | PerformsATC | NonDeterministic)
  | RecursiveCall _, (PerformsATC | NonDeterministic)
  | PerformsATC, NonDeterministic
  | ( ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | ThrowException _
      | RecursiveCall _ | PerformsATC | NonDeterministic ),
      ReadLocal _ )
  | ( ( ReadGlobal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
      | PerformsATC | NonDeterministic ),
      WriteLocal _ )
  | ( ( WriteGlobal _ | ThrowException _ | RecursiveCall _ | PerformsATC
      | NonDeterministic ),
      ReadGlobal _ )
  | ( (ThrowException _ | RecursiveCall _ | PerformsATC | NonDeterministic),
      WriteGlobal _ )
  | (RecursiveCall _ | PerformsATC | NonDeterministic), ThrowException _
  | (PerformsATC | NonDeterministic), RecursiveCall _
  | NonDeterministic, PerformsATC ->
      false

let compare (t1 : t) (t2 : t) : int =
  match (t1, t2) with
  | ReadLocal s1, ReadLocal s2
  | WriteLocal s1, WriteLocal s2
  | ReadGlobal s1, ReadGlobal s2
  | WriteGlobal s1, WriteGlobal s2
  | ThrowException s1, ThrowException s2
  | RecursiveCall s1, RecursiveCall s2 ->
      String.compare s1 s2
  | PerformsATC, PerformsATC | NonDeterministic, NonDeterministic -> 0
  | ( ReadLocal _,
      ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | ThrowException _
      | RecursiveCall _ | PerformsATC | NonDeterministic ) )
  | ( WriteLocal _,
      ( ReadGlobal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
      | PerformsATC | NonDeterministic ) )
  | ( ReadGlobal _,
      ( WriteGlobal _ | ThrowException _ | RecursiveCall _ | PerformsATC
      | NonDeterministic ) )
  | ( WriteGlobal _,
      (ThrowException _ | RecursiveCall _ | PerformsATC | NonDeterministic) )
  | ThrowException _, (RecursiveCall _ | PerformsATC | NonDeterministic)
  | RecursiveCall _, (PerformsATC | NonDeterministic)
  | PerformsATC, NonDeterministic ->
      1
  | ( ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | ThrowException _
      | RecursiveCall _ | PerformsATC | NonDeterministic ),
      ReadLocal _ )
  | ( ( ReadGlobal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
      | PerformsATC | NonDeterministic ),
      WriteLocal _ )
  | ( ( WriteGlobal _ | ThrowException _ | RecursiveCall _ | PerformsATC
      | NonDeterministic ),
      ReadGlobal _ )
  | ( (ThrowException _ | RecursiveCall _ | PerformsATC | NonDeterministic),
      WriteGlobal _ )
  | (RecursiveCall _ | PerformsATC | NonDeterministic), ThrowException _
  | (PerformsATC | NonDeterministic), RecursiveCall _
  | NonDeterministic, PerformsATC ->
      -1

let pp_print f =
  let open Format in
  function
  | ReadLocal s -> fprintf f "ReadLocal %S" s
  | WriteLocal s -> fprintf f "WriteLocal %S" s
  | ReadGlobal s -> fprintf f "ReadGlobal %S" s
  | WriteGlobal s -> fprintf f "WriteGlobal %S" s
  | ThrowException s -> fprintf f "RaiseException %S" s
  | RecursiveCall s -> fprintf f "RecursiveCall %S" s
  | PerformsATC -> fprintf f "PerformsATC"
  | NonDeterministic -> fprintf f "NonDeterministic"

let is_side_effect = function
  | ReadLocal _ | ReadGlobal _ | NonDeterministic -> false
  | WriteLocal _ | WriteGlobal _ | ThrowException _ | RecursiveCall _
  | PerformsATC ->
      true

(* SES = Side Effect Set *)
module SES = struct
  type t = {
    local_reads : ISet.t;
    local_writes : ISet.t;
    global_reads : ISet.t;
    global_writes : ISet.t;
    thrown_exceptions : ISet.t;
    recursive_calls : ISet.t;
    atcs_performed : bool;
    non_determinism : bool;
  }

  let empty =
    {
      local_reads = ISet.empty;
      local_writes = ISet.empty;
      global_reads = ISet.empty;
      global_writes = ISet.empty;
      thrown_exceptions = ISet.empty;
      recursive_calls = ISet.empty;
      atcs_performed = false;
      non_determinism = false;
    }

  let is_empty ses =
    ISet.is_empty ses.local_reads
    && ISet.is_empty ses.local_writes
    && ISet.is_empty ses.global_writes
    && ISet.is_empty ses.global_reads
    && ISet.is_empty ses.thrown_exceptions
    && ISet.is_empty ses.recursive_calls
    && (not ses.atcs_performed) && not ses.non_determinism

  let add_local_read s ses =
    { ses with local_reads = ISet.add s ses.local_reads }

  let add_local_write s ses =
    { ses with local_writes = ISet.add s ses.local_writes }

  let add_global_read s ses =
    { ses with global_reads = ISet.add s ses.global_reads }

  let add_global_write s ses =
    { ses with global_writes = ISet.add s ses.global_writes }

  let add_thrown_exception s ses =
    { ses with thrown_exceptions = ISet.add s ses.thrown_exceptions }

  let add_recursive_call s ses =
    { ses with recursive_calls = ISet.add s ses.thrown_exceptions }

  let set_atc_performed ses = { ses with atcs_performed = true }
  let set_non_determinism ses = { ses with non_determinism = true }

  let add_side_effect se ses =
    match se with
    | ReadLocal s -> add_local_read s ses
    | WriteLocal s -> add_local_write s ses
    | ReadGlobal s -> add_global_read s ses
    | WriteGlobal s -> add_global_write s ses
    | ThrowException s -> add_thrown_exception s ses
    | RecursiveCall s -> add_recursive_call s ses
    | PerformsATC -> set_atc_performed ses
    | NonDeterministic -> set_non_determinism ses

  (* Constructors *)
  let read_local s = add_local_read s empty
  let write_local s = add_local_write s empty
  let read_global s = add_global_read s empty
  let write_global s = add_global_write s empty
  let throw_exception s = add_thrown_exception s empty
  let recursive_call s = add_recursive_call s empty
  let performs_atc = set_atc_performed empty
  let non_deterministic = set_non_determinism empty

  let mem se ses =
    match se with
    | ReadGlobal s -> ISet.mem s ses.global_reads
    | WriteGlobal s -> ISet.mem s ses.global_writes
    | ReadLocal s -> ISet.mem s ses.local_reads
    | WriteLocal s -> ISet.mem s ses.local_writes
    | RecursiveCall s -> ISet.mem s ses.recursive_calls
    | ThrowException s -> ISet.mem s ses.thrown_exceptions
    | NonDeterministic -> ses.non_determinism
    | PerformsATC -> ses.atcs_performed

  let union ses1 ses2 =
    {
      local_reads = ISet.union ses1.local_reads ses2.local_reads;
      local_writes = ISet.union ses1.local_writes ses2.local_writes;
      global_reads = ISet.union ses1.global_reads ses2.global_reads;
      global_writes = ISet.union ses1.global_writes ses2.global_writes;
      thrown_exceptions =
        ISet.union ses1.thrown_exceptions ses2.thrown_exceptions;
      recursive_calls = ISet.union ses1.recursive_calls ses2.recursive_calls;
      atcs_performed = ses1.atcs_performed || ses2.atcs_performed;
      non_determinism = ses1.non_determinism || ses2.non_determinism;
    }

  (* Properties *)
  let is_side_effect_free ses =
    let is_empty = ISet.is_empty in
    is_empty ses.global_writes && is_empty ses.local_writes
    && is_empty ses.thrown_exceptions
    && is_empty ses.recursive_calls
    && not ses.atcs_performed

  let is_side_effect_with_global_reads ses =
    is_side_effect_free ses && ISet.is_empty ses.global_reads

  let is_non_concurrent ses1 ses2 =
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
    else if ses.atcs_performed then PerformsATC
    else raise Not_found

  let get_side_effect_with_global_reads ses =
    try get_side_effect ses
    with Not_found ->
      if not (ISet.is_empty ses.global_reads) then
        ReadGlobal (ISet.choose ses.global_reads)
      else raise Not_found

  let filter_side_effects ses =
    {
      ses with
      global_reads = ISet.empty;
      local_reads = ISet.empty;
      non_determinism = false;
    }

  let get_concurrent_side_effects ses1 ses2 =
    let open ISet in
    let choose_inter s1 s2 = ISet.inter s1 s2 |> ISet.choose in
    if not (ISet.is_empty ses1.thrown_exceptions) then
      (ThrowException (ISet.choose ses1.thrown_exceptions), get_side_effect ses2)
    else if not (ISet.is_empty ses2.thrown_exceptions) then
      (get_side_effect ses1, ThrowException (ISet.choose ses2.thrown_exceptions))
    else if not (ISet.is_empty ses1.recursive_calls) then
      ( RecursiveCall (ISet.choose ses1.recursive_calls),
        get_side_effect_with_global_reads ses2 )
    else if not (ISet.is_empty ses2.recursive_calls) then
      ( get_side_effect_with_global_reads ses1,
        RecursiveCall (ISet.choose ses2.recursive_calls) )
    else if not (ISet.disjoint ses1.global_writes ses2.global_writes) then
      let s = choose_inter ses1.global_writes ses2.global_writes in
      (WriteGlobal s, WriteGlobal s)
    else if not (ISet.disjoint ses1.global_writes ses2.global_reads) then
      let s = choose_inter ses1.global_writes ses2.global_reads in
      (WriteGlobal s, ReadGlobal s)
    else if not (ISet.disjoint ses1.global_reads ses2.global_writes) then
      let s = choose_inter ses1.global_reads ses2.global_writes in
      (ReadGlobal s, WriteGlobal s)
    else if not (ISet.disjoint ses1.local_writes ses2.local_writes) then
      let s = choose_inter ses1.local_writes ses2.local_writes in
      (WriteLocal s, WriteLocal s)
    else if not (ISet.disjoint ses1.local_writes ses2.local_reads) then
      let s = choose_inter ses1.local_writes ses2.local_reads in
      (WriteLocal s, ReadLocal s)
    else if not (ISet.disjoint ses1.local_reads ses2.local_writes) then
      let s = choose_inter ses1.local_reads ses2.local_writes in
      (ReadLocal s, WriteLocal s)
    else (get_side_effect ses1, get_side_effect ses2)

  let non_concurrent_union ~fail ses1 ses2 =
    if is_empty ses1 then ses2
    else if is_empty ses2 then ses1
    else if is_non_concurrent ses1 ses2 then union ses1 ses2
    else get_concurrent_side_effects ses1 ses2 |> fail

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

  let non_concurrent_unions ~fail =
    iterated_union (non_concurrent_union ~fail) empty

  let union3 ses1 ses2 ses3 = union ses1 (union ses2 ses3)

  let cardinal ses =
    ISet.cardinal ses.local_reads
    + ISet.cardinal ses.local_writes
    + ISet.cardinal ses.global_reads
    + ISet.cardinal ses.global_writes
    + ISet.cardinal ses.thrown_exceptions
    + ISet.cardinal ses.recursive_calls
    + (if ses.atcs_performed then 1 else 0)
    + if ses.non_determinism then 1 else 0

  let fold_recursive_calls f ses = ISet.fold f ses.recursive_calls

  let for_all_reads f ses =
    ISet.for_all f ses.global_reads && ISet.for_all f ses.local_reads

  let remove_locals ses =
    { ses with local_reads = ISet.empty; local_writes = ISet.empty }

  let remove_thrown_exceptions ses = { ses with thrown_exceptions = ISet.empty }
  let remove_recursive_calls ses = { ses with recursive_calls = ISet.empty }

  let filter_thrown_exceptions f ses =
    { ses with thrown_exceptions = ISet.filter f ses.thrown_exceptions }

  let filter_recursive_calls f ses =
    { ses with recursive_calls = ISet.filter f ses.recursive_calls }

  let iter f ses =
    ISet.iter (fun s -> f (ReadLocal s)) ses.local_reads;
    ISet.iter (fun s -> f (WriteLocal s)) ses.local_writes;
    ISet.iter (fun s -> f (ReadGlobal s)) ses.global_reads;
    ISet.iter (fun s -> f (WriteGlobal s)) ses.global_writes;
    ISet.iter (fun s -> f (ThrowException s)) ses.thrown_exceptions;
    ISet.iter (fun s -> f (RecursiveCall s)) ses.recursive_calls;
    if ses.non_determinism then f NonDeterministic;
    if ses.atcs_performed then f PerformsATC

  module SESet = Set.Make (struct
    type t = side_effect

    let compare = compare
  end)

  let to_side_effect_list ses =
    List.flatten
      [
        ISet.elements ses.local_reads |> List.map (fun s -> ReadLocal s);
        ISet.elements ses.local_writes |> List.map (fun s -> WriteLocal s);
        ISet.elements ses.global_reads |> List.map (fun s -> ReadGlobal s);
        ISet.elements ses.global_writes |> List.map (fun s -> WriteGlobal s);
        ISet.elements ses.thrown_exceptions
        |> List.map (fun s -> ThrowException s);
        ISet.elements ses.recursive_calls |> List.map (fun s -> RecursiveCall s);
        (if ses.non_determinism then [ NonDeterministic ] else []);
        (if ses.atcs_performed then [ PerformsATC ] else []);
      ]

  let to_side_effect_set ses = to_side_effect_list ses |> SESet.of_list

  let pp_print f ses =
    let elements = to_side_effect_list ses in
    let open Format in
    let pp_sep f () = fprintf f ",@ " in
    fprintf f "@[[%a]@]" (pp_print_list ~pp_sep pp_print) elements
end
