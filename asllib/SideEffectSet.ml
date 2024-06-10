open ASTUtils
open SideEffect
module SE = SideEffect
module SESet = Set.Make (SE)

type t = SESet.t

module ByCategories = struct
  type t = {
    write_local : ISet.t;
    write_global : ISet.t;
    read_local : ISet.t;
    read_global : ISet.t;
    throwing : ISet.t;
    string_specified : ISet.t;
    recursive_call : ISet.t;
  }

  let empty =
    {
      write_local = ISet.empty;
      write_global = ISet.empty;
      read_local = ISet.empty;
      read_global = ISet.empty;
      throwing = ISet.empty;
      string_specified = ISet.empty;
      recursive_call = ISet.empty;
    }

  let of_seset ses =
    SESet.fold
      (fun se cats ->
        match se with
        | WriteGlobal s ->
            { cats with write_global = ISet.add s cats.write_global }
        | ReadGlobal s ->
            { cats with read_global = ISet.add s cats.read_global }
        | ReadLocal s -> { cats with read_local = ISet.add s cats.read_local }
        | WriteLocal s ->
            { cats with write_local = ISet.add s cats.write_local }
        | Throwing s -> { cats with throwing = ISet.add s cats.throwing }
        | RecursiveCall s ->
            { cats with recursive_call = ISet.add s cats.recursive_call }
        | StringSpecified s ->
            { cats with string_specified = ISet.add s cats.string_specified })
      ses empty

  let to_seset ses =
    SESet.union
      (SESet.union
         (SESet.union
            (ISet.to_seq ses.read_local
            |> Seq.map (fun s -> ReadLocal s)
            |> SESet.of_seq)
            (ISet.to_seq ses.write_local
            |> Seq.map (fun s -> ReadGlobal s)
            |> SESet.of_seq))
         (SESet.union
            (ISet.to_seq ses.read_global
            |> Seq.map (fun s -> ReadGlobal s)
            |> SESet.of_seq)
            (ISet.to_seq ses.write_global
            |> Seq.map (fun s -> WriteGlobal s)
            |> SESet.of_seq)))
      (SESet.union
         (ISet.to_seq ses.throwing
         |> Seq.map (fun s -> Throwing s)
         |> SESet.of_seq)
         (ISet.to_seq ses.string_specified
         |> Seq.map (fun s -> StringSpecified s)
         |> SESet.of_seq))

  let union bc1 bc2 =
    {
      write_local = ISet.union bc1.write_local bc2.write_local;
      write_global = ISet.union bc1.write_global bc2.write_global;
      read_local = ISet.union bc1.read_local bc2.read_local;
      read_global = ISet.union bc1.read_global bc2.read_global;
      throwing = ISet.union bc1.throwing bc2.throwing;
      string_specified = ISet.union bc1.string_specified bc2.string_specified;
      recursive_call = ISet.union bc1.recursive_call bc2.recursive_call;
    }

  let is_side_effect_free bc =
    ISet.is_empty bc.write_global
    && ISet.is_empty bc.write_local
    && ISet.is_empty bc.throwing
    && ISet.is_empty bc.string_specified
    && ISet.is_empty bc.recursive_call
end

let pp_print =
  let open Format in
  let pp_sep f () = fprintf f ",@ " in
  fun f ses ->
    fprintf f "{%a}"
      (pp_print_seq ~pp_sep SideEffect.pp_print)
      (SESet.to_seq ses)

let pure = SESet.empty
let write_global s = SESet.singleton (WriteGlobal s)
let write_local s = SESet.singleton (WriteLocal s)
let read_global s = SESet.singleton (ReadGlobal s)
let read_local s = SESet.singleton (ReadLocal s)

(* module _ : Set.S = SESet *)

let for_all' set f = SESet.for_all f set

let is_non_concurrent cats1 cats2 =
  let open ByCategories in
  if not (ISet.is_empty cats1.throwing) then is_side_effect_free cats2
  else if not (ISet.is_empty cats2.throwing) then is_side_effect_free cats1
  else
    ISet.disjoint cats1.write_global cats2.write_global
    && ISet.disjoint cats1.write_global cats2.read_global
    && ISet.disjoint cats1.read_global cats2.write_global
    && ISet.disjoint cats1.write_local cats2.write_local
    && ISet.disjoint cats1.write_local cats2.read_local
    && ISet.disjoint cats1.read_local cats2.write_local
    && ISet.disjoint cats2.string_specified cats2.string_specified

let get_side_effect bc =
  let open ByCategories in
  if not (ISet.is_empty bc.write_global) then
    ISet.choose bc.write_global |> SE.write_global
  else if not (ISet.is_empty bc.write_local) then
    ISet.choose bc.write_local |> SE.write_local
  else if not (ISet.is_empty bc.throwing) then
    ISet.choose bc.throwing |> SE.throwing
  else if not (ISet.is_empty bc.string_specified) then
    ISet.choose bc.string_specified |> SE.string_specified
  else if not (ISet.is_empty bc.recursive_call) then
    ISet.choose bc.recursive_call |> SE.recursive_call
  else raise Not_found

let get_concurrent_ses cats1 cats2 =
  let open ByCategories in
  let choose_inter s1 s2 = ISet.inter s1 s2 |> ISet.choose in
  if not (ISet.is_empty cats1.throwing) then
    (ISet.choose cats1.throwing |> SE.throwing, get_side_effect cats2)
  else if not (ISet.is_empty cats2.throwing) then
    (get_side_effect cats1, ISet.choose cats2.throwing |> SE.throwing)
  else if not (ISet.disjoint cats1.write_global cats2.write_global) then
    let s = choose_inter cats1.write_global cats2.write_global in
    (WriteGlobal s, WriteGlobal s)
  else if not (ISet.disjoint cats1.write_global cats2.read_global) then
    let s = choose_inter cats1.write_global cats2.read_global in
    (WriteGlobal s, ReadGlobal s)
  else if not (ISet.disjoint cats1.read_global cats2.write_global) then
    let s = choose_inter cats1.read_global cats2.write_global in
    (ReadGlobal s, WriteGlobal s)
  else if not (ISet.disjoint cats1.write_local cats2.write_local) then
    let s = choose_inter cats1.write_local cats2.write_local in
    (WriteLocal s, WriteLocal s)
  else if not (ISet.disjoint cats1.write_local cats2.read_local) then
    let s = choose_inter cats1.write_local cats2.read_local in
    (WriteLocal s, ReadLocal s)
  else if not (ISet.disjoint cats1.read_local cats2.write_local) then
    let s = choose_inter cats1.read_local cats2.write_local in
    (ReadLocal s, WriteLocal s)
  else if not (ISet.disjoint cats1.string_specified cats2.string_specified) then
    let s = choose_inter cats1.string_specified cats2.string_specified in
    (StringSpecified s, StringSpecified s)
  else raise Not_found

let non_concurrent_union ~loc s1 s2 =
  if SESet.is_empty s1 then s2
  else if SESet.is_empty s2 then s1
  else
    let cats1 = ByCategories.of_seset s1 and cats2 = ByCategories.of_seset s2 in
    if is_non_concurrent cats1 cats2 then SESet.union s1 s2
    else
      let s1, s2 = get_concurrent_ses cats1 cats2 in
      Error.(fatal_from loc (ConcurrentSideEffects (s1, s2)))

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

let big_non_concurrent_union ~loc =
  let union bc1 bc2 =
    if is_non_concurrent bc1 bc2 then ByCategories.union bc1 bc2
    else
      let s1, s2 = get_concurrent_ses bc1 bc2 in
      Error.(fatal_from loc (ConcurrentSideEffects (s1, s2)))
  in
  fun ses ->
    List.map ByCategories.of_seset ses
    |> iterated_union union ByCategories.empty
    |> ByCategories.to_seset

let is_side_effect_free ses =
  for_all' ses @@ function
  | ReadGlobal _ | ReadLocal _ -> true
  | WriteLocal _ | WriteGlobal _ | Throwing _ | StringSpecified _
  | RecursiveCall _ ->
      false

let union = SESet.union
let unions = iterated_union union pure

let remove_throwings =
  SESet.filter @@ function
  | StringSpecified _ | WriteLocal _ | WriteGlobal _ | ReadGlobal _
  | ReadLocal _ | RecursiveCall _ ->
      true
  | Throwing _ -> false

let of_string_list li = List.map SideEffect.string_specified li |> SESet.of_list

let get_call_recursive t =
  SESet.fold
    (fun se acc -> match se with RecursiveCall s -> s :: acc | _ -> acc)
    t []

let remove_local =
  SESet.filter @@ function
  | StringSpecified _ | WriteGlobal _ | ReadGlobal _ | Throwing _
  | RecursiveCall _ ->
      true
  | ReadLocal _ | WriteLocal _ -> false
