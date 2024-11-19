
module type Var = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t -> bool -> string
end

module type S = sig
  type t
  type var
  val of_bool : bool -> t
  val mk_atom : var -> t
  val mk_and : t -> t -> t
  val mk_or : t -> t -> t
  val mk_not : t -> t
  val pp : t -> string

  (* Return a minimal set of model that satisfy the formula *)
  val all_sat : t -> ((var * bool) list) list
end

module Make(V: Var) : S with type var = V.t = struct
  type var = V.t

  let gentag =
    let r = ref 0 in
    fun () -> incr r; !r

  (* Here the `tag` field is used for hashconsing to manipulate a compresed
   * version for formulas, `Leaf true` represent the totology, `Leaf` false
   * represent the trivially false formula, and `Ite(i,t,e)` represent the
   * formula `if i then t else e` with the invariant that each variables in `t`
   * must be greater than `i` with the ordering relation `V.compare`. This
   * ensure that each formula has a unique representation as a Bdd *)
  type node =
    | Leaf of bool
    | Ite of var * t * t
  and t = {
    tag: int;
    node: node;
  }

  (* Hashconsing data structure: ensure that a node is assigned to a unique tag *)
  module H = Map.Make(struct
    type t = node

    let compare n1 n2 =
      match n1,n2 with
      | Ite (i1,t1,e1), Ite (i2,t2,e2) -> begin
        match Misc.int_compare t1.tag t2.tag with
        | 0 -> begin match Misc.int_compare e1.tag e2.tag with
          | 0 -> V.compare i1 i2
          | r -> r
        end
        | r -> r
      end
      | Ite _, Leaf _ | Leaf true, Leaf false -> -1
      | Leaf _, Ite _ | Leaf false, Leaf true -> 1
      | Leaf _, Leaf _ -> 0
  end)

  let hashcons =
    (* Data-base of hashconsed binary decision diagrams *)
    let db = ref H.empty in
    fun node ->
      try H.find node !db
      with Not_found -> begin
        let res = {node; tag=gentag ()} in
        db := H.add node res !db;
        res
      end

  let make (v: var) (t: t) (e: t) : t =
    if Misc.int_eq t.tag e.tag
    then e else hashcons (Ite (v,t,e))

  let of_bool b = hashcons (Leaf b)
  let zero = of_bool false
  let one = of_bool true

  let mk_atom v = make v one zero

  let rec pp term =
    match term.node with
    | Leaf true -> "true"
    | Leaf false -> "false"
    | Ite (i,t,e) ->
        Printf.sprintf "(%s <- %s -> %s)" (pp e) (V.pp i true) (pp t)

  module BddMap = Map.Make(struct
    type nonrec t = t
    let compare x y = Misc.int_compare x.tag y.tag
  end)

  let mk_not x =
    let cache = ref BddMap.empty in
    let rec do_rec term =
      try
        BddMap.find term !cache
      with Not_found ->
        let res = match term.node with
          | Leaf b -> of_bool (not b)
          | Ite (i,t,e) -> make i (do_rec t) (do_rec e)
        in
        cache := BddMap.add term res !cache;
        res
    in do_rec x

  module PairMap = Map.Make(struct
    type nonrec t = t * t
    let compare (x1,y1) (x2,y2) =
      match Misc.int_compare x1.tag x2.tag with
      | 0 -> Misc.int_compare y1.tag y2.tag
      | r -> r
  end)

  let mk_binop (binop : bool -> bool -> bool) x y =
    let cache = ref PairMap.empty in
    let rec do_rec x y =
      try
        PairMap.find (x,y) !cache
      with Not_found ->
        let res = match x.node,y.node with
          | Leaf b1, Leaf b2 ->
              of_bool (binop b1 b2)
          | Ite (i1,t1,e1), Ite (i2,t2,e2) -> begin
            match V.compare i1 i2 with
            | 0 ->
              make i1 (do_rec t1 t2) (do_rec e1 e2)
            | r when r < 0 ->
              make i1 (do_rec t1 y) (do_rec e1 y)
            | _ ->
              make i2 (do_rec x t2) (do_rec x e2)
          end
          | Ite (i1,t1,e1), _ ->
              make i1 (do_rec t1 y) (do_rec e1 y)
          | _, Ite (i2,t2,e2) ->
              make i2 (do_rec x t2) (do_rec x e2)
        in
        cache := PairMap.add (x,y) res !cache;
        res
    in do_rec x y

  let mk_and = mk_binop (&&)
  let mk_or = mk_binop (||)

  module VSet = MySet.Make(V)
  (* All to test if a model is a subset of another *)
  module Model = struct
    include Map.Make(V)
    type nonrec t = bool t

    let compare : t -> t -> int =
      compare (fun a b -> match a,b with
        | true,true | false,false -> 0
        | true,false -> -1
        | false,true -> 1
      )

    let equal : t -> t -> bool =
      equal Misc.bool_eq

    let subset (x : t) (y : t) : bool =
      fold (fun v b acc -> match find_opt v y with
        | Some b' -> Misc.bool_eq b b' && acc
        | None -> false
      ) x true
  end

  module ModelSet = MySet.Make(Model)

  let is_strict_subset (m1: Model.t) (m2: Model.t) : bool =
    Model.subset m1 m2 && not (Model.equal m1 m2)

  (* Remove all the models `x` in `dnf` such that `m` is a subset of `x` *)
  let remove_surset (m: Model.t) (dnf: ModelSet.t) =
    ModelSet.filter (fun x -> not (is_strict_subset m x)) dnf

  (* Evaluate a conjunction in a given context:
   * - Return `Some true` if the model implies the formula
   * - `Some false` if the model implies the negation of the formula
   * - `None` overwise
   *)
  let rec eval (model: Model.t) (term : t) : bool option =
    match term.node with
    | Leaf b -> Some b
    | Ite (i,t,e) -> begin
      match Model.find_opt i model with
      | Some b ->
          if b
          then eval model t
          else eval model e
      | None -> begin
        match eval model t, eval model e with
        | Some false, Some false -> Some false
        | Some true, Some true -> Some true
        | _, _ -> None
      end
    end

  (* Return model `m` such that forall `a` and `b` conjunctions such
   * that `m := a /\ b`, `a` is not a model of the input formula.
   * The resulting model must be a subset of the input *)
  let remove_vars (term: t) (model: Model.t) : Model.t =
    Model.fold (fun var _ model ->
      (* Remove `var` if it is useless *)
      match eval (Model.remove var model) term with
      | Some true -> Model.remove var model
      | _ -> model
    ) model model

  let rec pp_model (model : (var*bool) list) : string =
    match model with
    | [x,b] -> V.pp x b
    | (x,b) :: (_ :: _ as xs) ->
        Printf.sprintf "%s; %s" (V.pp x b) (pp_model xs)
    | [] -> ""

  let rec pp_model_set (model_set : (var*bool) list list) : string =
    match model_set with
    | [model] ->
        pp_model model
    | m :: (_ :: _ as ms) ->
        Printf.sprintf "%s\n\t%s" (pp_model m) (pp_model_set ms)
    | [] -> ""


  (* This function return a "locally optimal" set of models:
   * - Their is no `x` and `y` in the output such that `x` is a subset of `y`
   * - Their is no `x` in the output such that `x := a /\ b` and `a` is also a
   *   model of the input formula
   *)
  let all_sat term =
    let rec do_rec term =
      match term.node with
      | Leaf false -> ModelSet.empty
      | Leaf true -> ModelSet.singleton Model.empty
      | Ite (i,t,e) ->
          let t = do_rec t
          and e = do_rec e in
          ModelSet.union3
            (ModelSet.map (Model.add i true) (ModelSet.diff t e))
            (ModelSet.map (Model.add i false) (ModelSet.diff e t))
            (ModelSet.inter e t) in
    let models = do_rec term in
    let models = ModelSet.map (remove_vars term) models in
    let models = ModelSet.fold remove_surset models models in
    List.map Model.to_list (ModelSet.to_list models)
end
