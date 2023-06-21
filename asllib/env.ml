open AST
open ASTUtils

type func_sig = unit AST.func
(** Type signature for functions, some kind of an arrow type, with added
    informations. *)

type 'a env_result = Local of 'a | Global of 'a | NotFound

let ast_func_to_func_sig : 'a AST.func -> func_sig = function
  | { name; args; return_type; parameters; subprogram_type; body = _ } ->
      {
        name;
        args;
        return_type;
        parameters;
        subprogram_type;
        body = SB_Primitive ();
      }

(** Static environments and utils. *)
module Static = struct
  module Types = struct
    type global = {
      declared_types : ty IMap.t;  (** Maps a type name t to its declaration. *)
      constants_values : value IMap.t;
          (** Maps a global constant name to its value. *)
      storage_types : (ty * global_decl_keyword) IMap.t;
          (** Maps global declared storage elements to their types. *)
      subtypes : identifier IMap.t;
          (** Maps an identifier s to its parent in the subtype relation. *)
      subprograms : func_sig IMap.t;
          (** Maps each subprogram runtime name to its signature. *)
      subprogram_renamings : ISet.t IMap.t;
          (** Maps each subprogram declared name to the equivalence class of all
          the subprogram runtime names that were declared with this name. *)
    }
    (** Store all the global environment information at compile-time. *)

    type local = {
      constants_values : value IMap.t;
          (** Maps a local constant to its value. *)
      storage_types : (ty * local_decl_keyword) IMap.t;
          (** Maps an locally declared names to their type. *)
    }
    (** Store all the local environment information at compile-time. *)

    type env = { global : global; local : local }
    (** The static environment type. *)
  end

  include Types

  module PPEnv = struct
    open Format

    let pp_map pp_elt f m =
      let pp_sep f () = fprintf f ",@ " in
      let pp_one f (key, elt) =
        fprintf f "@[<h 2>%s |-> @[%a@]@]" key pp_elt elt
      in
      fprintf f "@[<hv 2>{@ %a}@]"
        (PP.pp_print_seq ~pp_sep pp_one)
        (IMap.to_seq m)

    let pp_iset f s =
      let pp_sep f () = fprintf f ",@ " in
      fprintf f "@[<hv 2>{@ %a}@]"
        (PP.pp_print_seq ~pp_sep pp_print_string)
        (ISet.to_seq s)

    let pp_local f { constants_values; storage_types } =
      fprintf f "@[<v 2>Local with:@ - @[constants:@ %a@]@ - @[storage:@ %a@]@]"
        (pp_map PP.pp_value) constants_values
        (pp_map (fun f (t, _) -> PP.pp_ty f t))
        storage_types

    let pp_subprogram f func_sig =
      fprintf f "@[<hov 2>%a@ -> %a@]"
        (pp_print_list ~pp_sep:pp_print_space PP.pp_typed_identifier)
        func_sig.args (pp_print_option PP.pp_ty) func_sig.return_type

    let pp_global f
        {
          constants_values;
          storage_types;
          declared_types;
          subtypes;
          subprograms;
          subprogram_renamings;
        } =
      fprintf f
        "@[<v 2>Global with:@ - @[constants:@ %a@]@ - @[storage:@ %a@]@ - \
         @[types:@ %a@]@ - @[subtypes:@ %a@]@ - @[subprograms:@ %a@] - \
         @[subprogram_renamings:@ %a@]@]"
        (pp_map PP.pp_value) constants_values
        (pp_map (fun f (t, _) -> PP.pp_ty f t))
        storage_types (pp_map PP.pp_ty) declared_types (pp_map pp_print_string)
        subtypes (pp_map pp_subprogram) subprograms (pp_map pp_iset)
        subprogram_renamings

    let pp_env f { global; local } =
      fprintf f "@[<v 2>Env with:@ - %a@ - %a@]" pp_local local pp_global global
  end

  (** An empty global static environment. *)
  let empty_global =
    {
      declared_types = IMap.empty;
      constants_values = IMap.empty;
      storage_types = IMap.empty;
      subtypes = IMap.empty;
      subprograms = IMap.empty;
      subprogram_renamings = IMap.empty;
    }

  (** An empty local static env. *)
  let empty_local =
    { constants_values = IMap.empty; storage_types = IMap.empty }

  (** An empty static env. *)
  let empty = { local = empty_local; global = empty_global }

  (** [lookup x env] is the value of x as defined in environment.

      @raise Not_found if it is not defined inside. *)
  let lookup_constants env x =
    try IMap.find x env.local.constants_values
    with Not_found -> IMap.find x env.global.constants_values

  (** [type_of env "x"] is the type of ["x"] in the environment [env]. *)
  let type_of env x =
    try IMap.find x env.local.storage_types |> fst
    with Not_found -> IMap.find x env.global.storage_types |> fst

  let type_of_opt env x =
    try IMap.find x env.local.storage_types |> fst |> Option.some
    with Not_found ->
      IMap.find_opt x env.global.storage_types |> Option.map fst

  let mem_constants env x =
    IMap.mem x env.global.constants_values
    || IMap.mem x env.local.constants_values

  let add_subprogram name func_sig env =
    {
      env with
      global =
        {
          env.global with
          subprograms = IMap.add name func_sig env.global.subprograms;
        };
    }

  let set_renamings name set env =
    {
      env with
      global =
        {
          env.global with
          subprogram_renamings =
            IMap.add name set env.global.subprogram_renamings;
        };
    }

  let add_global_storage x ty gdk env =
    {
      env with
      global =
        {
          env.global with
          storage_types = IMap.add x (ty, gdk) env.global.storage_types;
        };
    }

  let add_type x ty env =
    {
      env with
      global =
        {
          env.global with
          declared_types = IMap.add x ty env.global.declared_types;
        };
    }

  let add_global_constant name v env =
    {
      env with
      global =
        {
          env.global with
          constants_values = IMap.add name v env.global.constants_values;
        };
    }

  let add_local x ty ldk env =
    let () =
      if false then Format.eprintf "Adding to env %S <- %a@." x PP.pp_ty ty
    in
    {
      env with
      local =
        {
          env.local with
          storage_types = IMap.add x (ty, ldk) env.local.storage_types;
        };
    }

  let add_subtype s t env =
    {
      env with
      global = { env.global with subtypes = IMap.add s t env.global.subtypes };
    }
end

module type RunTimeConf = sig
  type v
  type primitive

  val unroll : int
end

let _runtime_assertions = true

module Storage : sig
  type 'v t

  val alloc : unit -> int
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : identifier -> 'a t -> bool
  val add : identifier -> 'a -> 'a t -> 'a t
  val assign : identifier -> 'a -> 'a t -> 'a t
  val declare : identifier -> 'a -> 'a t -> 'a t
  val find : identifier -> 'a t -> 'a
  val find_opt : identifier -> 'a option t -> 'a option
  val remove : identifier -> 'a t -> 'a t
  val singleton : identifier -> 'a -> 'a t
  val patch_mem : t_env:'a t -> t_mem:'a t -> identifier list -> 'a t
end = struct
  type pointer = int

  module PMap = Map.Make (Int)
  module PSet = Set.Make (Int)

  type 'v t = { env : pointer IMap.t; mem : 'v PMap.t }

  let alloc =
    let next = ref 0 in
    fun () ->
      let r = !next in
      next := r + 1;
      r

  let empty = { env = IMap.empty; mem = PMap.empty }
  let is_empty t = IMap.is_empty t.env
  let mem x t = IMap.mem x t.env

  let assign x v t =
    let p = IMap.find x t.env in
    { t with mem = PMap.add p v t.mem }

  let declare x v t =
    let () = if _runtime_assertions then assert (not (mem x t)) in
    let p = alloc () in
    { env = IMap.add x p t.env; mem = PMap.add p v t.mem }

  let add x v t = try assign x v t with Not_found -> declare x v t

  let find x t =
    let p = IMap.find x t.env in
    PMap.find p t.mem

  let find_opt x t = try find x t with Not_found -> None

  let remove x t =
    try
      let p = IMap.find x t.env in
      { mem = PMap.remove p t.mem; env = IMap.remove x t.env }
    with Not_found -> t

  let singleton x v = add x v empty

  let patch_mem ~t_env ~t_mem to_avoid =
    let env = t_env.env
    and mem =
      try
        List.fold_left
          (fun mem x ->
            let p = IMap.find x t_mem.env in
            PMap.remove p mem)
          t_mem.mem to_avoid
      with Not_found -> assert false
    in
    { env; mem }
end

module RunTime (C : RunTimeConf) = struct
  module Types = struct
    (** Internal representation for subprograms. *)
    type func = int ref * C.primitive AST.func
    (** A function has an index that keeps a unique calling index. *)

    type global = {
      static : Static.global;
          (** Keeps a trace of the static env for reference. *)
      storage : C.v Storage.t;
      funcs : func IMap.t;
          (** Declared subprograms, maps called identifier to their code. *)
    }

    type int_stack = int list
    (** Stack of ints, for limiting loop unrolling *)

    type local = {
      storage : C.v Storage.t;
      scope : AST.scope;
      unroll : int_stack;
      declared : identifier list;
    }

    type env = { global : global; local : local }
  end

  include Types

  let empty_local =
    {
      storage = Storage.empty;
      scope = Scope_Local ("", 0);
      unroll = [];
      declared = [];
    }

  let empty_scoped scope = { empty_local with scope }

  let empty_global =
    {
      static = Static.empty_global;
      storage = Storage.empty;
      funcs = IMap.empty;
    }

  let empty = { global = empty_global; local = empty_local }

  (* --------------------------------------------------------------------------*)
  (* Loop unrolling controls. *)

  (** [tick_push env] is [env] with [C.unroll] pushed on its unrolling stack. *)
  let tick_push env =
    let unroll = C.unroll :: env.local.unroll in
    { env with local = { env.local with unroll } }

  (** [tick_push_bis env] is [env] with [C.unroll -1] pushed on its unrolling
      stack. *)
  let tick_push_bis env =
    let unroll = (C.unroll - 1) :: env.local.unroll in
    { env with local = { env.local with unroll } }

  (** [tick_pop env] is [env] with removed the unrolling stack first element. *)
  let tick_pop env =
    match env.local.unroll with
    | [] -> assert false
    | _ :: unroll -> { env with local = { env.local with unroll } }

  (** [tick_decr env] decrements the unrolling stack of env and returns
      wheather it has poped something or not. *)
  let tick_decr env =
    match env.local.unroll with
    | [] -> assert false
    | x :: xs ->
        let x = x - 1 in
        if x <= 0 then
          let unroll = xs in
          (true, { env with local = { env.local with unroll } })
        else
          let unroll = x :: xs in
          (false, { env with local = { env.local with unroll } })

  (* --------------------------------------------------------------------------*)
  (* Retrieval utils *)

  let find x env =
    try Local (Storage.find x env.local.storage)
    with Not_found -> (
      try Global (Storage.find x env.global.storage)
      with Not_found -> NotFound)

  let mem x env =
    Storage.mem x env.local.storage || Storage.mem x env.global.storage

  (* --------------------------------------------------------------------------*)
  (* Assignments utils *)

  let declare_local x v env =
    {
      env with
      local =
        {
          env.local with
          storage = Storage.add x v env.local.storage;
          declared = x :: env.local.declared;
        };
    }

  let assign_local x v env =
    {
      env with
      local = { env.local with storage = Storage.assign x v env.local.storage };
    }

  let declare_global x v env =
    {
      env with
      global = { env.global with storage = Storage.add x v env.global.storage };
    }

  let assign_global x v env =
    {
      env with
      global =
        { env.global with storage = Storage.assign x v env.global.storage };
    }

  let remove_local x env =
    {
      env with
      local = { env.local with storage = Storage.remove x env.local.storage };
    }

  let assign x v env =
    try Local (assign_local x v env)
    with Not_found -> (
      try Global (assign_global x v env) with Not_found -> NotFound)

  (* --------------------------------------------------------------------------*)
  (* Scope swapping utils *)

  let push_scope env = { env with local = { env.local with declared = [] } }

  let pop_scope parent child =
    let local_storage =
      Storage.patch_mem ~t_env:parent.local.storage ~t_mem:child.local.storage
        child.local.declared
    in
    { child with local = { parent.local with storage = local_storage } }
end
