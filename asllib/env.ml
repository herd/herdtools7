open AST
open ASTUtils

type func_sig = {
  declared_name : string;
  args : (identifier * ty) list;
  return_type : ty option;
}
(** Type signature for functions, some kind of an arrow type, with added
    informations. *)

let ast_func_to_func_sig : 'a AST.func_skeleton -> func_sig = function
  | { name = declared_name; args; return_type; parameters = _; body = _ } ->
      { declared_name; args; return_type }

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
    {
      env with
      local =
        {
          env.local with
          storage_types = IMap.add x (ty, ldk) env.local.storage_types;
        };
    }
end

module type RunTimeConf = sig
  type v
  type primitive

  val unroll : int
end

module RunTime (C : RunTimeConf) = struct
  module Types = struct
    (** Internal representation for subprograms. *)
    type func =
      | Func of int ref * AST.func
          (** A function has an index that keeps a unique calling index. *)
      | Primitive of C.primitive
          (** A primitive is just given by its type passed as argument. *)

    type global = {
      static : Static.global;
          (** Keeps a trace of the static env for reference. *)
      storage : C.v IMap.t;  (** Global declared storage elements. *)
      funcs : func IMap.t;
          (** Declared subprograms, maps called identifier to their code. *)
    }

    type int_stack = int list
    (** Stack of ints, for limiting loop unrolling *)

    type local = { storage : C.v IMap.t; scope : AST.scope; unroll : int_stack }
    type env = { global : global; local : local }
  end

  include Types

  let empty_local =
    { storage = IMap.empty; scope = Scope_Local ("", 0); unroll = [] }

  let empty_scoped scope = { empty_local with scope }

  let empty_global =
    { static = Static.empty_global; storage = IMap.empty; funcs = IMap.empty }

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
  (* Assignments utils *)

  let add_local x v env =
    {
      env with
      local = { env.local with storage = IMap.add x v env.local.storage };
    }

  let remove_local x env =
    {
      env with
      local = { env.local with storage = IMap.remove x env.local.storage };
    }

  let add_global x v env =
    let () = if false then Format.eprintf "Writing to global %S.@." x in
    {
      env with
      global = { env.global with storage = IMap.add x v env.global.storage };
    }
end
