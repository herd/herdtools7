(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val numeric_labels : bool
  val timeloop : int
  val barrier : Barrier.t
end

module Make
    (O:Config)
    (T:Test.S with
     type P.code = string CAst.t and
     type A.reg = string and
     type A.loc_reg = string and
     type A.Out.t = CTarget.t) =
  struct

    module A = T.A
    module C = T.C
    module Generic = Compile.Generic(A)(C)

(******************************)
(* Compute observed locations *)
(******************************)

    type t =
      | Test of A.Out.t
      | Global of string

    let rec compat t1 t2 =
      let open CType in
      match t1,t2 with
      | (Base s1,Base s2)
      | (Array (s1,_),Base s2)
      | (Base s1,Array (s2,_))
        -> s1 = s2
      | Array (s1,sz1),Array (s2,sz2) ->
          s1 = s2 && sz1 = sz2
      | (Volatile t1),_ -> compat t1 t2
      | _,(Volatile t2) -> compat t1 t2
      | (Atomic t1,Atomic t2)
      | (Pointer t1,Pointer t2) -> compat t1 t2
      | _,_ -> false

    let add_atomic tenv tparam = match tparam with
    | CType.Atomic t ->
        if compat tenv t then Some tparam
        else None
    | _ -> None

    let add_param {CAst.param_ty; param_name} env =
      let ty = CType.strip_volatile param_ty in
      try
        let oty = StringMap.find param_name env in
(* add atomic qualifier, when appearing in parameters *)
        let oty = match add_atomic oty ty with
        | Some t -> t
        | None -> oty in
        if compat oty ty then StringMap.add param_name oty env
        else begin
          Warn.warn_always
            "Parameter %s, type mismatch %s vs. %s\n"
            param_name (CType.dump oty) (CType.dump ty) ;
          env
        end
      with Not_found ->
        StringMap.add param_name ty env

    let add_params = List.fold_right add_param

    let comp_globals env code =
      let env =
        A.LocMap.fold
          (fun loc t env -> match loc with
          | A.Location_global a -> StringMap.add a t env
          | A.Location_reg _ -> env)
          env StringMap.empty in
      let env =
        List.fold_right
          (function
            | CAst.Test {CAst.params; _} -> add_params params
            | _ -> Misc.identity
          )
          code
          env
      in
      StringMap.fold
        (fun a ty k -> (a,ty)::k)
        env []

     let string_of_params =
       let f {CAst.param_name; param_ty; } = param_name,param_ty in
       List.map f

    let comp_template final code =
      let inputs = string_of_params code.CAst.params in
      {
        CTarget.inputs ;
        finals=final ;
        code = code.CAst.body; }


    let comp_code obs env procs =
      List.fold_left
        (fun acc -> function
           | CAst.Test code ->
               let proc = code.CAst.proc in
               let regs =
                 A.LocSet.fold
                   (fun loc k -> match A.of_proc proc loc with
                   | Some r -> (r,Generic.find_type loc env)::k
                   | _ -> k)
                   obs [] in
               let final = List.map fst regs in
               let volatile = []
(*
                 let f acc = function
                   | {CAst.volatile = true; param_name; _} -> param_name :: acc
                   | {CAst.volatile = false; _} -> acc
                 in
                 List.fold_left f [] code.CAst.params *)
               in
               acc @ [(proc, (comp_template final code, (regs, volatile)))]
           | CAst.Global _ -> acc
        )
        [] procs

    let get_global_code =
      let f acc = function
        | CAst.Global x -> acc @ [x]
        | CAst.Test _ -> acc
      in
      List.fold_left f []

    let compile t =
      let
        { MiscParser.init = init ;
          info = info;
          prog = code;
          condition = final;
          locations = locs ; _
        } = t in
      let initenv = List.map (fun (x,(_,v)) -> x,v) init in
      let env = Generic.build_type_env init final locs in
      let observed = Generic.observed final locs in
      { T.init = initenv;
        info = info;
        code = comp_code observed env code;
        condition = final;
        globals = comp_globals env code;
        flocs = List.map fst locs;
        global_code = get_global_code code;
        src = t;
      }

  end
